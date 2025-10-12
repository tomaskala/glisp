package compiler

import (
	"fmt"
	"slices"
	"strconv"

	"tomaskala.com/glisp/internal/runtime"
	"tomaskala.com/glisp/internal/tokenizer"
)

type ParseError struct {
	Line    int
	Message string
}

func (e *ParseError) Error() string {
	return fmt.Sprintf("parse error at %d: %s", e.Line, e.Message)
}

type Parser struct {
	name       string               // Name of the source being tokenized, used for error reporting.
	defineName string               // Stored from the latest encountered "define", spliced into lambdas.
	previous   tokenizer.Token      // The last read token.
	current    tokenizer.Token      // The next token.
	tokenizer  *tokenizer.Tokenizer // The underlying tokenizer.
}

func (p *Parser) advance() {
	p.previous = p.current
	p.current = p.tokenizer.NextToken()
	if p.current.Type == tokenizer.TokenErr {
		panic(p.errorf("%s", p.current.Val))
	}
}

func (p *Parser) errorf(format string, args ...any) error {
	message := fmt.Sprintf(format, args...)
	return &ParseError{p.current.Line, message}
}

func (p *Parser) match(expected tokenizer.TokenType) bool {
	if p.current.Type != expected {
		return false
	}
	p.advance()
	return true
}

func (p *Parser) matchVal(expected string) bool {
	if p.current.Val != expected {
		return false
	}
	p.advance()
	return true
}

func (p *Parser) consume(expected tokenizer.TokenType) {
	if !p.match(expected) {
		panic(p.errorf("unexpected token: expected %v, got %v", expected, p.current.Type))
	}
}

type CompileError struct {
	Line    int
	Message string
}

func (e *CompileError) Error() string {
	return fmt.Sprintf("compile error at %d: %s", e.Line, e.Message)
}

type Local string

type Compiler struct {
	parent   *Compiler         // The enclosing compiler, if any.
	parser   *Parser           // The underlying parser.
	function *runtime.Function // The function currently being compiled.
	locals   []Local           // Local variables of the function.
}

func newCompiler(parent *Compiler, parser *Parser, name string) *Compiler {
	return &Compiler{
		parent:   parent,
		parser:   parser,
		function: &runtime.Function{Name: runtime.NewAtom(name)},
	}
}

func (c *Compiler) errorf(format string, args ...any) error {
	message := fmt.Sprintf(format, args...)
	return &CompileError{c.parser.current.Line, message}
}

func (c *Compiler) emit(code runtime.OpCode) {
	c.function.Chunk.Write(code, c.parser.previous.Line)
}

func (c *Compiler) emitArg(code runtime.OpCode, arg int) {
	c.emit(code)
	c.emit(c.opcodeInt(arg))
}

func (c *Compiler) emitJump(code runtime.OpCode) int {
	c.emitArg(code, runtime.OpCodeMax)
	return len(c.function.Chunk.Code) - 1
}

func (c *Compiler) patchJump(offset int) {
	jump := len(c.function.Chunk.Code) - 1 - offset
	c.function.Chunk.Code[offset] = c.opcodeInt(jump)
}

func (c *Compiler) end() *runtime.Function {
	c.emit(runtime.OpReturn)
	return c.function
}

func (c *Compiler) opcodeInt(n int) runtime.OpCode {
	if n < runtime.OpCodeMin || runtime.OpCodeMax < n {
		panic(c.errorf("program too large: constant/local index or a jump offset %d is out of bounds", n))
	}
	return runtime.OpCode(n)
}

func (c *Compiler) addConstant(constant runtime.Value) int {
	// Constant deduplication.
	for i, c := range c.function.Chunk.Constants {
		if c.Equal(constant) {
			return i
		}
	}
	return c.function.Chunk.AddConstant(constant)
}

func (c *Compiler) addLocal(name string) {
	c.locals = append(c.locals, Local(name))
}

func (c *Compiler) addUpvalue(idx int, isLocal bool) int {
	for i, upvalue := range c.function.Upvalues {
		if upvalue.Index == idx && upvalue.IsLocal == isLocal {
			return i
		}
	}
	c.function.Upvalues = append(c.function.Upvalues, runtime.UpvalueSpec{Index: idx, IsLocal: isLocal})
	return len(c.function.Upvalues) - 1
}

func (c *Compiler) resolveLocal(name string) (int, bool) {
	for i, local := range slices.Backward(c.locals) {
		if string(local) == name {
			return i, true
		}
	}
	return -1, false
}

func (c *Compiler) resolveUpvalue(name string) (int, bool) {
	if c.parent == nil {
		return -1, false
	}
	if idx, ok := c.parent.resolveLocal(name); ok {
		return c.addUpvalue(idx, true), true
	}
	if idx, ok := c.parent.resolveUpvalue(name); ok {
		return c.addUpvalue(idx, false), true
	}
	return -1, false
}

func Compile(name, source string) (prog *runtime.Program, err error) {
	defer func() {
		if r := recover(); r != nil {
			if parseError, ok := r.(*ParseError); ok {
				err = parseError
			} else if compileError, ok := r.(*CompileError); ok {
				err = compileError
			}
		}
	}()
	tokenizer := tokenizer.NewTokenizer(source)
	parser := &Parser{name: name, tokenizer: tokenizer}
	parser.advance() // Initialize the first token.

	compiler := newCompiler(nil, parser, name)
	compiler.compileProgram()
	topLevel := compiler.end()
	return &runtime.Program{Function: topLevel}, err
}

// Program:
//
// expr*.
func (c *Compiler) compileProgram() {
	for !c.parser.match(tokenizer.TokenEOF) {
		c.compileExpr(false)
		c.parser.advance()
	}
}

// Expr:
//
// call | quote | number | atom.
func (c *Compiler) compileExpr(tailPosition bool) {
	switch {
	case c.parser.match(tokenizer.TokenAtom):
		c.compileAtom(c.parser.previous.Val)
	case c.parser.match(tokenizer.TokenNumber):
		n := c.parser.parseNumber()
		val := runtime.MakeNumber(n)
		idx := c.addConstant(val)
		c.emitArg(runtime.OpConstant, idx)
	case c.parser.match(tokenizer.TokenQuote):
		val := c.compileQuoted()
		if val.IsNil() {
			c.emit(runtime.OpNil)
		} else {
			idx := c.addConstant(val)
			c.emitArg(runtime.OpConstant, idx)
		}
	case c.parser.match(tokenizer.TokenLeftParen):
		c.compileList(tailPosition)
	default:
		panic(c.parser.errorf("unexpected token: expected expression, got %v", c.parser.previous.Type))
	}
}

func (c *Compiler) compileAtom(name string) {
	var idx int
	var op runtime.OpCode
	if i, ok := c.resolveLocal(name); ok {
		idx = i
		op = runtime.OpGetLocal
	} else if i, ok := c.resolveUpvalue(name); ok {
		idx = i
		op = runtime.OpGetUpvalue
	} else {
		idx = c.addConstant(runtime.MakeAtom(name))
		op = runtime.OpGetGlobal
	}

	c.emitArg(op, idx)
}

// Call:
//
// "(" ")" | "(" special-form ")" | "(" expr list ")"
//
// "(" is past
//
// Special forms are built in to the language and their parameters differ.
func (c *Compiler) compileList(tailPosition bool) {
	switch {
	case c.parser.matchVal(")"):
		c.emit(runtime.OpNil)
	case c.parser.matchVal("quote"):
		c.compileLongQuote()
	case c.parser.matchVal("define"):
		c.compileDefine()
	case c.parser.matchVal("let"):
		c.compileLet()
	case c.parser.matchVal("let*"):
		c.compileLet()
	case c.parser.matchVal("letrec"):
		c.compileLet()
	case c.parser.matchVal("lambda"):
		c.compileLambda()
	case c.parser.matchVal("if"):
		c.compileIf(tailPosition)
	case c.parser.matchVal("cond"):
		c.compileCond(tailPosition)
	case c.parser.matchVal("and"):
		c.compileAnd()
	case c.parser.matchVal("or"):
		c.compileOr()
	case c.parser.matchVal("set!"):
		c.compileSet()
	case c.parser.matchVal("begin"):
		c.compileBegin(tailPosition)
	default:
		c.compileExpr(false) // Callee
		argCount := 0
		for !c.parser.match(tokenizer.TokenRightParen) {
			c.compileExpr(false) // Argument
			argCount++
		}

		if tailPosition {
			c.emitArg(runtime.OpTailCall, argCount)
		} else {
			c.emitArg(runtime.OpCall, argCount)
		}
	}
}

// Quote:
//
// "(" "quote" expr ")"
//
// "(" is past
// "quote" is past
//
// Compiles the long form (quote expr), as opposed to the shorthand 'expr.
func (c *Compiler) compileLongQuote() {
	val := c.compileQuoted()
	if val.IsNil() {
		c.emit(runtime.OpNil)
	} else {
		idx := c.addConstant(val)
		c.emitArg(runtime.OpConstant, idx)
	}
	c.parser.consume(tokenizer.TokenRightParen)
}

func (c *Compiler) compileQuoted() runtime.Value {
	switch {
	case c.parser.match(tokenizer.TokenAtom):
		return runtime.MakeAtom(c.parser.previous.Val)
	case c.parser.match(tokenizer.TokenNumber):
		n := c.parser.parseNumber()
		return runtime.MakeNumber(n)
	case c.parser.match(tokenizer.TokenQuote):
		c.parser.advance()
		return c.compileQuoted()
	case c.parser.match(tokenizer.TokenLeftParen):
		if c.parser.match(tokenizer.TokenRightParen) {
			return runtime.MakeNil()
		}

		list := runtime.MakePair(&runtime.Pair{})
		var curr *runtime.Value = &list
		for !c.parser.match(tokenizer.TokenRightParen) {
			if c.parser.match(tokenizer.TokenDot) {
				elem := c.compileQuoted()
				*curr = elem
				c.parser.consume(tokenizer.TokenRightParen)
				break
			}

			elem := c.compileQuoted()
			next := &runtime.Pair{Car: elem, Cdr: runtime.Nil}
			*curr = runtime.MakePair(next)
			curr = &next.Cdr
		}
		return list
	default:
		panic(c.parser.errorf("unexpected token: expected expression got: %v", c.parser.previous.Type))
	}
}

// Define:
//
// "(" "define" atom expr ")"
//
// "(" is past
// "define" is past.
func (c *Compiler) compileDefine() {
	c.parser.consume(tokenizer.TokenAtom)
	name := c.parser.previous.Val

	c.compileExpr(false) // Value
	c.parser.consume(tokenizer.TokenRightParen)

	idx := c.addConstant(runtime.MakeAtom(name))
	c.emitArg(runtime.OpDefineGlobal, idx)
	c.emitArg(runtime.OpConstant, idx)
}

// Let:
//
// "(" "let" "(" ("(" atom expr ")")* ")" expr ")"
// "(" "let*" "(" ("(" atom expr ")")* ")" expr ")"
// "(" "letrec" "(" ("(" atom expr ")")* ")" expr ")"
//
// "(" is past
// "let"/"let*"/"letrec" is past.
func (c *Compiler) compileLet() {
	panic(c.errorf("let expressions are not yet implemented"))
	/*
		var bindings []ast.Binding
		c.parser.consume(tokenizer.TokenLeftParen)
		for !c.parser.match(tokenizer.TokenRightParen) {
			c.parser.consume(tokenizer.TokenLeftParen)
			c.parser.consume(tokenizer.TokenAtom)
			name := c.parser.previous.Val
			c.parser.next()
			value := c.compileExpr()
			c.parser.consume(tokenizer.TokenRightParen)
			c.parser.next()
			bindings = append(bindings, ast.Binding{Name: name, Value: value})
		}
		c.parser.next()
		body := c.compileExpr()
		c.parser.consume(tokenizer.TokenRightParen)
		return &ast.Let{Kind: kind, Bindings: bindings, Body: body}
	*/
}

// Lambda:
//
// "(" "lambda" (atom | "(" expr* ("." expr)? ")") expr ")"
//
// "(" is past
// "lambda" is past.
func (c *Compiler) compileLambda() {
	compiler := newCompiler(c, c.parser, c.parser.defineName)
	switch {
	case c.parser.match(tokenizer.TokenLeftParen):
		compiler.compileStringList()
	case c.parser.match(tokenizer.TokenAtom):
		compiler.addLocal(compiler.parser.previous.Val)
		compiler.function.HasRestParam = true
	default:
		panic(compiler.parser.errorf("unexpected lambda parameter: %v", compiler.parser.previous.Type))
	}

	compiler.compileExpr(true) // Body
	compiler.parser.consume(tokenizer.TokenRightParen)
	function := compiler.end()
	idx := c.addConstant(runtime.MakeFunction(function))
	c.emitArg(runtime.OpClosure, idx)
}

// If:
//
// "(" "if" expr expr expr ")"
//
// "(" is past
// "if" is past.
func (c *Compiler) compileIf(tailPosition bool) {
	c.compileExpr(false) // Condition
	thenJump := c.emitJump(runtime.OpJumpIfFalse)
	c.emit(runtime.OpPop)

	c.compileExpr(tailPosition) // Then branch
	elseJump := c.emitJump(runtime.OpJump)
	c.patchJump(thenJump)
	c.emit(runtime.OpPop)

	c.compileExpr(tailPosition) // Else branch
	c.parser.consume(tokenizer.TokenRightParen)
	c.patchJump(elseJump)
}

// Cond:
//
// "(" "cond" ( "(" expr expr ")" )* ")"
//
// "(" is past
// "cond" is past.
func (c *Compiler) compileCond(tailPosition bool) {
	if c.parser.match(tokenizer.TokenRightParen) {
		c.emit(runtime.OpNil)
		return
	}

	var endJumps []int
	for !c.parser.match(tokenizer.TokenRightParen) {
		c.parser.consume(tokenizer.TokenLeftParen)
		c.compileExpr(false) // Condition
		nextJump := c.emitJump(runtime.OpJumpIfFalse)
		c.emit(runtime.OpPop)

		c.compileExpr(tailPosition) // Value
		c.parser.consume(tokenizer.TokenRightParen)
		endJump := c.emitJump(runtime.OpJump)
		endJumps = append(endJumps, endJump)
		c.patchJump(nextJump)
		c.emit(runtime.OpPop)
	}

	c.emit(runtime.OpNil)
	for _, jump := range endJumps {
		c.patchJump(jump)
	}
}

// And:
//
// "(" "and" expr* ")"
//
// "(" is past
// "and" is past.
func (c *Compiler) compileAnd() {
	if c.parser.match(tokenizer.TokenRightParen) {
		idx := c.addConstant(runtime.True)
		c.emitArg(runtime.OpConstant, idx)
		return
	}

	var endJumps []int
	for {
		c.compileExpr(false)
		if c.parser.match(tokenizer.TokenRightParen) {
			break
		}
		endJump := c.emitJump(runtime.OpJumpIfFalse)
		endJumps = append(endJumps, endJump)
		c.emit(runtime.OpPop)
	}

	for _, jump := range endJumps {
		c.patchJump(jump)
	}
}

// Or:
//
// "(" "or" expr* ")"
//
// "(" is past
// "or" is past.
func (c *Compiler) compileOr() {
	if c.parser.match(tokenizer.TokenRightParen) {
		c.emit(runtime.OpNil)
		return
	}

	var endJumps []int
	for {
		c.compileExpr(false)
		if c.parser.match(tokenizer.TokenRightParen) {
			break
		}
		endJump := c.emitJump(runtime.OpJumpIfTrue)
		endJumps = append(endJumps, endJump)
		c.emit(runtime.OpPop)
	}

	for _, jump := range endJumps {
		c.patchJump(jump)
	}
}

// Set:
//
// "(" "set!" atom expr ")"
//
// "(" is past
// "set!" is past.
func (c *Compiler) compileSet() {
	c.parser.consume(tokenizer.TokenAtom)
	id := c.parser.previous.Val

	var idx int
	var op runtime.OpCode
	if i, ok := c.resolveLocal(id); ok {
		idx = i
		op = runtime.OpSetLocal
	} else if i, ok := c.resolveUpvalue(id); ok {
		idx = i
		op = runtime.OpSetUpvalue
	} else {
		idx = c.addConstant(runtime.MakeAtom(id))
		op = runtime.OpSetGlobal
	}

	c.compileExpr(false)
	c.parser.consume(tokenizer.TokenRightParen)
	c.emitArg(op, idx)
}

// Begin:
//
// "(" "begin" expr* expr ")"
//
// "(" is past
// "begin" is past.
func (c *Compiler) compileBegin(tailPosition bool) {
	if c.parser.match(tokenizer.TokenRightParen) {
		c.emit(runtime.OpNil)
		return
	}

	for !c.parser.match(tokenizer.TokenRightParen) {
		// TODO: Fix the tail position calculation - we need to run
		// c.compileExpr(tailPosition && next is TokenRightParen), but
		// at this point, next is ALWAYS the last expression to be compiled
		// c.compileExpr advances so that when it ends the compilation of the
		// last expression, c.parser.current is TokenRightParen, the for loop
		// check succeeds and the loop terminates.
		c.compileExpr(tailPosition)
	}
}

// String list:
//
// "(" atom* ("." atom)? ")"
//
// "(" is past
//
// This is not a part of the Lisp grammar, but the function is used
// as a utility to parse a lambda function parameters.
func (c *Compiler) compileStringList() {
	for !c.parser.match(tokenizer.TokenRightParen) {
		if c.parser.match(tokenizer.TokenDot) {
			c.parser.consume(tokenizer.TokenAtom)
			c.addLocal(c.parser.previous.Val)
			c.function.HasRestParam = true
			c.parser.consume(tokenizer.TokenRightParen)
			break
		}

		c.parser.consume(tokenizer.TokenAtom)
		c.addLocal(c.parser.previous.Val)
		c.function.Arity++
	}
}

// Number:
//
// number literal.
func (p *Parser) parseNumber() float64 {
	if num, err := strconv.ParseInt(p.previous.Val, 0, 0); err == nil {
		return float64(num)
	}

	if num, err := strconv.ParseFloat(p.previous.Val, 64); err == nil {
		return num
	}

	panic(p.errorf("illegal number syntax"))
}
