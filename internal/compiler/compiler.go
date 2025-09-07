package compiler

import (
	"fmt"

	"tomaskala.com/glisp/internal/parser"
	"tomaskala.com/glisp/internal/tokenizer"
)

type CompileError struct {
	Message string
}

func (e *CompileError) Error() string {
	return e.Message
}

type Program struct {
	Function *Function
}

type Local struct {
	Name       string
	Depth      int
	IsCaptured bool
}

type UpvalueSpec struct {
	Index   int  // Captured value index.
	IsLocal bool // true: capture parent's local at Index; false: capture parent's upvalue at Index.
}

type Compiler struct {
	parent     *Compiler // The enclosing compiler, if any.
	function   *Function // The function currently being compiled.
	locals     []Local   // Local variables of the function.
	scopeDepth int       // The nesting level of the function's local variables, 0 being the global scope.
}

func Compile(name string, program *parser.Program) (prog *Program, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(*CompileError)
		}
	}()
	compiler := newCompiler(nil, name, 0)
	prog = compiler.compileProgram(program)
	return prog, err
}

func newCompiler(parent *Compiler, name string, funcArity int) *Compiler {
	return &Compiler{
		parent:   parent,
		function: &Function{Name: name, Arity: funcArity},
	}
}

func (c *Compiler) emit1(code OpCode, token tokenizer.Token) {
	c.function.Chunk.write(code, token.Pos)
}

func (c *Compiler) emit2(code1, code2 OpCode, token tokenizer.Token) {
	c.function.Chunk.write(code1, token.Pos)
	c.function.Chunk.write(code2, token.Pos)
}

func (c *Compiler) end(expr parser.Expr) *Function {
	c.emit1(OpReturn, expr.Token())
	return c.function
}

func (c *Compiler) beginScope() {
	c.scopeDepth++
}

func (c *Compiler) endScope(scopedExpr parser.Expr) {
	c.scopeDepth--
	for len(c.locals) > 0 && c.locals[len(c.locals)-1].Depth > c.scopeDepth {
		local := c.locals[len(c.locals)-1]
		if local.IsCaptured {
			c.emit1(OpCloseUpvalue, scopedExpr.Token())
		}
		c.locals = c.locals[:len(c.locals)-1]
	}
}

func opcodeInt(n int) OpCode {
	if n < OpCodeMin || OpCodeMax < n {
		panic(&CompileError{fmt.Sprintf("Program too large: constant/local index or a jump offset %d is out of bounds", n)})
	}
	return OpCode(n)
}

func (c *Compiler) addConstant(constant Value) int {
	// Constant deduplication.
	for i, c := range c.function.Chunk.Constants {
		if c.Equal(constant) {
			return i
		}
	}
	return c.function.Chunk.addConstant(constant)
}

func (c *Compiler) addLocal(name string) int {
	local := Local{Name: name, Depth: c.scopeDepth}
	c.locals = append(c.locals, local)
	return len(c.locals) - 1
}

func (c *Compiler) addUpvalue(idx int, isLocal bool) int {
	for i, upvalue := range c.function.Upvalues {
		if upvalue.Index == idx && upvalue.IsLocal == isLocal {
			return i
		}
	}
	c.function.Upvalues = append(c.function.Upvalues, UpvalueSpec{idx, isLocal})
	return len(c.function.Upvalues) - 1
}

func (c *Compiler) resolveLocal(name string) (int, bool) {
	for i := len(c.locals) - 1; i >= 0; i-- {
		if c.locals[i].Name == name {
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
		c.parent.locals[idx].IsCaptured = true
		return c.addUpvalue(idx, true), true
	}
	if idx, ok := c.parent.resolveUpvalue(name); ok {
		return c.addUpvalue(idx, false), true
	}
	return -1, false
}

func (c *Compiler) compileProgram(program *parser.Program) *Program {
	for i, expr := range program.Exprs {
		c.compileExpr(expr)
		// Pop all intermediate values except for the last one - REPL friendly.
		if i < len(program.Exprs)-1 {
			c.emit1(OpPop, expr.Token())
		}
	}
	// Handle empty programs.
	if len(program.Exprs) == 0 {
		c.emit1(OpNil, program.Token())
	}
	topLevel := c.end(program)
	return &Program{topLevel}
}

func (c *Compiler) compileExpr(expr parser.Expr) {
	switch e := expr.(type) {
	case *parser.Nil:
		c.emit1(OpNil, e.Token())
	case *parser.Atom:
		c.compileAtom(e)
	case *parser.Number:
		val := Number(e.Value)
		idx := c.addConstant(val)
		c.emit2(OpConstant, opcodeInt(idx), e.Token())
	case *parser.Quote:
		if _, ok := e.Value.(*parser.Nil); ok {
			c.emit1(OpNil, e.Token())
			return
		}
		val := c.compileQuoted(e.Value)
		idx := c.addConstant(val)
		c.emit2(OpConstant, opcodeInt(idx), e.Token())
	case *parser.Call:
		c.compileCall(e)
	case *parser.Function:
		c.compileFunction(e)
	case *parser.Define:
		c.compileExpr(e.Value)
		idx := c.addConstant(Atom(e.Name))
		c.emit2(OpSetGlobal, opcodeInt(idx), e.Token())
	case *parser.Let:
		c.compileLet(e)
	default:
		panic(&CompileError{fmt.Sprintf("Unrecognized expression type: %v", expr)})
	}
}

func (c *Compiler) compileAtom(atom *parser.Atom) {
	if idx, ok := c.resolveLocal(atom.Name); ok {
		c.emit2(OpGetLocal, opcodeInt(idx), atom.Token())
		return
	}
	if idx, ok := c.resolveUpvalue(atom.Name); ok {
		c.emit2(OpGetUpvalue, opcodeInt(idx), atom.Token())
		return
	}
	idx := c.addConstant(Atom(atom.Name))
	c.emit2(OpGetGlobal, opcodeInt(idx), atom.Token())
}

func (c *Compiler) compileQuoted(expr parser.Expr) Value {
	// Quoted Nil is handled outside of this function so that we can emit OpConstant
	// with this function's result.
	switch e := expr.(type) {
	case *parser.Atom:
		return Atom(e.Name)
	case *parser.Number:
		return Number(e.Value)
	case *parser.Quote:
		return c.compileQuoted(e.Value)
	case *parser.Call:
		var cons Value = NilVal
		for i := len(e.Args) - 1; i >= 0; i-- {
			cons = Cons(c.compileQuoted(e.Args[i]), cons)
		}
		return Cons(c.compileQuoted(e.Func), cons)
	case *parser.Function:
		var params Value = NilVal
		if e.RestParam != "" {
			params = Atom(e.RestParam)
		}
		for i := len(e.Params) - 1; i >= 0; i-- {
			params = Cons(Atom(e.Params[i]), params)
		}
		return Cons(Atom("lambda"), Cons(params, Cons(c.compileQuoted(e.Body), NilVal)))
	case *parser.Define:
		return Cons(Atom("define"), Cons(Atom(e.Name), Cons(c.compileQuoted(e.Value), NilVal)))
	case *parser.Let:
		var bindings Value = NilVal
		for i := len(e.Bindings) - 1; i >= 0; i-- {
			binding := Cons(Atom(e.Bindings[i].Name), Cons(c.compileQuoted(e.Bindings[i].Value), NilVal))
			bindings = Cons(binding, bindings)
		}
		return Cons(Atom("let"), Cons(bindings, Cons(c.compileQuoted(e.Body), NilVal)))
	default:
		panic(&CompileError{fmt.Sprintf("Unexpected quoted expression type: %v", expr)})
	}
}

func (c *Compiler) compileCall(call *parser.Call) {
	c.compileExpr(call.Func)
	for _, a := range call.Args {
		c.compileExpr(a)
	}
	c.emit2(OpCall, opcodeInt(len(call.Args)), call.Token())
}

func (c *Compiler) compileFunction(f *parser.Function) {
	compiler := newCompiler(c, f.Name, len(f.Params))
	// We don't need to end the scope here, because once a function compilation
	// has finished, we end the entire compiler responsible.
	compiler.beginScope()
	for _, param := range f.Params {
		compiler.addLocal(param)
	}
	if f.RestParam != "" {
		compiler.addLocal(f.RestParam)
		compiler.function.HasRestParam = true
	}
	compiler.compileExpr(f.Body)
	function := compiler.end(f)
	idx := c.addConstant(function)
	c.emit2(OpClosure, opcodeInt(idx), f.Token())
}

func (c *Compiler) compileLet(l *parser.Let) {
	c.beginScope()
	defer c.endScope(l)
	for _, binding := range l.Bindings {
		c.compileExpr(binding.Value)
		idx := c.addLocal(binding.Name)
		c.emit2(OpSetLocal, opcodeInt(idx), l.Token())
	}
	c.compileExpr(l.Body)
}
