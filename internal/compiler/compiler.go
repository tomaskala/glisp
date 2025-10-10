package compiler

import (
	"fmt"
	"slices"

	"tomaskala.com/glisp/internal/ast"
	"tomaskala.com/glisp/internal/expander"
	"tomaskala.com/glisp/internal/runtime"
	"tomaskala.com/glisp/internal/tokenizer"
)

type CompileError struct {
	Message string
}

func (e *CompileError) Error() string {
	return e.Message
}

type Local string

type Compiler struct {
	parent   *Compiler         // The enclosing compiler, if any.
	function *runtime.Function // The function currently being compiled.
	locals   []Local           // Local variables of the function.
}

func Compile(name string, program *ast.Program) (prog *runtime.Program, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(*CompileError)
		}
	}()
	expandedProgram := expander.Expand(program)
	compiler := newCompiler(nil, name, 0)
	compiler.compileExpr(expandedProgram, false)
	topLevel := compiler.end(program)
	return &runtime.Program{Function: topLevel}, err
}

func newCompiler(parent *Compiler, name string, funcArity int) *Compiler {
	return &Compiler{
		parent:   parent,
		function: &runtime.Function{Name: runtime.NewAtom(name), Arity: funcArity},
	}
}

func errorf(format string, args ...any) error {
	message := fmt.Sprintf(format, args...)
	return &CompileError{message}
}

func (c *Compiler) emit1(code runtime.OpCode, token tokenizer.Token) {
	c.function.Chunk.Write(code, token.Line)
}

func (c *Compiler) emit2(code1, code2 runtime.OpCode, token tokenizer.Token) {
	c.emit1(code1, token)
	c.emit1(code2, token)
}

func (c *Compiler) emitJump(code runtime.OpCode, token tokenizer.Token) int {
	c.emit2(code, opcodeInt(runtime.OpCodeMax), token)
	return len(c.function.Chunk.Code) - 1
}

func (c *Compiler) patchJump(offset int) {
	jump := len(c.function.Chunk.Code) - 1 - offset
	c.function.Chunk.Code[offset] = opcodeInt(jump)
}

func (c *Compiler) end(node ast.Node) *runtime.Function {
	c.emit1(runtime.OpReturn, ast.Token(node))
	return c.function
}

func opcodeInt(n int) runtime.OpCode {
	if n < runtime.OpCodeMin || runtime.OpCodeMax < n {
		panic(errorf("program too large: constant/local index or a jump offset %d is out of bounds", n))
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

func (c *Compiler) addLocal(name string) int {
	c.locals = append(c.locals, Local(name))
	return len(c.locals) - 1
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

func (c *Compiler) compileExpr(node ast.Node, tailPosition bool) {
	switch n := node.(type) {
	case *ast.Program:
		c.compileProgram(n)
	case *ast.Nil:
		c.emit1(runtime.OpNil, ast.Token(n))
	case *ast.Atom:
		c.compileAtom(n)
	case *ast.Number:
		val := runtime.MakeNumber(n.Value)
		idx := c.addConstant(val)
		c.emit2(runtime.OpConstant, opcodeInt(idx), ast.Token(n))
	case *ast.Quote:
		if _, ok := n.Value.(*ast.Nil); ok {
			c.emit1(runtime.OpNil, ast.Token(n))
			return
		}
		val := c.compileQuoted(n.Value)
		idx := c.addConstant(val)
		c.emit2(runtime.OpConstant, opcodeInt(idx), ast.Token(n))
	case *ast.Call:
		c.compileCall(n, tailPosition)
	case *ast.Function:
		c.compileFunction(n)
	case *ast.Define:
		c.compileExpr(n.Value, false)
		idx := c.addConstant(runtime.MakeAtom(n.Name))
		c.emit2(runtime.OpDefineGlobal, opcodeInt(idx), ast.Token(n))
		c.emit2(runtime.OpConstant, opcodeInt(idx), ast.Token(n))
	case *ast.Let:
		panic(errorf("unexpanded let expression"))
	case *ast.If:
		c.compileIf(n, tailPosition)
	case *ast.Cond:
		c.compileCond(n, tailPosition)
	case *ast.And:
		c.compileAnd(n, tailPosition)
	case *ast.Or:
		c.compileOr(n, tailPosition)
	case *ast.Set:
		c.compileSet(n)
	case *ast.Begin:
		c.compileBegin(n, tailPosition)
	default:
		panic(errorf("unrecognized expression type: %v", node))
	}
}

func (c *Compiler) compileProgram(program *ast.Program) {
	for i, expr := range program.Exprs {
		c.compileExpr(expr, false)
		// Pop all intermediate values except for the last one - REPL friendly.
		if i < len(program.Exprs)-1 {
			c.emit1(runtime.OpPop, ast.Token(expr))
		}
	}
	// Handle empty programs.
	if len(program.Exprs) == 0 {
		c.emit1(runtime.OpNil, ast.Token(program))
	}
}

func (c *Compiler) compileAtom(atom *ast.Atom) {
	if idx, ok := c.resolveLocal(atom.Name); ok {
		c.emit2(runtime.OpGetLocal, opcodeInt(idx), ast.Token(atom))
		return
	}
	if idx, ok := c.resolveUpvalue(atom.Name); ok {
		c.emit2(runtime.OpGetUpvalue, opcodeInt(idx), ast.Token(atom))
		return
	}
	idx := c.addConstant(runtime.MakeAtom(atom.Name))
	c.emit2(runtime.OpGetGlobal, opcodeInt(idx), ast.Token(atom))
}

func (c *Compiler) compileQuoted(node ast.Node) runtime.Value {
	// Quoted Nil is handled outside of this function so that we can emit runtime.OpConstant
	// with this function's result.
	switch n := node.(type) {
	case *ast.Atom:
		return runtime.MakeAtom(n.Name)
	case *ast.Number:
		return runtime.MakeNumber(n.Value)
	case *ast.Quote:
		return c.compileQuoted(n.Value)
	case *ast.Call:
		var cons runtime.Value = runtime.Nil
		for _, arg := range slices.Backward(n.Args) {
			cons = runtime.Cons(c.compileQuoted(arg), cons)
		}
		return runtime.Cons(c.compileQuoted(n.Func), cons)
	case *ast.Function:
		var params runtime.Value = runtime.Nil
		if n.RestParam != "" {
			params = runtime.MakeAtom(n.RestParam)
		}
		for _, param := range slices.Backward(n.Params) {
			params = runtime.Cons(runtime.MakeAtom(param), params)
		}
		return runtime.Cons(runtime.MakeAtom("lambda"), runtime.Cons(params, runtime.Cons(c.compileQuoted(n.Body), runtime.Nil)))
	case *ast.Define:
		return runtime.Cons(runtime.MakeAtom("define"), runtime.Cons(runtime.MakeAtom(n.Name), runtime.Cons(c.compileQuoted(n.Value), runtime.Nil)))
	case *ast.Let:
		panic(errorf("unexpanded let expression"))
	case *ast.If:
		cond := c.compileQuoted(n.Cond)
		thenBranch := c.compileQuoted(n.Then)
		elseBranch := c.compileQuoted(n.Else)
		return runtime.Cons(runtime.MakeAtom("if"), runtime.Cons(cond, runtime.Cons(thenBranch, runtime.Cons(elseBranch, runtime.Nil))))
	case *ast.Cond:
		var clauses runtime.Value = runtime.Nil
		for _, cl := range slices.Backward(n.Clauses) {
			cond := c.compileQuoted(cl.Cond)
			value := c.compileQuoted(cl.Value)
			clause := runtime.Cons(cond, runtime.Cons(value, runtime.Nil))
			clauses = runtime.Cons(clause, clauses)
		}
		return runtime.Cons(runtime.MakeAtom("cond"), clauses)
	case *ast.And:
		var exprs runtime.Value = runtime.Nil
		for _, expr := range slices.Backward(n.Exprs) {
			exprs = runtime.Cons(c.compileQuoted(expr), exprs)
		}
		return runtime.Cons(runtime.MakeAtom("and"), exprs)
	case *ast.Or:
		var exprs runtime.Value = runtime.Nil
		for _, expr := range slices.Backward(n.Exprs) {
			exprs = runtime.Cons(c.compileQuoted(expr), exprs)
		}
		return runtime.Cons(runtime.MakeAtom("or"), exprs)
	case *ast.Set:
		return runtime.Cons(runtime.MakeAtom("set!"), runtime.Cons(runtime.MakeAtom(n.Variable), runtime.Cons(c.compileQuoted(n.Value), runtime.Nil)))
	case *ast.Begin:
		var exprs runtime.Value = runtime.Cons(c.compileQuoted(n.Tail), runtime.Nil)
		for _, expr := range slices.Backward(n.Exprs) {
			exprs = runtime.Cons(c.compileQuoted(expr), exprs)
		}
		return runtime.Cons(runtime.MakeAtom("begin"), exprs)
	default:
		panic(errorf("unexpected quoted expression type: %v", node))
	}
}

func (c *Compiler) compileCall(call *ast.Call, tailPosition bool) {
	c.compileExpr(call.Func, false)
	for _, a := range call.Args {
		c.compileExpr(a, false)
	}
	if tailPosition {
		c.emit2(runtime.OpTailCall, opcodeInt(len(call.Args)), ast.Token(call))
	} else {
		c.emit2(runtime.OpCall, opcodeInt(len(call.Args)), ast.Token(call))
	}
}

func (c *Compiler) compileFunction(f *ast.Function) {
	compiler := newCompiler(c, f.Name, len(f.Params))
	for _, param := range f.Params {
		compiler.addLocal(param)
	}
	if f.RestParam != "" {
		compiler.addLocal(f.RestParam)
		compiler.function.HasRestParam = true
	}
	compiler.compileExpr(f.Body, true)
	function := compiler.end(f)
	idx := c.addConstant(runtime.MakeFunction(function))
	c.emit2(runtime.OpClosure, opcodeInt(idx), ast.Token(f))
}

func (c *Compiler) compileIf(i *ast.If, tailPosition bool) {
	c.compileExpr(i.Cond, false)
	thenJump := c.emitJump(runtime.OpJumpIfFalse, ast.Token(i))
	c.emit1(runtime.OpPop, ast.Token(i))
	c.compileExpr(i.Then, tailPosition)
	elseJump := c.emitJump(runtime.OpJump, ast.Token(i))
	c.patchJump(thenJump)
	c.emit1(runtime.OpPop, ast.Token(i))
	c.compileExpr(i.Else, tailPosition)
	c.patchJump(elseJump)
}

func (c *Compiler) compileCond(cond *ast.Cond, tailPosition bool) {
	if len(cond.Clauses) == 0 {
		c.emit1(runtime.OpNil, ast.Token(cond))
		return
	}
	var endJumps []int
	for _, clause := range cond.Clauses {
		c.compileExpr(clause.Cond, false)
		nextJump := c.emitJump(runtime.OpJumpIfFalse, ast.Token(clause.Cond))
		c.emit1(runtime.OpPop, ast.Token(clause.Cond))
		c.compileExpr(clause.Value, tailPosition)
		endJump := c.emitJump(runtime.OpJump, ast.Token(clause.Value))
		endJumps = append(endJumps, endJump)
		c.patchJump(nextJump)
		c.emit1(runtime.OpPop, ast.Token(clause.Cond))
	}
	c.emit1(runtime.OpNil, ast.Token(cond))
	for _, jump := range endJumps {
		c.patchJump(jump)
	}
}

func (c *Compiler) compileAnd(and *ast.And, tailPosition bool) {
	if len(and.Exprs) == 0 {
		idx := c.addConstant(runtime.True)
		c.emit2(runtime.OpConstant, opcodeInt(idx), ast.Token(and))
		return
	}
	var endJumps []int
	for i, expr := range and.Exprs {
		exprTailPosition := tailPosition && i == len(and.Exprs)-1
		c.compileExpr(expr, exprTailPosition)
		if i < len(and.Exprs)-1 {
			endJump := c.emitJump(runtime.OpJumpIfFalse, ast.Token(expr))
			endJumps = append(endJumps, endJump)
			c.emit1(runtime.OpPop, ast.Token(expr))
		}
	}
	for _, jump := range endJumps {
		c.patchJump(jump)
	}
}

func (c *Compiler) compileOr(or *ast.Or, tailPosition bool) {
	if len(or.Exprs) == 0 {
		c.emit1(runtime.OpNil, ast.Token(or))
		return
	}
	var endJumps []int
	for i, expr := range or.Exprs {
		exprTailPosition := tailPosition && i == len(or.Exprs)-1
		c.compileExpr(expr, exprTailPosition)
		if i < len(or.Exprs)-1 {
			endJump := c.emitJump(runtime.OpJumpIfTrue, ast.Token(expr))
			endJumps = append(endJumps, endJump)
			c.emit1(runtime.OpPop, ast.Token(expr))
		}
	}
	for _, jump := range endJumps {
		c.patchJump(jump)
	}
}

func (c *Compiler) compileSet(set *ast.Set) {
	if idx, ok := c.resolveLocal(set.Variable); ok {
		c.compileExpr(set.Value, false)
		c.emit2(runtime.OpSetLocal, opcodeInt(idx), ast.Token(set))
		return
	}
	if idx, ok := c.resolveUpvalue(set.Variable); ok {
		c.compileExpr(set.Value, false)
		c.emit2(runtime.OpSetUpvalue, opcodeInt(idx), ast.Token(set))
		return
	}
	idx := c.addConstant(runtime.MakeAtom(set.Variable))
	c.compileExpr(set.Value, false)
	c.emit2(runtime.OpSetGlobal, opcodeInt(idx), ast.Token(set))
}

func (c *Compiler) compileBegin(begin *ast.Begin, tailPosition bool) {
	for _, expr := range begin.Exprs {
		c.compileExpr(expr, false)
		c.emit1(runtime.OpPop, ast.Token(begin))
	}
	c.compileExpr(begin.Tail, tailPosition)
}
