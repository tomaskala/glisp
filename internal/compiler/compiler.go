package compiler

import (
	"fmt"
	"slices"

	"tomaskala.com/glisp/internal/ast"
	"tomaskala.com/glisp/internal/expander"
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

func Compile(name string, program *ast.Program) (prog *Program, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(*CompileError)
		}
	}()
	expandedProgram := expander.Expand(program)
	compiler := newCompiler(nil, name, 0)
	compiler.compileExpr(expandedProgram, false)
	topLevel := compiler.end(program)
	return &Program{topLevel}, err
}

func newCompiler(parent *Compiler, name string, funcArity int) *Compiler {
	return &Compiler{
		parent:   parent,
		function: &Function{Name: NewAtom(name), Arity: funcArity},
	}
}

func (c *Compiler) errorf(format string, args ...any) error {
	message := fmt.Sprintf(format, args...)
	return &CompileError{message}
}

func (c *Compiler) emit1(code OpCode, token tokenizer.Token) {
	c.function.Chunk.write(code, token.Pos.Line)
}

func (c *Compiler) emit2(code1, code2 OpCode, token tokenizer.Token) {
	c.function.Chunk.write(code1, token.Pos.Line)
	c.function.Chunk.write(code2, token.Pos.Line)
}

func (c *Compiler) emitJump(code OpCode, token tokenizer.Token) int {
	c.emit2(code, opcodeInt(OpCodeMax), token)
	return len(c.function.Chunk.Code) - 1
}

func (c *Compiler) patchJump(offset int) {
	jump := len(c.function.Chunk.Code) - 1 - offset
	c.function.Chunk.Code[offset] = opcodeInt(jump)
}

func (c *Compiler) end(node ast.Node) *Function {
	c.emit1(OpReturn, ast.Token(node))
	return c.function
}

func (c *Compiler) beginScope() {
	c.scopeDepth++
}

// TODO: Use errorf here?
func opcodeInt(n int) OpCode {
	if n < OpCodeMin || OpCodeMax < n {
		panic(&CompileError{fmt.Sprintf("program too large: constant/local index or a jump offset %d is out of bounds", n)})
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
	for i, local := range slices.Backward(c.locals) {
		if local.Name == name {
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

func (c *Compiler) compileExpr(node ast.Node, tailPosition bool) {
	switch n := node.(type) {
	case *ast.Program:
		c.compileProgram(n)
	case *ast.Nil:
		c.emit1(OpNil, ast.Token(n))
	case *ast.Atom:
		c.compileAtom(n)
	case *ast.Number:
		val := MakeNumber(n.Value)
		idx := c.addConstant(val)
		c.emit2(OpConstant, opcodeInt(idx), ast.Token(n))
	case *ast.Quote:
		if _, ok := n.Value.(*ast.Nil); ok {
			c.emit1(OpNil, ast.Token(n))
			return
		}
		val := c.compileQuoted(n.Value)
		idx := c.addConstant(val)
		c.emit2(OpConstant, opcodeInt(idx), ast.Token(n))
	case *ast.Call:
		c.compileCall(n, tailPosition)
	case *ast.Function:
		c.compileFunction(n)
	case *ast.Define:
		c.compileExpr(n.Value, false)
		idx := c.addConstant(MakeAtom(n.Name))
		c.emit2(OpDefineGlobal, opcodeInt(idx), ast.Token(n))
		c.emit2(OpConstant, opcodeInt(idx), ast.Token(n))
	case *ast.Let:
		panic(c.errorf("unexpanded let expression"))
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
		panic(c.errorf("unrecognized expression type: %v", node))
	}
}

func (c *Compiler) compileProgram(program *ast.Program) {
	for i, expr := range program.Exprs {
		c.compileExpr(expr, false)
		// Pop all intermediate values except for the last one - REPL friendly.
		if i < len(program.Exprs)-1 {
			c.emit1(OpPop, ast.Token(expr))
		}
	}
	// Handle empty programs.
	if len(program.Exprs) == 0 {
		c.emit1(OpNil, ast.Token(program))
	}
}

func (c *Compiler) compileAtom(atom *ast.Atom) {
	if idx, ok := c.resolveLocal(atom.Name); ok {
		c.emit2(OpGetLocal, opcodeInt(idx), ast.Token(atom))
		return
	}
	if idx, ok := c.resolveUpvalue(atom.Name); ok {
		c.emit2(OpGetUpvalue, opcodeInt(idx), ast.Token(atom))
		return
	}
	idx := c.addConstant(MakeAtom(atom.Name))
	c.emit2(OpGetGlobal, opcodeInt(idx), ast.Token(atom))
}

func (c *Compiler) compileQuoted(node ast.Node) Value {
	// Quoted Nil is handled outside of this function so that we can emit OpConstant
	// with this function's result.
	switch n := node.(type) {
	case *ast.Atom:
		return MakeAtom(n.Name)
	case *ast.Number:
		return MakeNumber(n.Value)
	case *ast.Quote:
		return c.compileQuoted(n.Value)
	case *ast.Call:
		var cons Value = Nil
		for _, arg := range slices.Backward(n.Args) {
			cons = Cons(c.compileQuoted(arg), cons)
		}
		return Cons(c.compileQuoted(n.Func), cons)
	case *ast.Function:
		var params Value = Nil
		if n.RestParam != "" {
			params = MakeAtom(n.RestParam)
		}
		for _, param := range slices.Backward(n.Params) {
			params = Cons(MakeAtom(param), params)
		}
		return Cons(MakeAtom("lambda"), Cons(params, Cons(c.compileQuoted(n.Body), Nil)))
	case *ast.Define:
		return Cons(MakeAtom("define"), Cons(MakeAtom(n.Name), Cons(c.compileQuoted(n.Value), Nil)))
	case *ast.Let:
		panic(c.errorf("unexpanded let expression"))
	case *ast.If:
		cond := c.compileQuoted(n.Cond)
		thenBranch := c.compileQuoted(n.Then)
		elseBranch := c.compileQuoted(n.Else)
		return Cons(MakeAtom("if"), Cons(cond, Cons(thenBranch, Cons(elseBranch, Nil))))
	case *ast.Cond:
		var clauses Value = Nil
		for _, cl := range slices.Backward(n.Clauses) {
			cond := c.compileQuoted(cl.Cond)
			value := c.compileQuoted(cl.Value)
			clause := Cons(cond, Cons(value, Nil))
			clauses = Cons(clause, clauses)
		}
		return Cons(MakeAtom("cond"), clauses)
	case *ast.And:
		var exprs Value = Nil
		for _, expr := range slices.Backward(n.Exprs) {
			exprs = Cons(c.compileQuoted(expr), exprs)
		}
		return Cons(MakeAtom("and"), exprs)
	case *ast.Or:
		var exprs Value = Nil
		for _, expr := range slices.Backward(n.Exprs) {
			exprs = Cons(c.compileQuoted(expr), exprs)
		}
		return Cons(MakeAtom("or"), exprs)
	case *ast.Set:
		return Cons(MakeAtom("set!"), Cons(MakeAtom(n.Variable), Cons(c.compileQuoted(n.Value), Nil)))
	case *ast.Begin:
		var exprs Value = Cons(c.compileQuoted(n.Tail), Nil)
		for _, expr := range slices.Backward(n.Exprs) {
			exprs = Cons(c.compileQuoted(expr), exprs)
		}
		return Cons(MakeAtom("begin"), exprs)
	default:
		panic(c.errorf("unexpected quoted expression type: %v", node))
	}
}

type builtinFunc struct {
	name  string
	kind  Builtin
	arity int
}

var builtins map[string]builtinFunc = map[string]builtinFunc{
	"cons":     {"cons", BuiltinCons, 2},
	"car":      {"car", BuiltinCar, 1},
	"cdr":      {"cdr", BuiltinCdr, 1},
	"+":        {"+", BuiltinAdd, -1},
	"-":        {"-", BuiltinSub, -1},
	"*":        {"*", BuiltinMul, -1},
	"/":        {"/", BuiltinDiv, -1},
	"=":        {"=", BuiltinNumEq, 2},
	"<":        {"<", BuiltinNumLt, 2},
	"<=":       {"<=", BuiltinNumLte, 2},
	">":        {">", BuiltinNumGt, 2},
	">=":       {">=", BuiltinNumGte, 2},
	"eq?":      {"eq?", BuiltinEq, 2},
	"atom?":    {"atom?", BuiltinIsAtom, 1},
	"nil?":     {"nil?", BuiltinIsNil, 1},
	"pair?":    {"pair?", BuiltinIsPair, 1},
	"not":      {"not", BuiltinIsNil, 1},
	"set-car!": {"set-car!", BuiltinSetCar, 2},
	"set-cdr!": {"set-cdr!", BuiltinSetCdr, 2},
	"display":  {"display", BuiltinDisplay, 1},
	"newline":  {"newline", BuiltinNewline, 0},
}

func (c *Compiler) compileCall(call *ast.Call, tailPosition bool) {
	builtin, builtinFound := getBuiltin(call)
	if builtinFound {
		c.checkArgs(builtin.name, builtin.arity, len(call.Args))
	} else {
		c.compileExpr(call.Func, false)
	}
	for _, a := range call.Args {
		c.compileExpr(a, false)
	}
	if builtinFound {
		c.emit1(OpCallBuiltin, ast.Token(call))
		c.emit2(OpCode(builtin.kind), opcodeInt(len(call.Args)), ast.Token(call))
	} else if tailPosition {
		c.emit2(OpTailCall, opcodeInt(len(call.Args)), ast.Token(call))
	} else {
		c.emit2(OpCall, opcodeInt(len(call.Args)), ast.Token(call))
	}
}

func (c *Compiler) checkArgs(name string, arity, argCount int) {
	if arity >= 0 && arity != argCount {
		panic(c.errorf("%s expects %d arguments, got %d", name, arity, argCount))
	}
	if arity == -1 && argCount == 0 {
		panic(c.errorf("%s expects at least 1 argument", name))
	}
}

func getBuiltin(call *ast.Call) (builtinFunc, bool) {
	atom, ok := call.Func.(*ast.Atom)
	if !ok {
		return builtinFunc{}, false
	}
	builtin, ok := builtins[atom.Name]
	return builtin, ok
}

func (c *Compiler) compileFunction(f *ast.Function) {
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
	compiler.compileExpr(f.Body, true)
	function := compiler.end(f)
	idx := c.addConstant(MakeFunction(function))
	c.emit2(OpClosure, opcodeInt(idx), ast.Token(f))
}

func (c *Compiler) compileIf(i *ast.If, tailPosition bool) {
	c.compileExpr(i.Cond, false)
	thenJump := c.emitJump(OpJumpIfFalse, ast.Token(i))
	c.emit1(OpPop, ast.Token(i))
	c.compileExpr(i.Then, tailPosition)
	elseJump := c.emitJump(OpJump, ast.Token(i))
	c.patchJump(thenJump)
	c.emit1(OpPop, ast.Token(i))
	c.compileExpr(i.Else, tailPosition)
	c.patchJump(elseJump)
}

func (c *Compiler) compileCond(cond *ast.Cond, tailPosition bool) {
	if len(cond.Clauses) == 0 {
		c.emit1(OpNil, ast.Token(cond))
		return
	}
	var endJumps []int
	for _, clause := range cond.Clauses {
		c.compileExpr(clause.Cond, false)
		nextJump := c.emitJump(OpJumpIfFalse, ast.Token(clause.Cond))
		c.emit1(OpPop, ast.Token(clause.Cond))
		c.compileExpr(clause.Value, tailPosition)
		endJump := c.emitJump(OpJump, ast.Token(clause.Value))
		endJumps = append(endJumps, endJump)
		c.patchJump(nextJump)
		c.emit1(OpPop, ast.Token(clause.Cond))
	}
	c.emit1(OpNil, ast.Token(cond))
	for _, jump := range endJumps {
		c.patchJump(jump)
	}
}

func (c *Compiler) compileAnd(and *ast.And, tailPosition bool) {
	if len(and.Exprs) == 0 {
		idx := c.addConstant(True)
		c.emit2(OpConstant, opcodeInt(idx), ast.Token(and))
		return
	}
	var endJumps []int
	for i, expr := range and.Exprs {
		exprTailPosition := tailPosition && i == len(and.Exprs)-1
		c.compileExpr(expr, exprTailPosition)
		if i < len(and.Exprs)-1 {
			endJump := c.emitJump(OpJumpIfFalse, ast.Token(expr))
			endJumps = append(endJumps, endJump)
			c.emit1(OpPop, ast.Token(expr))
		}
	}
	for _, jump := range endJumps {
		c.patchJump(jump)
	}
}

func (c *Compiler) compileOr(or *ast.Or, tailPosition bool) {
	if len(or.Exprs) == 0 {
		c.emit1(OpNil, ast.Token(or))
		return
	}
	var endJumps []int
	for i, expr := range or.Exprs {
		exprTailPosition := tailPosition && i == len(or.Exprs)-1
		c.compileExpr(expr, exprTailPosition)
		if i < len(or.Exprs)-1 {
			endJump := c.emitJump(OpJumpIfTrue, ast.Token(expr))
			endJumps = append(endJumps, endJump)
			c.emit1(OpPop, ast.Token(expr))
		}
	}
	for _, jump := range endJumps {
		c.patchJump(jump)
	}
}

func (c *Compiler) compileSet(set *ast.Set) {
	if idx, ok := c.resolveLocal(set.Variable); ok {
		c.compileExpr(set.Value, false)
		c.emit2(OpSetLocal, opcodeInt(idx), ast.Token(set))
		return
	}
	if idx, ok := c.resolveUpvalue(set.Variable); ok {
		c.compileExpr(set.Value, false)
		c.emit2(OpSetUpvalue, opcodeInt(idx), ast.Token(set))
		return
	}
	idx := c.addConstant(MakeAtom(set.Variable))
	c.compileExpr(set.Value, false)
	c.emit2(OpSetGlobal, opcodeInt(idx), ast.Token(set))
}

func (c *Compiler) compileBegin(begin *ast.Begin, tailPosition bool) {
	for _, expr := range begin.Exprs {
		c.compileExpr(expr, false)
		c.emit1(OpPop, ast.Token(begin))
	}
	c.compileExpr(begin.Tail, tailPosition)
}
