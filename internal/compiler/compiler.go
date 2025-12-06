package compiler

import (
	"fmt"
	"slices"

	"tomaskala.com/glisp/internal/runtime"
)

type CompileError struct {
	Line    runtime.Line
	Message string
}

func (e *CompileError) Error() string {
	return fmt.Sprintf("compile error at %d: %s", e.Line, e.Message)
}

type Compiler struct {
	parent   *Compiler             // The enclosing compiler, if any.
	function *runtime.Function     // The function currently being compiled.
	locals   []runtime.Atom        // Local variables of the function.
	macros   runtime.MacroRegistry // Stores and expands macros.
}

func NewCompiler(name string, macros runtime.MacroRegistry) *Compiler {
	return newCompiler(nil, runtime.NewAtom(name), macros)
}

func newCompiler(parent *Compiler, name runtime.Atom, macros runtime.MacroRegistry) *Compiler {
	return &Compiler{
		parent:   parent,
		function: &runtime.Function{Name: name},
		macros:   macros,
	}
}

func (c *Compiler) errorf(line runtime.Line, format string, args ...any) error {
	message := fmt.Sprintf(format, args...)
	return &CompileError{line, message}
}

func (c *Compiler) emit(code runtime.OpCode, line runtime.Line) {
	c.function.Chunk.Write(code, line)
}

func (c *Compiler) emitArg(code runtime.OpCode, arg int, line runtime.Line) {
	c.emit(code, line)
	c.emit(c.opcodeInt(arg, line), line)
}

func (c *Compiler) emitJump(code runtime.OpCode, line runtime.Line) int {
	c.emitArg(code, runtime.OpCodeMax, line)
	return len(c.function.Chunk.Code) - 1
}

func (c *Compiler) patchJump(offset int, line runtime.Line) {
	jump := len(c.function.Chunk.Code) - 1 - offset
	c.function.Chunk.Code[offset] = c.opcodeInt(jump, line)
}

func (c *Compiler) end(line runtime.Line) *runtime.Function {
	c.emit(runtime.OpReturn, line)
	result := c.function
	c.function = &runtime.Function{Name: c.function.Name}
	c.locals = nil
	return result
}

func (c *Compiler) opcodeInt(n int, line runtime.Line) runtime.OpCode {
	if n < runtime.OpCodeMin || runtime.OpCodeMax < n {
		panic(c.errorf(line, "program too large: constant index, local index or a jump offset %d is out of bounds", n))
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

func (c *Compiler) addLocal(name runtime.Atom) {
	c.locals = append(c.locals, name)
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

func (c *Compiler) resolveLocal(name runtime.Atom) (int, bool) {
	for i, local := range slices.Backward(c.locals) {
		if local == name {
			return i, true
		}
	}

	return -1, false
}

func (c *Compiler) resolveUpvalue(name runtime.Atom) (int, bool) {
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

// extract1 requires expr to be a pair (val1 . Nil).
func (c *Compiler) extract1(expr runtime.Value, name string) runtime.Value {
	if !expr.IsPair() {
		panic(c.errorf(expr.Line, "%s expects 1 argument, got 0", name))
	}
	pair1 := expr.AsPair()
	val1 := pair1.Car

	if !pair1.Cdr.IsNil() {
		panic(c.errorf(pair1.Cdr.Line, "%s expects 1 argument, got more", name))
	}

	return val1
}

// extract2 requires expr to be a pair (val1 . (val2 . Nil)).
func (c *Compiler) extract2(expr runtime.Value, name string) (runtime.Value, runtime.Value) {
	if !expr.IsPair() {
		panic(c.errorf(expr.Line, "%s expects 2 arguments, got 0", name))
	}
	pair1 := expr.AsPair()
	val1 := pair1.Car

	if !pair1.Cdr.IsPair() {
		panic(c.errorf(pair1.Cdr.Line, "%s expects 2 arguments, got 1", name))
	}
	pair2 := pair1.Cdr.AsPair()
	val2 := pair2.Car

	if !pair2.Cdr.IsNil() {
		panic(c.errorf(pair2.Cdr.Line, "%s expects 2 arguments, got more", name))
	}

	return val1, val2
}

// extract3 requires expr to be a pair (val1 . (val2 . (val3 . Nil))).
func (c *Compiler) extract3(expr runtime.Value, name string) (runtime.Value, runtime.Value, runtime.Value) {
	if !expr.IsPair() {
		panic(c.errorf(expr.Line, "%s expects 3 arguments, got 0", name))
	}
	pair1 := expr.AsPair()
	val1 := pair1.Car

	if !pair1.Cdr.IsPair() {
		panic(c.errorf(pair1.Cdr.Line, "%s expects 3 arguments, got 1", name))
	}
	pair2 := pair1.Cdr.AsPair()
	val2 := pair2.Car

	if !pair2.Cdr.IsPair() {
		panic(c.errorf(pair2.Cdr.Line, "%s expects 3 arguments, got 2", name))
	}
	pair3 := pair2.Cdr.AsPair()
	val3 := pair3.Car

	if !pair3.Cdr.IsNil() {
		panic(c.errorf(pair3.Cdr.Line, "%s expects 3 arguments, got more", name))
	}

	return val1, val2, val3
}

func (c *Compiler) Compile(expr runtime.Value) (prog *runtime.Program, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(*CompileError)
		}
	}()

	c.compileExpr(expr, runtime.EmptyAtom, false)
	function := c.end(expr.Line)
	return &runtime.Program{Function: function}, err
}

// Expr:
//
// nil | atom | number | pair.
func (c *Compiler) compileExpr(expr runtime.Value, nameHint runtime.Atom, tailPosition bool) {
	switch {
	case expr.IsNil():
		c.emit(runtime.OpNil, expr.Line)
	case expr.IsAtom():
		atom := expr.AsAtom()

		var idx int
		var op runtime.OpCode
		if i, ok := c.resolveLocal(atom); ok {
			idx = i
			op = runtime.OpGetLocal
		} else if i, ok := c.resolveUpvalue(atom); ok {
			idx = i
			op = runtime.OpGetUpvalue
		} else {
			idx = c.addConstant(expr)
			op = runtime.OpGetGlobal
		}

		c.emitArg(op, idx, expr.Line)
	case expr.IsNumber():
		idx := c.addConstant(expr)
		c.emitArg(runtime.OpConstant, idx, expr.Line)
	case expr.IsPair():
		c.compilePair(expr.AsPair(), nameHint, tailPosition)
	default:
		panic(c.errorf(expr.Line, "unexpected expression: %v", expr))
	}
}

// Pair:
//
// "(" expr expr ")".
func (c *Compiler) compilePair(pair *runtime.Pair, nameHint runtime.Atom, tailPosition bool) {
	// Known form or macro.
	if pair.Car.IsAtom() && c.compileForm(pair.Car.AsAtom(), pair.Cdr, nameHint, tailPosition) {
		return
	}

	// Function call.
	c.compileExpr(pair.Car, runtime.EmptyAtom, false) // Callee
	args := pair.Cdr
	argCount := 0

	for !args.IsNil() {
		if !args.IsPair() {
			panic(c.errorf(args.Line, "function call expects a proper list"))
		}
		arg := args.AsPair()

		c.compileExpr(arg.Car, runtime.EmptyAtom, false) // Argument
		args = arg.Cdr
		argCount++
	}

	if tailPosition {
		c.emitArg(runtime.OpTailCall, argCount, pair.Car.Line)
	} else {
		c.emitArg(runtime.OpCall, argCount, pair.Car.Line)
	}
}

func (c *Compiler) compileForm(name runtime.Atom, args runtime.Value, nameHint runtime.Atom, tailPosition bool) bool {
	switch name {
	case runtime.NewAtom("quote"):
		c.compileQuote(args)
	case runtime.NewAtom("define"):
		c.compileDefine(args)
	case runtime.NewAtom("lambda"):
		c.compileLambda(args, nameHint)
	case runtime.NewAtom("macro"):
		c.compileMacro(args, nameHint)
	case runtime.NewAtom("if"):
		c.compileIf(args, tailPosition)
	case runtime.NewAtom("cond"):
		c.compileCond(args, tailPosition)
	case runtime.NewAtom("and"):
		c.compileAnd(args, tailPosition)
	case runtime.NewAtom("or"):
		c.compileOr(args, tailPosition)
	case runtime.NewAtom("set!"):
		c.compileSet(args)
	case runtime.NewAtom("begin"):
		c.compileBegin(args, tailPosition)
	case runtime.NewAtom("apply"):
		c.compileApply(args, tailPosition)
	default:
		// Macro expansion.
		m, ok := c.macros.GetMacro(name)
		if !ok {
			return false
		}

		expanded, err := c.macros.ExpandMacro(m, args)
		if err != nil {
			panic(c.errorf(args.Line, "%s", err.Error()))
		}
		c.compileExpr(expanded, nameHint, tailPosition)
	}

	return true
}

// Quote:
//
// "(" "quote" expr ")".
func (c *Compiler) compileQuote(expr runtime.Value) {
	quoted := c.extract1(expr, "quote")

	if quoted.IsNil() {
		c.emit(runtime.OpNil, quoted.Line)
	} else {
		idx := c.addConstant(quoted)
		c.emitArg(runtime.OpConstant, idx, quoted.Line)
	}
}

// Define:
//
// "(" "define" atom expr ")".
func (c *Compiler) compileDefine(expr runtime.Value) {
	name, value := c.extract2(expr, "define")

	if !name.IsAtom() {
		panic(c.errorf(expr.Line, "define expects an atom"))
	}

	c.compileExpr(value, name.AsAtom(), false)
	idx := c.addConstant(name)
	c.emitArg(runtime.OpDefineGlobal, idx, expr.Line)
	c.emitArg(runtime.OpConstant, idx, expr.Line)
}

// Lambda:
//
// "(" "lambda" (atom | "(" expr* ")" | "(" expr+ "." expr ")") expr ")".
func (c *Compiler) compileLambda(expr runtime.Value, nameHint runtime.Atom) {
	params, body := c.extract2(expr, "lambda")
	compiler := newCompiler(c, nameHint, c.macros)

	for params.IsPair() {
		param := params.AsPair()
		if !param.Car.IsAtom() {
			panic(compiler.errorf(expr.Line, "function parameter expects atom"))
		}
		atom := param.Car.AsAtom()

		compiler.addLocal(atom)
		compiler.function.Arity++
		params = param.Cdr
	}

	if params.IsAtom() {
		atom := params.AsAtom()
		compiler.addLocal(atom)
		compiler.function.IsVariadic = true
	} else if !params.IsNil() {
		panic(compiler.errorf(expr.Line, "function parameter expects atom"))
	}

	compiler.compileExpr(body, runtime.EmptyAtom, true)
	function := compiler.end(body.Line)
	idx := c.addConstant(runtime.MakeFunction(function))
	c.emitArg(runtime.OpClosure, idx, expr.Line)
}

// Macro:
//
// "(" "macro" (atom | "(" expr* ")" | "(" expr+ "." expr ")") expr ")".
func (c *Compiler) compileMacro(expr runtime.Value, nameHint runtime.Atom) {
	params, body := c.extract2(expr, "macro")
	macro := runtime.NewMacro(nameHint, body)

	for params.IsPair() {
		param := params.AsPair()
		if !param.Car.IsAtom() {
			panic(c.errorf(expr.Line, "macro parameter expects atom"))
		}
		atom := param.Car.AsAtom()

		macro.Params = append(macro.Params, atom)
		params = param.Cdr
	}

	if params.IsAtom() {
		atom := params.AsAtom()
		macro.RestParam = atom
	} else if !params.IsNil() {
		panic(c.errorf(expr.Line, "macro parameter expects atom"))
	}

	c.macros.StoreMacro(nameHint, macro)
	c.emit(runtime.OpNil, expr.Line)
}

// If:
//
// "(" "if" expr expr expr ")".
func (c *Compiler) compileIf(expr runtime.Value, tailPosition bool) {
	condition, thenBranch, elseBranch := c.extract3(expr, "if")

	c.compileExpr(condition, runtime.EmptyAtom, false)
	thenJump := c.emitJump(runtime.OpJumpIfFalse, condition.Line)
	c.emit(runtime.OpPop, condition.Line)

	c.compileExpr(thenBranch, runtime.EmptyAtom, tailPosition)
	elseJump := c.emitJump(runtime.OpJump, expr.Line)
	c.patchJump(thenJump, thenBranch.Line)
	c.emit(runtime.OpPop, thenBranch.Line)

	c.compileExpr(elseBranch, runtime.EmptyAtom, tailPosition)
	c.patchJump(elseJump, elseBranch.Line)
}

// Cond:
//
// "(" "cond" ( "(" expr expr ")" )* ")".
func (c *Compiler) compileCond(expr runtime.Value, tailPosition bool) {
	curr := expr
	var endJumps []int

	for !curr.IsNil() {
		if !curr.IsPair() {
			panic(c.errorf(expr.Line, "cond expect a list of clauses"))
		}
		clause := curr.AsPair()
		condition, value := c.extract2(clause.Car, "cond clause")

		c.compileExpr(condition, runtime.EmptyAtom, false)
		nextJump := c.emitJump(runtime.OpJumpIfFalse, condition.Line)
		c.emit(runtime.OpPop, condition.Line)

		c.compileExpr(value, runtime.EmptyAtom, tailPosition)
		endJump := c.emitJump(runtime.OpJump, value.Line)
		endJumps = append(endJumps, endJump)
		c.patchJump(nextJump, value.Line)
		c.emit(runtime.OpPop, value.Line)

		curr = clause.Cdr
	}

	c.emit(runtime.OpNil, expr.Line)
	for _, jump := range endJumps {
		c.patchJump(jump, expr.Line)
	}
}

// And:
//
// "(" "and" expr* ")".
func (c *Compiler) compileAnd(expr runtime.Value, tailPosition bool) {
	if expr.IsNil() {
		idx := c.addConstant(runtime.True)
		c.emitArg(runtime.OpConstant, idx, expr.Line)
		return
	}

	if !expr.IsPair() {
		panic(c.errorf(expr.Line, "and expects arguments"))
	}
	args := expr.AsPair()
	var endJumps []int

	for !args.Cdr.IsNil() {
		c.compileExpr(args.Car, runtime.EmptyAtom, false)
		endJump := c.emitJump(runtime.OpJumpIfFalse, args.Car.Line)
		endJumps = append(endJumps, endJump)
		c.emit(runtime.OpPop, args.Car.Line)

		if !args.Cdr.IsPair() {
			panic(c.errorf(expr.Line, "and expects arguments"))
		}
		args = args.Cdr.AsPair()
	}

	c.compileExpr(args.Car, runtime.EmptyAtom, tailPosition)
	for _, jump := range endJumps {
		c.patchJump(jump, expr.Line)
	}
}

// Or:
//
// "(" "or" expr* ")".
func (c *Compiler) compileOr(expr runtime.Value, tailPosition bool) {
	if expr.IsNil() {
		c.emit(runtime.OpNil, expr.Line)
		return
	}

	if !expr.IsPair() {
		panic(c.errorf(expr.Line, "or expects arguments"))
	}
	args := expr.AsPair()
	var endJumps []int

	for !args.Cdr.IsNil() {
		c.compileExpr(args.Car, runtime.EmptyAtom, false)
		endJump := c.emitJump(runtime.OpJumpIfTrue, args.Car.Line)
		endJumps = append(endJumps, endJump)
		c.emit(runtime.OpPop, args.Car.Line)

		if !args.Cdr.IsPair() {
			panic(c.errorf(expr.Line, "or expects arguments"))
		}
		args = args.Cdr.AsPair()
	}

	c.compileExpr(args.Car, runtime.EmptyAtom, tailPosition)
	for _, jump := range endJumps {
		c.patchJump(jump, expr.Line)
	}
}

// Set:
//
// "(" "set!" atom expr ")".
func (c *Compiler) compileSet(expr runtime.Value) {
	dest, value := c.extract2(expr, "set!")

	if !dest.IsAtom() {
		panic(c.errorf(expr.Line, "set! expects an atom"))
	}
	name := dest.AsAtom()

	var idx int
	var op runtime.OpCode
	if i, ok := c.resolveLocal(name); ok {
		idx = i
		op = runtime.OpSetLocal
	} else if i, ok := c.resolveUpvalue(name); ok {
		idx = i
		op = runtime.OpSetUpvalue
	} else {
		idx = c.addConstant(dest)
		op = runtime.OpSetGlobal
	}

	c.compileExpr(value, runtime.EmptyAtom, false)
	c.emitArg(op, idx, value.Line)
}

// Begin:
//
// "(" "begin" expr+ ")".
func (c *Compiler) compileBegin(expr runtime.Value, tailPosition bool) {
	if !expr.IsPair() {
		panic(c.errorf(expr.Line, "begin expects arguments"))
	}
	args := expr.AsPair()

	for !args.Cdr.IsNil() {
		c.compileExpr(args.Car, runtime.EmptyAtom, false)
		c.emit(runtime.OpPop, args.Car.Line)

		if !args.Cdr.IsPair() {
			panic(c.errorf(expr.Line, "begin expects arguments"))
		}
		args = args.Cdr.AsPair()
	}

	c.compileExpr(args.Car, runtime.EmptyAtom, tailPosition)
}

// Apply:
//
// "(" "apply" expr expr* pair ")".
func (c *Compiler) compileApply(expr runtime.Value, tailPosition bool) {
	if !expr.IsPair() {
		panic(c.errorf(expr.Line, "apply expects arguments"))
	}
	pair := expr.AsPair()

	c.compileExpr(pair.Car, runtime.EmptyAtom, false)
	args := pair.Cdr
	argCount := 0

	for !args.IsNil() {
		if !args.IsPair() {
			panic(c.errorf(expr.Line, "apply expects a proper list"))
		}
		arg := args.AsPair()

		c.compileExpr(arg.Car, runtime.EmptyAtom, false)
		args = arg.Cdr
		argCount++
	}

	if argCount == 0 {
		panic(c.errorf(expr.Line, "apply expects at least 2 arguments"))
	}

	if tailPosition {
		c.emitArg(runtime.OpTailApply, argCount, expr.Line)
	} else {
		c.emitArg(runtime.OpApply, argCount, expr.Line)
	}
}
