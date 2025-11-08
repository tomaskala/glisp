package compiler

import (
	"fmt"
	"slices"

	"tomaskala.com/glisp/internal/runtime"
)

type CompileError struct {
	Line    int
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

func (c *Compiler) errorf(format string, args ...any) error {
	message := fmt.Sprintf(format, args...)
	// TODO: Emit the current line
	// return &CompileError{c.parser.current.Line, message}
	return &CompileError{1, message}
}

func (c *Compiler) emit(code runtime.OpCode) {
	// TODO: Write the current line
	// c.function.Chunk.Write(code, c.parser.previous.Line)
	c.function.Chunk.Write(code, 1)
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
	result := c.function
	c.function = &runtime.Function{Name: c.function.Name}
	c.locals = nil
	return result
}

func (c *Compiler) opcodeInt(n int) runtime.OpCode {
	if n < runtime.OpCodeMin || runtime.OpCodeMax < n {
		panic(c.errorf("program too large: constant index, local index or a jump offset %d is out of bounds", n))
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
		panic(c.errorf("%s expects 1 argument, got 0", name))
	}
	pair1 := expr.AsPair()
	val1 := pair1.Car

	if !pair1.Cdr.IsNil() {
		panic(c.errorf("%s expects 1 argument, got more", name))
	}

	return val1
}

// extract2 requires expr to be a pair (val1 . (val2 . Nil)).
func (c *Compiler) extract2(expr runtime.Value, name string) (runtime.Value, runtime.Value) {
	if !expr.IsPair() {
		panic(c.errorf("%s expects 2 arguments, got 0", name))
	}
	pair1 := expr.AsPair()
	val1 := pair1.Car

	if !pair1.Cdr.IsPair() {
		panic(c.errorf("%s expects 2 arguments, got 1", name))
	}
	pair2 := pair1.Cdr.AsPair()
	val2 := pair2.Car

	if !pair2.Cdr.IsNil() {
		panic(c.errorf("%s expects 2 arguments, got more", name))
	}

	return val1, val2
}

// extract3 requires expr to be a pair (val1 . (val2 . (val3 . Nil))).
func (c *Compiler) extract3(expr runtime.Value, name string) (runtime.Value, runtime.Value, runtime.Value) {
	if !expr.IsPair() {
		panic(c.errorf("%s expects 3 arguments, got 0", name))
	}
	pair1 := expr.AsPair()
	val1 := pair1.Car

	if !pair1.Cdr.IsPair() {
		panic(c.errorf("%s expects 3 arguments, got 1", name))
	}
	pair2 := pair1.Cdr.AsPair()
	val2 := pair2.Car

	if !pair2.Cdr.IsPair() {
		panic(c.errorf("%s expects 3 arguments, got 2", name))
	}
	pair3 := pair2.Cdr.AsPair()
	val3 := pair3.Car

	if !pair3.Cdr.IsNil() {
		panic(c.errorf("%s expects 3 arguments, got more", name))
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
	function := c.end()
	return &runtime.Program{Function: function}, err
}

// Expr:
//
// nil | atom | number | pair.
func (c *Compiler) compileExpr(expr runtime.Value, nameHint runtime.Atom, tailPosition bool) {
	switch {
	case expr.IsNil():
		c.emit(runtime.OpNil)
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

		c.emitArg(op, idx)
	case expr.IsNumber():
		idx := c.addConstant(expr)
		c.emitArg(runtime.OpConstant, idx)
	case expr.IsPair():
		c.compilePair(expr.AsPair(), nameHint, tailPosition)
	default:
		panic(c.errorf("unexpected expression: %v", expr))
	}
}

// Pair:
//
// "(" expr expr ")".
func (c *Compiler) compilePair(pair *runtime.Pair, nameHint runtime.Atom, tailPosition bool) {
	switch pair.Car {
	case runtime.MakeAtom("quote"):
		c.compileQuote(pair.Cdr)
	case runtime.MakeAtom("define"):
		c.compileDefine(pair.Cdr)
	case runtime.MakeAtom("lambda"):
		c.compileLambda(pair.Cdr, nameHint)
	case runtime.MakeAtom("macro"):
		c.compileMacro(pair.Cdr, nameHint)
	case runtime.MakeAtom("if"):
		c.compileIf(pair.Cdr, tailPosition)
	case runtime.MakeAtom("cond"):
		c.compileCond(pair.Cdr, tailPosition)
	case runtime.MakeAtom("and"):
		c.compileAnd(pair.Cdr, tailPosition)
	case runtime.MakeAtom("or"):
		c.compileOr(pair.Cdr, tailPosition)
	case runtime.MakeAtom("set!"):
		c.compileSet(pair.Cdr)
	case runtime.MakeAtom("begin"):
		c.compileBegin(pair.Cdr, tailPosition)
	case runtime.MakeAtom("apply"):
		c.compileApply(pair.Cdr, tailPosition)
	default:
		// Macro expansion.
		if pair.Car.IsAtom() {
			if m, ok := c.macros.GetMacro(pair.Car.AsAtom()); ok {
				expanded, err := c.macros.ExpandMacro(m, pair.Cdr)
				if err != nil {
					panic(c.errorf("%s", err.Error()))
				}
				c.compileExpr(expanded, nameHint, tailPosition)
				return
			}
		}

		// Function call.
		c.compileExpr(pair.Car, runtime.EmptyAtom, false) // Callee
		args := pair.Cdr
		argCount := 0

		for !args.IsNil() {
			if !args.IsPair() {
				panic(c.errorf("function call expects a proper list"))
			}
			arg := args.AsPair()

			c.compileExpr(arg.Car, runtime.EmptyAtom, false) // Argument
			args = arg.Cdr
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
// "(" "quote" expr ")".
func (c *Compiler) compileQuote(expr runtime.Value) {
	quoted := c.extract1(expr, "quote")

	if quoted.IsNil() {
		c.emit(runtime.OpNil)
	} else {
		idx := c.addConstant(quoted)
		c.emitArg(runtime.OpConstant, idx)
	}
}

// Define:
//
// "(" "define" atom expr ")".
func (c *Compiler) compileDefine(expr runtime.Value) {
	name, value := c.extract2(expr, "define")

	if !name.IsAtom() {
		panic(c.errorf("define expects an atom"))
	}

	c.compileExpr(value, name.AsAtom(), false)
	idx := c.addConstant(name)
	c.emitArg(runtime.OpDefineGlobal, idx)
	c.emitArg(runtime.OpConstant, idx)
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
			panic(compiler.errorf("function parameter expects atom"))
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
		panic(compiler.errorf("function parameter expects atom"))
	}

	compiler.compileExpr(body, runtime.EmptyAtom, true)
	function := compiler.end()
	idx := c.addConstant(runtime.MakeFunction(function))
	c.emitArg(runtime.OpClosure, idx)
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
			panic(c.errorf("macro parameter expects atom"))
		}
		atom := param.Car.AsAtom()

		macro.Params = append(macro.Params, atom)
		params = param.Cdr
	}

	if params.IsAtom() {
		atom := params.AsAtom()
		macro.RestParam = atom
	} else if !params.IsNil() {
		panic(c.errorf("macro parameter expects atom"))
	}

	c.macros.StoreMacro(nameHint, macro)
	c.emit(runtime.OpNil)
}

// If:
//
// "(" "if" expr expr expr ")".
func (c *Compiler) compileIf(expr runtime.Value, tailPosition bool) {
	condition, thenBranch, elseBranch := c.extract3(expr, "if")

	c.compileExpr(condition, runtime.EmptyAtom, false)
	thenJump := c.emitJump(runtime.OpJumpIfFalse)
	c.emit(runtime.OpPop)

	c.compileExpr(thenBranch, runtime.EmptyAtom, tailPosition)
	elseJump := c.emitJump(runtime.OpJump)
	c.patchJump(thenJump)
	c.emit(runtime.OpPop)

	c.compileExpr(elseBranch, runtime.EmptyAtom, tailPosition)
	c.patchJump(elseJump)
}

// Cond:
//
// "(" "cond" ( "(" expr expr ")" )* ")".
func (c *Compiler) compileCond(expr runtime.Value, tailPosition bool) {
	curr := expr
	var endJumps []int

	for !curr.IsNil() {
		if !curr.IsPair() {
			panic(c.errorf("cond expect a list of clauses"))
		}
		clause := curr.AsPair()
		condition, value := c.extract2(clause.Car, "cond clause")

		c.compileExpr(condition, runtime.EmptyAtom, false)
		nextJump := c.emitJump(runtime.OpJumpIfFalse)
		c.emit(runtime.OpPop)

		c.compileExpr(value, runtime.EmptyAtom, tailPosition)
		endJump := c.emitJump(runtime.OpJump)
		endJumps = append(endJumps, endJump)
		c.patchJump(nextJump)
		c.emit(runtime.OpPop)

		curr = clause.Cdr
	}

	c.emit(runtime.OpNil)
	for _, jump := range endJumps {
		c.patchJump(jump)
	}
}

// And:
//
// "(" "and" expr* ")".
func (c *Compiler) compileAnd(expr runtime.Value, tailPosition bool) {
	if expr.IsNil() {
		idx := c.addConstant(runtime.True)
		c.emitArg(runtime.OpConstant, idx)
		return
	}

	if !expr.IsPair() {
		panic(c.errorf("and expects arguments"))
	}
	args := expr.AsPair()
	var endJumps []int

	for !args.Cdr.IsNil() {
		c.compileExpr(args.Car, runtime.EmptyAtom, false)
		endJump := c.emitJump(runtime.OpJumpIfFalse)
		endJumps = append(endJumps, endJump)
		c.emit(runtime.OpPop)

		if !args.Cdr.IsPair() {
			panic(c.errorf("and expects arguments"))
		}
		args = args.Cdr.AsPair()
	}

	c.compileExpr(args.Car, runtime.EmptyAtom, tailPosition)
	for _, jump := range endJumps {
		c.patchJump(jump)
	}
}

// Or:
//
// "(" "or" expr* ")".
func (c *Compiler) compileOr(expr runtime.Value, tailPosition bool) {
	if expr.IsNil() {
		c.emit(runtime.OpNil)
		return
	}

	if !expr.IsPair() {
		panic(c.errorf("or expects arguments"))
	}
	args := expr.AsPair()
	var endJumps []int

	for !args.Cdr.IsNil() {
		c.compileExpr(args.Car, runtime.EmptyAtom, false)
		endJump := c.emitJump(runtime.OpJumpIfTrue)
		endJumps = append(endJumps, endJump)
		c.emit(runtime.OpPop)

		if !args.Cdr.IsPair() {
			panic(c.errorf("or expects arguments"))
		}
		args = args.Cdr.AsPair()
	}

	c.compileExpr(args.Car, runtime.EmptyAtom, tailPosition)
	for _, jump := range endJumps {
		c.patchJump(jump)
	}
}

// Set:
//
// "(" "set!" atom expr ")".
func (c *Compiler) compileSet(expr runtime.Value) {
	dest, value := c.extract2(expr, "set!")

	if !dest.IsAtom() {
		panic(c.errorf("set! expects an atom"))
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
	c.emitArg(op, idx)
}

// Begin:
//
// "(" "begin" expr+ ")".
func (c *Compiler) compileBegin(expr runtime.Value, tailPosition bool) {
	if !expr.IsPair() {
		panic(c.errorf("begin expects arguments"))
	}
	args := expr.AsPair()

	for !args.Cdr.IsNil() {
		c.compileExpr(args.Car, runtime.EmptyAtom, false)
		c.emit(runtime.OpPop)

		if !args.Cdr.IsPair() {
			panic(c.errorf("begin expects arguments"))
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
		panic(c.errorf("apply expects arguments"))
	}
	pair := expr.AsPair()

	c.compileExpr(pair.Car, runtime.EmptyAtom, false)
	args := pair.Cdr
	argCount := 0

	for !args.IsNil() {
		if !args.IsPair() {
			panic(c.errorf("apply expects a proper list"))
		}
		arg := args.AsPair()

		c.compileExpr(arg.Car, runtime.EmptyAtom, false)
		args = arg.Cdr
		argCount++
	}

	if argCount == 0 {
		panic(c.errorf("apply expects at least 2 arguments"))
	}

	if tailPosition {
		c.emitArg(runtime.OpTailApply, argCount)
	} else {
		c.emitArg(runtime.OpApply, argCount)
	}
}
