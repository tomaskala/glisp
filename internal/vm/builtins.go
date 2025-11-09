package vm

import (
	"fmt"

	"tomaskala.com/glisp/internal/compiler"
	"tomaskala.com/glisp/internal/runtime"
)

func checkArgs(vm *VM, n, expected int, name string) error {
	if n != expected {
		return vm.runtimeError("%s expects %d arguments, got %d", name, expected, n)
	}
	return nil
}

func checkNonZeroArgs(vm *VM, n int, name string) error {
	if n == 0 {
		return vm.runtimeError("%s expects at least 1 argument, got %d", name, n)
	}
	return nil
}

// varNumOp applies the given binary operation as a reducer
// while assuming that at least one argument was provided.
func varNumOp(args []runtime.Value, op func(float64, float64) float64) (runtime.Value, bool) {
	if !args[0].IsNumber() {
		return runtime.Nil, false
	}
	result := args[0].AsNumber()
	for i := 1; i < len(args); i++ {
		if !args[i].IsNumber() {
			return runtime.Nil, false
		}
		result = op(result, args[i].AsNumber())
	}
	return runtime.MakeNumber(result), true
}

func relNumOp(l, r runtime.Value, op func(float64, float64) bool) (runtime.Value, bool) {
	if !l.IsNumber() || !r.IsNumber() {
		return runtime.Nil, false
	}
	if op(l.AsNumber(), r.AsNumber()) {
		return runtime.True, true
	}
	return runtime.Nil, true
}

func builtinGensym(vm *VM, n int) error {
	if err := checkArgs(vm, n, 0, "newline"); err != nil {
		return err
	}

	sym := runtime.MakeAtom(fmt.Sprintf("#:G%d", vm.gensymCounter))
	vm.gensymCounter++
	vm.setTop(sym)
	return nil
}

func builtinEval(vm *VM, n int) error {
	if err := checkArgs(vm, n, 1, "eval"); err != nil {
		return err
	}

	arg := vm.pop()
	child := vm.child()

	c := compiler.NewCompiler("eval", child)
	prog, err := c.Compile(arg)
	if err != nil {
		return err
	}

	result, err := child.Run(prog)
	if err != nil {
		return err
	}

	vm.setTop(result)
	return nil
}

func builtinCons(vm *VM, n int) error {
	if err := checkArgs(vm, n, 2, "cons"); err != nil {
		return err
	}

	cdr := vm.pop()
	car := vm.pop()
	vm.setTop(runtime.Cons(car, cdr))
	return nil
}

func builtinCar(vm *VM, n int) error {
	if err := checkArgs(vm, n, 1, "car"); err != nil {
		return err
	}

	arg := vm.pop()
	if !arg.IsPair() {
		return vm.runtimeError("car expects a pair")
	}

	vm.setTop(arg.AsPair().Car)
	return nil
}

func builtinCdr(vm *VM, n int) error {
	if err := checkArgs(vm, n, 1, "cdr"); err != nil {
		return err
	}

	arg := vm.pop()
	if !arg.IsPair() {
		return vm.runtimeError("cdr expects a pair")
	}

	vm.setTop(arg.AsPair().Cdr)
	return nil
}

func builtinAdd(vm *VM, n int) error {
	if n == 0 {
		vm.setTop(runtime.MakeNumber(0))
		return nil
	}

	args := vm.popSlice(n)
	result, ok := varNumOp(args, func(acc, n float64) float64 { return acc + n })
	if !ok {
		return vm.runtimeError("+ is only defined for numbers")
	}

	vm.setTop(result)
	return nil
}

func builtinSub(vm *VM, n int) error {
	if err := checkNonZeroArgs(vm, n, "-"); err != nil {
		return err
	}

	if n == 1 {
		arg := vm.pop()
		if !arg.IsNumber() {
			return vm.runtimeError("- is only defined for numbers")
		}

		vm.setTop(runtime.MakeNumber(-arg.AsNumber()))
		return nil
	}

	args := vm.popSlice(n)
	result, ok := varNumOp(args, func(acc, n float64) float64 { return acc - n })
	if !ok {
		return vm.runtimeError("- is only defined for numbers")
	}

	vm.setTop(result)
	return nil
}

func builtinMul(vm *VM, n int) error {
	if n == 0 {
		vm.setTop(runtime.MakeNumber(1))
		return nil
	}

	args := vm.popSlice(n)
	result, ok := varNumOp(args, func(acc, n float64) float64 { return acc * n })
	if !ok {
		return vm.runtimeError("* is only defined for numbers")
	}

	vm.setTop(result)
	return nil
}

func builtinDiv(vm *VM, n int) error {
	if err := checkNonZeroArgs(vm, n, "/"); err != nil {
		return err
	}

	if n == 1 {
		arg := vm.pop()
		if !arg.IsNumber() {
			return vm.runtimeError("/ is only defined for numbers")
		}
		vm.setTop(runtime.MakeNumber(1 / arg.AsNumber()))
		return nil
	}

	args := vm.popSlice(n)
	result, ok := varNumOp(args, func(acc, n float64) float64 { return acc / n })
	if !ok {
		return vm.runtimeError("/ is only defined for numbers")
	}

	vm.setTop(result)
	return nil
}

func builtinInt(vm *VM, n int) error {
	if err := checkArgs(vm, n, 1, "int"); err != nil {
		return err
	}

	arg := vm.pop()
	if !arg.IsNumber() {
		return vm.runtimeError("int is only defined for numbers")
	}
	num := arg.AsNumber()

	vm.setTop(runtime.MakeNumber(float64(int(num))))
	return nil
}

func builtinNumEq(vm *VM, n int) error {
	if err := checkArgs(vm, n, 2, ">="); err != nil {
		return err
	}

	r := vm.pop()
	l := vm.pop()
	result, ok := relNumOp(l, r, func(a, b float64) bool { return a == b })
	if !ok {
		return vm.runtimeError("= is only defined for numbers")
	}

	vm.setTop(result)
	return nil
}

func builtinNumLt(vm *VM, n int) error {
	if err := checkArgs(vm, n, 2, ">="); err != nil {
		return err
	}

	r := vm.pop()
	l := vm.pop()
	result, ok := relNumOp(l, r, func(a, b float64) bool { return a < b })
	if !ok {
		return vm.runtimeError("< is only defined for numbers")
	}

	vm.setTop(result)
	return nil
}

func builtinNumLte(vm *VM, n int) error {
	if err := checkArgs(vm, n, 2, ">="); err != nil {
		return err
	}

	r := vm.pop()
	l := vm.pop()
	result, ok := relNumOp(l, r, func(a, b float64) bool { return a <= b })
	if !ok {
		return vm.runtimeError("<= is only defined for numbers")
	}

	vm.setTop(result)
	return nil
}

func builtinNumGt(vm *VM, n int) error {
	if err := checkArgs(vm, n, 2, ">="); err != nil {
		return err
	}

	r := vm.pop()
	l := vm.pop()
	result, ok := relNumOp(l, r, func(a, b float64) bool { return a > b })
	if !ok {
		return vm.runtimeError("> is only defined for numbers")
	}

	vm.setTop(result)
	return nil
}

func builtinNumGte(vm *VM, n int) error {
	if err := checkArgs(vm, n, 2, ">="); err != nil {
		return err
	}

	r := vm.pop()
	l := vm.pop()
	result, ok := relNumOp(l, r, func(a, b float64) bool { return a >= b })
	if !ok {
		return vm.runtimeError(">= is only defined for numbers")
	}

	vm.setTop(result)
	return nil
}

func builtinEq(vm *VM, n int) error {
	if err := checkArgs(vm, n, 2, "eq?"); err != nil {
		return err
	}

	r := vm.pop()
	l := vm.pop()
	if l == r {
		vm.setTop(runtime.True)
	} else {
		vm.setTop(runtime.MakeNil())
	}

	return nil
}

func builtinIsAtom(vm *VM, n int) error {
	if err := checkArgs(vm, n, 1, "atom?"); err != nil {
		return err
	}

	arg := vm.pop()
	if arg.IsAtom() {
		vm.setTop(runtime.True)
	} else {
		vm.setTop(runtime.MakeNil())
	}

	return nil
}

func builtinIsNil(vm *VM, n int) error {
	if err := checkArgs(vm, n, 1, "nil?"); err != nil {
		return err
	}

	arg := vm.pop()
	if runtime.IsTruthy(arg) {
		vm.setTop(runtime.MakeNil())
	} else {
		vm.setTop(runtime.True)
	}

	return nil
}

func builtinIsPair(vm *VM, n int) error {
	if err := checkArgs(vm, n, 1, "pair?"); err != nil {
		return err
	}

	arg := vm.pop()
	if arg.IsPair() {
		vm.setTop(runtime.True)
	} else {
		vm.setTop(runtime.MakeNil())
	}

	return nil
}

func builtinSetCar(vm *VM, n int) error {
	if err := checkArgs(vm, n, 2, "set-car!"); err != nil {
		return err
	}

	car := vm.pop()
	arg := vm.pop()
	if !arg.IsPair() {
		return vm.runtimeError("set-car! expects a pair")
	}

	arg.AsPair().Car = car
	vm.setTop(runtime.MakeNil())
	return nil
}

func builtinSetCdr(vm *VM, n int) error {
	if err := checkArgs(vm, n, 2, "set-cdr!"); err != nil {
		return err
	}

	cdr := vm.pop()
	arg := vm.pop()
	if !arg.IsPair() {
		return vm.runtimeError("set-cdr! expects a pair")
	}

	arg.AsPair().Cdr = cdr
	vm.setTop(runtime.MakeNil())
	return nil
}

func builtinDisplay(vm *VM, n int) error {
	if err := checkArgs(vm, n, 1, "display"); err != nil {
		return err
	}

	fmt.Println(vm.pop())
	vm.setTop(runtime.MakeNil())
	return nil
}

func builtinNewline(vm *VM, n int) error {
	if err := checkArgs(vm, n, 0, "newline"); err != nil {
		return err
	}

	fmt.Println()
	vm.setTop(runtime.MakeNil())
	return nil
}

var (
	// The two slices must be kept in the same order.
	builtinsNames     []runtime.Atom
	builtinsFunctions []func(*VM, int) error
)

func init() {
	builtinsNames = []runtime.Atom{
		runtime.NewAtom("gensym"),
		runtime.NewAtom("eval"),
		runtime.NewAtom("cons"),
		runtime.NewAtom("car"),
		runtime.NewAtom("cdr"),
		runtime.NewAtom("+"),
		runtime.NewAtom("-"),
		runtime.NewAtom("*"),
		runtime.NewAtom("/"),
		runtime.NewAtom("int"),
		runtime.NewAtom("="),
		runtime.NewAtom("<"),
		runtime.NewAtom("<="),
		runtime.NewAtom(">"),
		runtime.NewAtom(">="),
		runtime.NewAtom("eq?"),
		runtime.NewAtom("atom?"),
		runtime.NewAtom("nil?"),
		runtime.NewAtom("pair?"),
		runtime.NewAtom("set-car!"),
		runtime.NewAtom("set-cdr!"),
		runtime.NewAtom("display"),
		runtime.NewAtom("newline"),
	}

	builtinsFunctions = []func(*VM, int) error{
		builtinGensym,
		builtinEval,
		builtinCons,
		builtinCar,
		builtinCdr,
		builtinAdd,
		builtinSub,
		builtinMul,
		builtinDiv,
		builtinInt,
		builtinNumEq,
		builtinNumLt,
		builtinNumLte,
		builtinNumGt,
		builtinNumGte,
		builtinEq,
		builtinIsAtom,
		builtinIsNil,
		builtinIsPair,
		builtinSetCar,
		builtinSetCdr,
		builtinDisplay,
		builtinNewline,
	}
}

func LoadBuiltins() map[runtime.Atom]runtime.Value {
	builtins := make(map[runtime.Atom]runtime.Value)
	builtins[runtime.NewAtom("#t")] = runtime.True

	for i, name := range builtinsNames {
		builtins[name] = runtime.MakeBuiltin(i)
	}

	return builtins
}

func (vm *VM) callBuiltin(idx int, argCount int) error {
	builtin := builtinsFunctions[idx]
	return builtin(vm, argCount)
}
