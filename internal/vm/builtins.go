package vm

import (
	"errors"
	"fmt"

	"tomaskala.com/glisp/internal/compiler"
	"tomaskala.com/glisp/internal/runtime"
)

func checkArgs(n, expected int, name string) error {
	if n != expected {
		return fmt.Errorf("%s expects %d arguments, got %d", name, expected, n)
	}
	return nil
}

func checkNonZeroArgs(n int, name string) error {
	if n == 0 {
		return fmt.Errorf("%s expects at least 1 argument, got %d", name, n)
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

func builtinEval(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 1, "eval"); err != nil {
		return err
	}

	arg := vm.Pop()
	child := vm.Child()

	c := compiler.NewCompiler("eval", child)
	prog, err := c.Compile(arg)
	if err != nil {
		return err
	}

	result, err := child.Run(prog)
	if err != nil {
		return err
	}

	vm.SetTop(result)
	return nil
}

func builtinCons(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 2, "cons"); err != nil {
		return err
	}

	cdr := vm.Pop()
	car := vm.Pop()
	vm.SetTop(runtime.Cons(car, cdr))
	return nil
}

func builtinCar(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 1, "car"); err != nil {
		return err
	}

	arg := vm.Pop()
	if !arg.IsPair() {
		return errors.New("car expects a pair")
	}

	vm.SetTop(arg.AsPair().Car)
	return nil
}

func builtinCdr(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 1, "cdr"); err != nil {
		return err
	}

	arg := vm.Pop()
	if !arg.IsPair() {
		return errors.New("cdr expects a pair")
	}

	vm.SetTop(arg.AsPair().Cdr)
	return nil
}

func builtinAdd(vm runtime.Evaluator, n int) error {
	if n == 0 {
		vm.SetTop(runtime.MakeNumber(0))
		return nil
	}

	args := vm.PopSlice(n)
	result, ok := varNumOp(args, func(acc, n float64) float64 { return acc + n })
	if !ok {
		return errors.New("+ is only defined for numbers")
	}

	vm.SetTop(result)
	return nil
}

func builtinSub(vm runtime.Evaluator, n int) error {
	if err := checkNonZeroArgs(n, "-"); err != nil {
		return err
	}

	if n == 1 {
		arg := vm.Pop()
		if !arg.IsNumber() {
			return errors.New("- is only defined for numbers")
		}

		vm.SetTop(runtime.MakeNumber(-arg.AsNumber()))
		return nil
	}

	args := vm.PopSlice(n)
	result, ok := varNumOp(args, func(acc, n float64) float64 { return acc - n })
	if !ok {
		return errors.New("- is only defined for numbers")
	}

	vm.SetTop(result)
	return nil
}

func builtinMul(vm runtime.Evaluator, n int) error {
	if n == 0 {
		vm.SetTop(runtime.MakeNumber(1))
		return nil
	}

	args := vm.PopSlice(n)
	result, ok := varNumOp(args, func(acc, n float64) float64 { return acc * n })
	if !ok {
		return errors.New("* is only defined for numbers")
	}

	vm.SetTop(result)
	return nil
}

func builtinDiv(vm runtime.Evaluator, n int) error {
	if err := checkNonZeroArgs(n, "/"); err != nil {
		return err
	}

	if n == 1 {
		arg := vm.Pop()
		if !arg.IsNumber() {
			return errors.New("/ is only defined for numbers")
		}
		vm.SetTop(runtime.MakeNumber(1 / arg.AsNumber()))
		return nil
	}

	args := vm.PopSlice(n)
	result, ok := varNumOp(args, func(acc, n float64) float64 { return acc / n })
	if !ok {
		return errors.New("/ is only defined for numbers")
	}

	vm.SetTop(result)
	return nil
}

func builtinNumEq(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 2, ">="); err != nil {
		return err
	}

	r := vm.Pop()
	l := vm.Pop()
	result, ok := relNumOp(l, r, func(a, b float64) bool { return a == b })
	if !ok {
		return errors.New("= is only defined for numbers")
	}

	vm.SetTop(result)
	return nil
}

func builtinNumLt(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 2, ">="); err != nil {
		return err
	}

	r := vm.Pop()
	l := vm.Pop()
	result, ok := relNumOp(l, r, func(a, b float64) bool { return a < b })
	if !ok {
		return errors.New("< is only defined for numbers")
	}

	vm.SetTop(result)
	return nil
}

func builtinNumLte(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 2, ">="); err != nil {
		return err
	}

	r := vm.Pop()
	l := vm.Pop()
	result, ok := relNumOp(l, r, func(a, b float64) bool { return a <= b })
	if !ok {
		return errors.New("<= is only defined for numbers")
	}

	vm.SetTop(result)
	return nil
}

func builtinNumGt(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 2, ">="); err != nil {
		return err
	}

	r := vm.Pop()
	l := vm.Pop()
	result, ok := relNumOp(l, r, func(a, b float64) bool { return a > b })
	if !ok {
		return errors.New("> is only defined for numbers")
	}

	vm.SetTop(result)
	return nil
}

func builtinNumGte(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 2, ">="); err != nil {
		return err
	}

	r := vm.Pop()
	l := vm.Pop()
	result, ok := relNumOp(l, r, func(a, b float64) bool { return a >= b })
	if !ok {
		return errors.New(">= is only defined for numbers")
	}

	vm.SetTop(result)
	return nil
}

func builtinEq(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 2, "eq?"); err != nil {
		return err
	}

	r := vm.Pop()
	l := vm.Pop()
	if l == r {
		vm.SetTop(runtime.True)
	} else {
		vm.SetTop(runtime.MakeNil())
	}

	return nil
}

func builtinIsAtom(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 1, "atom?"); err != nil {
		return err
	}

	arg := vm.Pop()
	if arg.IsAtom() {
		vm.SetTop(runtime.True)
	} else {
		vm.SetTop(runtime.MakeNil())
	}

	return nil
}

func builtinIsNil(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 1, "nil?"); err != nil {
		return err
	}

	arg := vm.Pop()
	if runtime.IsTruthy(arg) {
		vm.SetTop(runtime.MakeNil())
	} else {
		vm.SetTop(runtime.True)
	}

	return nil
}

func builtinIsPair(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 1, "pair?"); err != nil {
		return err
	}

	arg := vm.Pop()
	if arg.IsPair() {
		vm.SetTop(runtime.True)
	} else {
		vm.SetTop(runtime.MakeNil())
	}

	return nil
}

func builtinSetCar(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 2, "set-car!"); err != nil {
		return err
	}

	car := vm.Pop()
	arg := vm.Pop()
	if !arg.IsPair() {
		return errors.New("set-car! expects a pair")
	}

	arg.AsPair().Car = car
	vm.SetTop(runtime.MakeNil())
	return nil
}

func builtinSetCdr(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 2, "set-cdr!"); err != nil {
		return err
	}

	cdr := vm.Pop()
	arg := vm.Pop()
	if !arg.IsPair() {
		return errors.New("set-cdr! expects a pair")
	}

	arg.AsPair().Cdr = cdr
	vm.SetTop(runtime.MakeNil())
	return nil
}

func builtinDisplay(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 1, "display"); err != nil {
		return err
	}

	fmt.Println(vm.Pop())
	vm.SetTop(runtime.MakeNil())
	return nil
}

func builtinNewline(vm runtime.Evaluator, n int) error {
	if err := checkArgs(n, 0, "newline"); err != nil {
		return err
	}

	fmt.Println()
	vm.SetTop(runtime.MakeNil())
	return nil
}

func LoadBuiltins() map[runtime.Atom]runtime.Value {
	return map[runtime.Atom]runtime.Value{
		runtime.NewAtom("#t"):       runtime.True,
		runtime.NewAtom("eval"):     runtime.MakeBuiltin(builtinEval),
		runtime.NewAtom("cons"):     runtime.MakeBuiltin(builtinCons),
		runtime.NewAtom("car"):      runtime.MakeBuiltin(builtinCar),
		runtime.NewAtom("cdr"):      runtime.MakeBuiltin(builtinCdr),
		runtime.NewAtom("+"):        runtime.MakeBuiltin(builtinAdd),
		runtime.NewAtom("-"):        runtime.MakeBuiltin(builtinSub),
		runtime.NewAtom("*"):        runtime.MakeBuiltin(builtinMul),
		runtime.NewAtom("/"):        runtime.MakeBuiltin(builtinDiv),
		runtime.NewAtom("="):        runtime.MakeBuiltin(builtinNumEq),
		runtime.NewAtom("<"):        runtime.MakeBuiltin(builtinNumLt),
		runtime.NewAtom("<="):       runtime.MakeBuiltin(builtinNumLte),
		runtime.NewAtom(">"):        runtime.MakeBuiltin(builtinNumGt),
		runtime.NewAtom(">="):       runtime.MakeBuiltin(builtinNumGte),
		runtime.NewAtom("eq?"):      runtime.MakeBuiltin(builtinEq),
		runtime.NewAtom("atom?"):    runtime.MakeBuiltin(builtinIsAtom),
		runtime.NewAtom("nil?"):     runtime.MakeBuiltin(builtinIsNil),
		runtime.NewAtom("not"):      runtime.MakeBuiltin(builtinIsNil),
		runtime.NewAtom("pair?"):    runtime.MakeBuiltin(builtinIsPair),
		runtime.NewAtom("set-car!"): runtime.MakeBuiltin(builtinSetCar),
		runtime.NewAtom("set-cdr!"): runtime.MakeBuiltin(builtinSetCdr),
		runtime.NewAtom("display"):  runtime.MakeBuiltin(builtinDisplay),
		runtime.NewAtom("newline"):  runtime.MakeBuiltin(builtinNewline),
	}
}
