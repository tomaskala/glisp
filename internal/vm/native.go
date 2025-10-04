package vm

import (
	"errors"
	"fmt"

	"tomaskala.com/glisp/internal/compiler"
)

func loadNatives() map[compiler.Atom]compiler.Value {
	return map[compiler.Atom]compiler.Value{
		compiler.NewAtom("#t"):    compiler.True,
		compiler.NewAtom("cons"):  compiler.MakeNative(compiler.NewNative("cons", 2, NativeCons)),
		compiler.NewAtom("car"):   compiler.MakeNative(compiler.NewNative("car", 1, NativeCar)),
		compiler.NewAtom("cdr"):   compiler.MakeNative(compiler.NewNative("cdr", 1, NativeCdr)),
		compiler.NewAtom("+"):     compiler.MakeNative(compiler.NewNative("+", -1, NativeAdd)),
		compiler.NewAtom("-"):     compiler.MakeNative(compiler.NewNative("-", -1, NativeSub)),
		compiler.NewAtom("*"):     compiler.MakeNative(compiler.NewNative("*", -1, NativeMul)),
		compiler.NewAtom("/"):     compiler.MakeNative(compiler.NewNative("/", -1, NativeDiv)),
		compiler.NewAtom("="):     compiler.MakeNative(compiler.NewNative("=", 2, NativeNumEq)),
		compiler.NewAtom("<"):     compiler.MakeNative(compiler.NewNative("<", 2, NativeNumLt)),
		compiler.NewAtom("<="):    compiler.MakeNative(compiler.NewNative("<=", 2, NativeNumLte)),
		compiler.NewAtom(">"):     compiler.MakeNative(compiler.NewNative(">", 2, NativeNumGt)),
		compiler.NewAtom(">="):    compiler.MakeNative(compiler.NewNative(">=", 2, NativeNumGte)),
		compiler.NewAtom("eq?"):   compiler.MakeNative(compiler.NewNative("eq?", 2, NativeEq)),
		compiler.NewAtom("atom?"): compiler.MakeNative(compiler.NewNative("atom?", 1, NativeIsAtom)),
		compiler.NewAtom("nil?"):  compiler.MakeNative(compiler.NewNative("nil?", 1, NativeIsNil)),
		compiler.NewAtom("pair?"): compiler.MakeNative(compiler.NewNative("pair?", 1, NativeIsPair)),
		// Reuse the nil? implementation, since they behave the same way.
		compiler.NewAtom("not"):      compiler.MakeNative(compiler.NewNative("not", 1, NativeIsNil)),
		compiler.NewAtom("set-car!"): compiler.MakeNative(compiler.NewNative("set-car!", 2, NativeSetCar)),
		compiler.NewAtom("set-cdr!"): compiler.MakeNative(compiler.NewNative("set-cdr!", 2, NativeSetCdr)),
		compiler.NewAtom("display"):  compiler.MakeNative(compiler.NewNative("display", 1, NativeDisplay)),
		compiler.NewAtom("newline"):  compiler.MakeNative(compiler.NewNative("newline", 0, NativeNewline)),
	}
}

func NativeCons(args []compiler.Value) (compiler.Value, error) {
	car := args[0]
	cdr := args[1]
	return compiler.Cons(car, cdr), nil
}

func NativeCar(args []compiler.Value) (compiler.Value, error) {
	if cons, ok := args[0].AsPair(); ok {
		return cons.Car, nil
	}
	return compiler.Nil, errors.New("car expects a pair")
}

func NativeCdr(args []compiler.Value) (compiler.Value, error) {
	if cons, ok := args[0].AsPair(); ok {
		return cons.Cdr, nil
	}
	return compiler.Nil, errors.New("cdr expects a pair")
}

func NativeAdd(args []compiler.Value) (compiler.Value, error) {
	result := float64(0)
	for _, arg := range args {
		num, ok := arg.AsNumber()
		if !ok {
			return compiler.Nil, errors.New("attempting to add non-number")
		}
		result += num
	}
	return compiler.MakeNumber(result), nil
}

func NativeSub(args []compiler.Value) (compiler.Value, error) {
	switch len(args) {
	case 0:
		return compiler.Nil, errors.New("- expects at least one argument")
	case 1:
		num, ok := args[0].AsNumber()
		if !ok {
			return compiler.Nil, errors.New("attempting to negate non-number")
		}
		return compiler.MakeNumber(-num), nil
	default:
		result, ok := args[0].AsNumber()
		if !ok {
			return compiler.Nil, errors.New("attempting to subtract non-number")
		}
		for i := 1; i < len(args); i++ {
			num, ok := args[i].AsNumber()
			if !ok {
				return compiler.Nil, errors.New("attempting to subtract non-number")
			}
			result -= num
		}
		return compiler.MakeNumber(result), nil
	}
}

func NativeMul(args []compiler.Value) (compiler.Value, error) {
	result := float64(1)
	for _, arg := range args {
		num, ok := arg.AsNumber()
		if !ok {
			return compiler.Nil, errors.New("attempting to multiply non-number")
		}
		result *= num
	}
	return compiler.MakeNumber(result), nil
}

func NativeDiv(args []compiler.Value) (compiler.Value, error) {
	switch len(args) {
	case 0:
		return compiler.Nil, errors.New("/ expects at least one argument")
	case 1:
		num, ok := args[0].AsNumber()
		if !ok {
			return compiler.Nil, errors.New("attempting to invert non-number")
		}
		if num == 0 {
			return compiler.Nil, errors.New("zero division")
		}
		return compiler.MakeNumber(1 / num), nil
	default:
		result, ok := args[0].AsNumber()
		if !ok {
			return compiler.Nil, errors.New("attempting to divide non-number")
		}
		for i := 1; i < len(args); i++ {
			num, ok := args[i].AsNumber()
			if !ok {
				return compiler.Nil, errors.New("attempting to divide non-number")
			}
			if num == 0 {
				return compiler.Nil, errors.New("zero division")
			}
			result /= num
		}
		return compiler.MakeNumber(result), nil
	}
}

func NativeNumEq(args []compiler.Value) (compiler.Value, error) {
	num1, ok1 := args[0].AsNumber()
	num2, ok2 := args[1].AsNumber()
	if !ok1 || !ok2 {
		return compiler.Nil, errors.New("= is only defined for numbers")
	}
	if num1 == num2 {
		return compiler.True, nil
	}
	return compiler.Nil, nil
}

func NativeNumLt(args []compiler.Value) (compiler.Value, error) {
	num1, ok1 := args[0].AsNumber()
	num2, ok2 := args[1].AsNumber()
	if !ok1 || !ok2 {
		return compiler.Nil, errors.New("< is only defined for numbers")
	}
	if num1 < num2 {
		return compiler.True, nil
	}
	return compiler.Nil, nil
}

func NativeNumLte(args []compiler.Value) (compiler.Value, error) {
	num1, ok1 := args[0].AsNumber()
	num2, ok2 := args[1].AsNumber()
	if !ok1 || !ok2 {
		return compiler.Nil, errors.New("<= is only defined for numbers")
	}
	if num1 <= num2 {
		return compiler.True, nil
	}
	return compiler.Nil, nil
}

func NativeNumGt(args []compiler.Value) (compiler.Value, error) {
	num1, ok1 := args[0].AsNumber()
	num2, ok2 := args[1].AsNumber()
	if !ok1 || !ok2 {
		return compiler.Nil, errors.New("> is only defined for numbers")
	}
	if num1 > num2 {
		return compiler.True, nil
	}
	return compiler.Nil, nil
}

func NativeNumGte(args []compiler.Value) (compiler.Value, error) {
	num1, ok1 := args[0].AsNumber()
	num2, ok2 := args[1].AsNumber()
	if !ok1 || !ok2 {
		return compiler.Nil, errors.New(">= is only defined for numbers")
	}
	if num1 >= num2 {
		return compiler.True, nil
	}
	return compiler.Nil, nil
}

func NativeEq(args []compiler.Value) (compiler.Value, error) {
	if args[0] == args[1] {
		return compiler.True, nil
	}
	return compiler.Nil, nil
}

func NativeIsAtom(args []compiler.Value) (compiler.Value, error) {
	if args[0].IsAtom() {
		return compiler.True, nil
	}
	return compiler.Nil, nil
}

func NativeIsNil(args []compiler.Value) (compiler.Value, error) {
	if compiler.IsTruthy(args[0]) {
		return compiler.Nil, nil
	}
	return compiler.True, nil
}

func NativeIsPair(args []compiler.Value) (compiler.Value, error) {
	if args[0].IsPair() {
		return compiler.True, nil
	}
	return compiler.Nil, nil
}

func NativeSetCar(args []compiler.Value) (compiler.Value, error) {
	pair, ok := args[0].AsPair()
	if !ok {
		return compiler.Nil, errors.New("Attempting to set the car of a non-pair")
	}
	pair.Car = args[1]
	return compiler.Nil, nil
}

func NativeSetCdr(args []compiler.Value) (compiler.Value, error) {
	pair, ok := args[0].AsPair()
	if !ok {
		return compiler.Nil, errors.New("Attempting to set the cdr of a non-pair")
	}
	pair.Cdr = args[1]
	return compiler.Nil, nil
}

func NativeDisplay(args []compiler.Value) (compiler.Value, error) {
	fmt.Println(args[0].String())
	return compiler.Nil, nil
}

func NativeNewline([]compiler.Value) (compiler.Value, error) {
	fmt.Println()
	return compiler.Nil, nil
}
