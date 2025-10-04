package vm

import (
	"errors"
	"fmt"

	"tomaskala.com/glisp/internal/compiler"
)

func loadNatives() map[string]compiler.Value {
	return map[string]compiler.Value{
		"#t":    compiler.True,
		"cons":  compiler.MakeNative(&compiler.NativeFunction{Name: "cons", Arity: 2, Func: NativeCons}),
		"car":   compiler.MakeNative(&compiler.NativeFunction{Name: "car", Arity: 1, Func: NativeCar}),
		"cdr":   compiler.MakeNative(&compiler.NativeFunction{Name: "cdr", Arity: 1, Func: NativeCdr}),
		"+":     compiler.MakeNative(&compiler.NativeFunction{Name: "+", Arity: -1, Func: NativeAdd}),
		"-":     compiler.MakeNative(&compiler.NativeFunction{Name: "-", Arity: -1, Func: NativeSub}),
		"*":     compiler.MakeNative(&compiler.NativeFunction{Name: "*", Arity: -1, Func: NativeMul}),
		"/":     compiler.MakeNative(&compiler.NativeFunction{Name: "/", Arity: -1, Func: NativeDiv}),
		"=":     compiler.MakeNative(&compiler.NativeFunction{Name: "=", Arity: 2, Func: NativeNumEq}),
		"<":     compiler.MakeNative(&compiler.NativeFunction{Name: "<", Arity: 2, Func: NativeNumLt}),
		"<=":    compiler.MakeNative(&compiler.NativeFunction{Name: "<=", Arity: 2, Func: NativeNumLte}),
		">":     compiler.MakeNative(&compiler.NativeFunction{Name: ">", Arity: 2, Func: NativeNumGt}),
		">=":    compiler.MakeNative(&compiler.NativeFunction{Name: ">=", Arity: 2, Func: NativeNumGte}),
		"eq?":   compiler.MakeNative(&compiler.NativeFunction{Name: "eq?", Arity: 2, Func: NativeEq}),
		"atom?": compiler.MakeNative(&compiler.NativeFunction{Name: "atom?", Arity: 1, Func: NativeIsAtom}),
		"nil?":  compiler.MakeNative(&compiler.NativeFunction{Name: "nil?", Arity: 1, Func: NativeIsNil}),
		"pair?": compiler.MakeNative(&compiler.NativeFunction{Name: "pair?", Arity: 1, Func: NativeIsPair}),
		// Reuse the nil? implementation, since they behave the same way.
		"not":      compiler.MakeNative(&compiler.NativeFunction{Name: "not", Arity: 1, Func: NativeIsNil}),
		"set-car!": compiler.MakeNative(&compiler.NativeFunction{Name: "set-car!", Arity: 2, Func: NativeSetCar}),
		"set-cdr!": compiler.MakeNative(&compiler.NativeFunction{Name: "set-cdr!", Arity: 2, Func: NativeSetCdr}),
		"display":  compiler.MakeNative(&compiler.NativeFunction{Name: "display", Arity: 1, Func: NativeDisplay}),
		"newline":  compiler.MakeNative(&compiler.NativeFunction{Name: "newline", Arity: 0, Func: NativeNewline}),
	}
}

func NativeCons(args []compiler.Value) (compiler.Value, error) {
	car := args[0]
	cdr := args[1]
	return compiler.Cons(car, cdr), nil
}

func NativeCar(args []compiler.Value) (compiler.Value, error) {
	car, ok := compiler.Car(args[0])
	if !ok {
		return compiler.Null, errors.New("car expects a pair")
	}
	return car, nil
}

func NativeCdr(args []compiler.Value) (compiler.Value, error) {
	cdr, ok := compiler.Cdr(args[0])
	if !ok {
		return compiler.Null, errors.New("cdr expects a pair")
	}
	return cdr, nil
}

func NativeAdd(args []compiler.Value) (compiler.Value, error) {
	result := float64(0)
	for _, arg := range args {
		num, ok := arg.AsNumber()
		if !ok {
			return compiler.Null, errors.New("attempting to add non-number")
		}
		result += num
	}
	return compiler.MakeNumber(result), nil
}

func NativeSub(args []compiler.Value) (compiler.Value, error) {
	switch len(args) {
	case 0:
		return compiler.Null, errors.New("- expects at least one argument")
	case 1:
		num, ok := args[0].AsNumber()
		if !ok {
			return compiler.Null, errors.New("attempting to negate non-number")
		}
		return compiler.MakeNumber(-num), nil
	default:
		result, ok := args[0].AsNumber()
		if !ok {
			return compiler.Null, errors.New("attempting to subtract non-number")
		}
		for i := 1; i < len(args); i++ {
			num, ok := args[i].AsNumber()
			if !ok {
				return compiler.Null, errors.New("attempting to subtract non-number")
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
			return compiler.Null, errors.New("attempting to multiply non-number")
		}
		result *= num
	}
	return compiler.MakeNumber(result), nil
}

func NativeDiv(args []compiler.Value) (compiler.Value, error) {
	switch len(args) {
	case 0:
		return compiler.Null, errors.New("/ expects at least one argument")
	case 1:
		num, ok := args[0].AsNumber()
		if !ok {
			return compiler.Null, errors.New("attempting to invert non-number")
		}
		if num == 0 {
			return compiler.Null, errors.New("zero division")
		}
		return compiler.MakeNumber(1 / num), nil
	default:
		result, ok := args[0].AsNumber()
		if !ok {
			return compiler.Null, errors.New("attempting to divide non-number")
		}
		for i := 1; i < len(args); i++ {
			num, ok := args[i].AsNumber()
			if !ok {
				return compiler.Null, errors.New("attempting to divide non-number")
			}
			if num == 0 {
				return compiler.Null, errors.New("zero division")
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
		return compiler.Null, errors.New("= is only defined for numbers")
	}
	if num1 == num2 {
		return compiler.True, nil
	}
	return compiler.Null, nil
}

func NativeNumLt(args []compiler.Value) (compiler.Value, error) {
	num1, ok1 := args[0].AsNumber()
	num2, ok2 := args[1].AsNumber()
	if !ok1 || !ok2 {
		return compiler.Null, errors.New("< is only defined for numbers")
	}
	if num1 < num2 {
		return compiler.True, nil
	}
	return compiler.Null, nil
}

func NativeNumLte(args []compiler.Value) (compiler.Value, error) {
	num1, ok1 := args[0].AsNumber()
	num2, ok2 := args[1].AsNumber()
	if !ok1 || !ok2 {
		return compiler.Null, errors.New("<= is only defined for numbers")
	}
	if num1 <= num2 {
		return compiler.True, nil
	}
	return compiler.Null, nil
}

func NativeNumGt(args []compiler.Value) (compiler.Value, error) {
	num1, ok1 := args[0].AsNumber()
	num2, ok2 := args[1].AsNumber()
	if !ok1 || !ok2 {
		return compiler.Null, errors.New("> is only defined for numbers")
	}
	if num1 > num2 {
		return compiler.True, nil
	}
	return compiler.Null, nil
}

func NativeNumGte(args []compiler.Value) (compiler.Value, error) {
	num1, ok1 := args[0].AsNumber()
	num2, ok2 := args[1].AsNumber()
	if !ok1 || !ok2 {
		return compiler.Null, errors.New(">= is only defined for numbers")
	}
	if num1 >= num2 {
		return compiler.True, nil
	}
	return compiler.Null, nil
}

func NativeEq(args []compiler.Value) (compiler.Value, error) {
	if args[0] == args[1] {
		return compiler.True, nil
	}
	return compiler.Null, nil
}

func NativeIsAtom(args []compiler.Value) (compiler.Value, error) {
	if args[0].IsAtom() {
		return compiler.True, nil
	}
	return compiler.Null, nil
}

func NativeIsNil(args []compiler.Value) (compiler.Value, error) {
	if compiler.IsTruthy(args[0]) {
		return compiler.Null, nil
	}
	return compiler.True, nil
}

func NativeIsPair(args []compiler.Value) (compiler.Value, error) {
	if args[0].IsPair() {
		return compiler.True, nil
	}
	return compiler.Null, nil
}

func NativeSetCar(args []compiler.Value) (compiler.Value, error) {
	pair, ok := args[0].AsPair()
	if !ok {
		return compiler.Null, errors.New("Attempting to set the car of a non-pair")
	}
	pair.Car = args[1]
	return compiler.Null, nil
}

func NativeSetCdr(args []compiler.Value) (compiler.Value, error) {
	pair, ok := args[0].AsPair()
	if !ok {
		return compiler.Null, errors.New("Attempting to set the cdr of a non-pair")
	}
	pair.Cdr = args[1]
	return compiler.Null, nil
}

func NativeDisplay(args []compiler.Value) (compiler.Value, error) {
	fmt.Println(args[0].String())
	return compiler.Null, nil
}

func NativeNewline([]compiler.Value) (compiler.Value, error) {
	fmt.Println()
	return compiler.Null, nil
}
