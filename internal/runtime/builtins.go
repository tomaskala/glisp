package runtime

import (
	"errors"
	"fmt"
)

// varNumOp applies the given binary operation as a reducer
// while assuming that at least one argument was provided.
func varNumOp(args []Value, op func(float64, float64) float64) (Value, bool) {
	if !args[0].IsNumber() {
		return Nil, false
	}
	result := args[0].AsNumber()
	for i := 1; i < len(args); i++ {
		if !args[i].IsNumber() {
			return Nil, false
		}
		result = op(result, args[i].AsNumber())
	}
	return MakeNumber(result), true
}

func relNumOp(args []Value, op func(float64, float64) bool) (Value, bool) {
	if !args[0].IsNumber() || !args[1].IsNumber() {
		return Nil, false
	}
	if op(args[0].AsNumber(), args[1].AsNumber()) {
		return True, true
	}
	return Nil, true
}

func builtinCons(args []Value) (Value, error) {
	car := args[0]
	cdr := args[1]
	return Cons(car, cdr), nil
}

func builtinCar(args []Value) (Value, error) {
	if !args[0].IsPair() {
		return Nil, errors.New("car expects a pair")
	}
	return args[0].AsPair().Car, nil
}

func builtinCdr(args []Value) (Value, error) {
	if !args[0].IsPair() {
		return Nil, errors.New("cdr expects a pair")
	}
	return args[0].AsPair().Cdr, nil
}

func builtinAdd(args []Value) (Value, error) {
	result, ok := varNumOp(args, func(acc, n float64) float64 { return acc + n })
	if !ok {
		return Nil, errors.New("+ is only defined for numbers")
	}
	return result, nil
}

func builtinSub(args []Value) (Value, error) {
	if len(args) == 1 {
		if !args[0].IsNumber() {
			return Nil, errors.New("- is only defined for numbers")
		}
		return MakeNumber(-args[0].AsNumber()), nil
	}
	result, ok := varNumOp(args, func(acc, n float64) float64 { return acc - n })
	if !ok {
		return Nil, errors.New("- is only defined for numbers")
	}
	return result, nil
}

func builtinMul(args []Value) (Value, error) {
	result, ok := varNumOp(args, func(acc, n float64) float64 { return acc * n })
	if !ok {
		return Nil, errors.New("* is only defined for numbers")
	}
	return result, nil
}

func builtinDiv(args []Value) (Value, error) {
	if len(args) == 1 {
		if !args[0].IsNumber() {
			return Nil, errors.New("/ is only defined for numbers")
		}
		return MakeNumber(1 / args[0].AsNumber()), nil
	}
	result, ok := varNumOp(args, func(acc, n float64) float64 { return acc / n })
	if !ok {
		return Nil, errors.New("/ is only defined for numbers")
	}
	return result, nil
}

func builtinNumEq(args []Value) (Value, error) {
	result, ok := relNumOp(args, func(a, b float64) bool { return a == b })
	if !ok {
		return Nil, errors.New("= is only defined for numbers")
	}
	return result, nil
}

func builtinNumLt(args []Value) (Value, error) {
	result, ok := relNumOp(args, func(a, b float64) bool { return a < b })
	if !ok {
		return Nil, errors.New("< is only defined for numbers")
	}
	return result, nil
}

func builtinNumLte(args []Value) (Value, error) {
	result, ok := relNumOp(args, func(a, b float64) bool { return a <= b })
	if !ok {
		return Nil, errors.New("<= is only defined for numbers")
	}
	return result, nil
}

func builtinNumGt(args []Value) (Value, error) {
	result, ok := relNumOp(args, func(a, b float64) bool { return a > b })
	if !ok {
		return Nil, errors.New("> is only defined for numbers")
	}
	return result, nil
}

func builtinNumGte(args []Value) (Value, error) {
	result, ok := relNumOp(args, func(a, b float64) bool { return a >= b })
	if !ok {
		return Nil, errors.New(">= is only defined for numbers")
	}
	return result, nil
}

func builtinEq(args []Value) (Value, error) {
	if args[0] == args[1] {
		return True, nil
	}
	return Nil, nil
}

func builtinIsAtom(args []Value) (Value, error) {
	if args[0].IsAtom() {
		return True, nil
	}
	return Nil, nil
}

func builtinIsNil(args []Value) (Value, error) {
	if IsTruthy(args[0]) {
		return Nil, nil
	}
	return True, nil
}

func builtinIsPair(args []Value) (Value, error) {
	if args[0].IsPair() {
		return True, nil
	}
	return Nil, nil
}

func builtinSetCar(args []Value) (Value, error) {
	if !args[0].IsPair() {
		return Nil, errors.New("set-car! expects a pair")
	}
	car := args[1]
	args[0].AsPair().Car = car
	return Nil, nil
}

func builtinSetCdr(args []Value) (Value, error) {
	if !args[0].IsPair() {
		return Nil, errors.New("set-cdr! expects a pair")
	}
	cdr := args[1]
	args[0].AsPair().Cdr = cdr
	return Nil, nil
}

func builtinDisplay(args []Value) (Value, error) {
	fmt.Println(args[0])
	return Nil, nil
}

func builtinNewline([]Value) (Value, error) {
	fmt.Println()
	return Nil, nil
}

func LoadBuiltins() map[Atom]Value {
	return map[Atom]Value{
		NewAtom("#t"):       True,
		NewAtom("cons"):     MakeBuiltin(&Builtin{NewAtom("cons"), 2, builtinCons}),
		NewAtom("car"):      MakeBuiltin(&Builtin{NewAtom("car"), 1, builtinCar}),
		NewAtom("cdr"):      MakeBuiltin(&Builtin{NewAtom("cdr"), 1, builtinCdr}),
		NewAtom("+"):        MakeBuiltin(&Builtin{NewAtom("+"), -1, builtinAdd}),
		NewAtom("-"):        MakeBuiltin(&Builtin{NewAtom("-"), -1, builtinSub}),
		NewAtom("*"):        MakeBuiltin(&Builtin{NewAtom("*"), -1, builtinMul}),
		NewAtom("/"):        MakeBuiltin(&Builtin{NewAtom("/"), -1, builtinDiv}),
		NewAtom("="):        MakeBuiltin(&Builtin{NewAtom("="), 2, builtinNumEq}),
		NewAtom("<"):        MakeBuiltin(&Builtin{NewAtom("<"), 2, builtinNumLt}),
		NewAtom("<="):       MakeBuiltin(&Builtin{NewAtom("<="), 2, builtinNumLte}),
		NewAtom(">"):        MakeBuiltin(&Builtin{NewAtom(">"), 2, builtinNumGt}),
		NewAtom(">="):       MakeBuiltin(&Builtin{NewAtom(">="), 2, builtinNumGte}),
		NewAtom("eq?"):      MakeBuiltin(&Builtin{NewAtom("eq?"), 2, builtinEq}),
		NewAtom("atom?"):    MakeBuiltin(&Builtin{NewAtom("atom?"), 1, builtinIsAtom}),
		NewAtom("nil?"):     MakeBuiltin(&Builtin{NewAtom("nil?"), 1, builtinIsNil}),
		NewAtom("not"):      MakeBuiltin(&Builtin{NewAtom("not"), 1, builtinIsNil}),
		NewAtom("pair?"):    MakeBuiltin(&Builtin{NewAtom("pair?"), 1, builtinIsPair}),
		NewAtom("set-car!"): MakeBuiltin(&Builtin{NewAtom("set-car!"), 2, builtinSetCar}),
		NewAtom("set-cdr!"): MakeBuiltin(&Builtin{NewAtom("set-cdr!"), 2, builtinSetCdr}),
		NewAtom("display"):  MakeBuiltin(&Builtin{NewAtom("display"), 1, builtinDisplay}),
		NewAtom("newline"):  MakeBuiltin(&Builtin{NewAtom("newline"), 0, builtinNewline}),
	}
}
