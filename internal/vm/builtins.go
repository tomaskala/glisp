package vm

import (
	"fmt"

	"tomaskala.com/glisp/internal/runtime"
)

func (vm *VM) pushBool(b bool) {
	if b {
		vm.push(runtime.True)
	} else {
		vm.push(runtime.Nil)
	}
}

func (vm *VM) pushNumber(n float64) {
	vm.push(runtime.MakeNumber(n))
}

func (vm *VM) callBuiltin(builtin runtime.Builtin, argCount int) error {
	switch builtin {
	case runtime.BuiltinCons:
		cdr, car := vm.pop2()
		vm.push(runtime.Cons(car, cdr))
	case runtime.BuiltinCar:
		if pair, ok := vm.pop().AsPair(); ok {
			vm.push(pair.Car)
		} else {
			return vm.runtimeError("car expects a pair")
		}
	case runtime.BuiltinCdr:
		if pair, ok := vm.pop().AsPair(); ok {
			vm.push(pair.Cdr)
		} else {
			return vm.runtimeError("cdr expects a pair")
		}
	case runtime.BuiltinAdd:
		if ok := vm.varNumOp(argCount, func(acc, n float64) float64 { return acc + n }); !ok {
			return vm.runtimeError("+ is only defined for numbers")
		}
	case runtime.BuiltinSub:
		if argCount == 1 {
			num, ok := vm.pop().AsNumber()
			if !ok {
				return vm.runtimeError("- is only defined for numbers")
			}
			vm.pushNumber(-num)
		} else if ok := vm.varNumOp(argCount, func(acc, n float64) float64 { return acc - n }); !ok {
			return vm.runtimeError("- is only defined for numbers")
		}
	case runtime.BuiltinMul:
		if ok := vm.varNumOp(argCount, func(acc, n float64) float64 { return acc * n }); !ok {
			return vm.runtimeError("* is only defined for numbers")
		}
	case runtime.BuiltinDiv:
		if argCount == 1 {
			num, ok := vm.pop().AsNumber()
			if !ok {
				return vm.runtimeError("/ is only defined for numbers")
			}
			vm.pushNumber(1 / num)
		} else if ok := vm.varNumOp(argCount, func(acc, n float64) float64 { return acc / n }); !ok {
			return vm.runtimeError("/ is only defined for numbers")
		}
	case runtime.BuiltinNumEq:
		if ok := vm.relNumOp(func(a, b float64) bool { return a == b }); !ok {
			return vm.runtimeError("= is only defined for numbers")
		}
	case runtime.BuiltinNumLt:
		if ok := vm.relNumOp(func(a, b float64) bool { return a < b }); !ok {
			return vm.runtimeError("< is only defined for numbers")
		}
	case runtime.BuiltinNumLte:
		if ok := vm.relNumOp(func(a, b float64) bool { return a <= b }); !ok {
			return vm.runtimeError("<= is only defined for numbers")
		}
	case runtime.BuiltinNumGt:
		if ok := vm.relNumOp(func(a, b float64) bool { return a > b }); !ok {
			return vm.runtimeError("> is only defined for numbers")
		}
	case runtime.BuiltinNumGte:
		if ok := vm.relNumOp(func(a, b float64) bool { return a >= b }); !ok {
			return vm.runtimeError(">= is only defined for numbers")
		}
	case runtime.BuiltinEq:
		r, l := vm.pop2()
		vm.pushBool(l == r)
	case runtime.BuiltinIsAtom:
		vm.pushBool(vm.pop().IsAtom())
	case runtime.BuiltinIsNil:
		vm.pushBool(!isTruthy(vm.pop()))
	case runtime.BuiltinIsPair:
		vm.pushBool(vm.pop().IsPair())
	case runtime.BuiltinSetCar:
		car := vm.pop()
		pair, ok := vm.pop().AsPair()
		if !ok {
			return vm.runtimeError("set-car! is only defined for pairs")
		}
		pair.Car = car
		vm.push(runtime.Nil)
	case runtime.BuiltinSetCdr:
		cdr := vm.pop()
		pair, ok := vm.pop().AsPair()
		if !ok {
			return vm.runtimeError("set-cdr! is only defined for pairs")
		}
		pair.Cdr = cdr
		vm.push(runtime.Nil)
	case runtime.BuiltinDisplay:
		fmt.Println(vm.pop())
		vm.push(runtime.Nil)
	case runtime.BuiltinNewline:
		fmt.Println()
		vm.push(runtime.Nil)
	default:
		return vm.runtimeError("unknown builtin: %v", builtin)
	}
	return nil
}

// varNumOp applies the given binary operation as a reducer
// while assuming that at least one argument was provided.
func (vm *VM) varNumOp(argCount int, op func(float64, float64) float64) bool {
	base := vm.stackTop - argCount
	result, ok := vm.stack[base].AsNumber()
	if !ok {
		return false
	}
	for i := base + 1; i < vm.stackTop; i++ {
		num, ok := vm.stack[i].AsNumber()
		if !ok {
			return false
		}
		result = op(result, num)
	}
	vm.stackTop = base
	vm.pushNumber(result)
	return true
}

func (vm *VM) relNumOp(op func(float64, float64) bool) bool {
	r, l := vm.pop2()
	rNum, ok2 := r.AsNumber()
	lNum, ok1 := l.AsNumber()
	if !ok1 || !ok2 {
		return false
	}
	vm.pushBool(op(lNum, rNum))
	return true
}
