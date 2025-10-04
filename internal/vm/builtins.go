package vm

import (
	"fmt"

	"tomaskala.com/glisp/internal/compiler"
)

// TODO: Check arity - might have to emit the builtin's arity as an opcode. Don't forget that - and / require at least one arg.
func (vm *VM) callBuiltin(builtin compiler.Builtin, argCount int) error {
	switch builtin {
	case compiler.BuiltinCons:
		cdr, car := vm.pop2()
		vm.push(compiler.Cons(car, cdr))
	case compiler.BuiltinCar:
		if pair, ok := vm.pop().AsPair(); ok {
			vm.push(pair.Car)
		} else {
			return vm.runtimeError("car expects a pair")
		}
	case compiler.BuiltinCdr:
		if pair, ok := vm.pop().AsPair(); ok {
			vm.push(pair.Cdr)
		} else {
			return vm.runtimeError("cdr expects a pair")
		}
	case compiler.BuiltinAdd:
		if ok := vm.varNumOp(float64(0), argCount, func(acc, n float64) float64 { return acc + n }); !ok {
			return vm.runtimeError("+ is only defined for numbers")
		}
	case compiler.BuiltinSub:
		if argCount == 1 {
			num, ok := vm.pop().AsNumber()
			if !ok {
				return vm.runtimeError("- is only defined for numbers")
			}
			vm.push(compiler.MakeNumber(-num))
		} else {
			result, ok := vm.stack[len(vm.stack)-argCount].AsNumber()
			if !ok {
				return vm.runtimeError("- is only defined for numbers")
			}
			for _, arg := range vm.stack[len(vm.stack)-argCount+1:] {
				num, ok := arg.AsNumber()
				if !ok {
					return vm.runtimeError("- is only defined for numbers")
				}
				result -= num
			}
			vm.stack = vm.stack[:len(vm.stack)-argCount]
			vm.push(compiler.MakeNumber(result))
		}
	case compiler.BuiltinMul:
		if ok := vm.varNumOp(float64(1), argCount, func(acc, n float64) float64 { return acc * n }); !ok {
			return vm.runtimeError("* is only defined for numbers")
		}
	case compiler.BuiltinDiv:
		if argCount == 1 {
			num, ok := vm.pop().AsNumber()
			if !ok {
				return vm.runtimeError("/ is only defined for numbers")
			}
			if num == 0 {
				return vm.runtimeError("zero division")
			}
			vm.push(compiler.MakeNumber(1 / num))
		} else {
			result, ok := vm.stack[len(vm.stack)-argCount].AsNumber()
			if !ok {
				return vm.runtimeError("/ is only defined for numbers")
			}
			for _, arg := range vm.stack[len(vm.stack)-argCount+1:] {
				num, ok := arg.AsNumber()
				if !ok {
					return vm.runtimeError("/ is only defined for numbers")
				}
				if num == 0 {
					return vm.runtimeError("zero division")
				}
				result /= num
			}
			vm.stack = vm.stack[:len(vm.stack)-argCount]
			vm.push(compiler.MakeNumber(result))
		}
	case compiler.BuiltinNumEq:
		if ok := vm.relNumOp(func(a, b float64) bool { return a == b }); !ok {
			return vm.runtimeError("= is only defined for numbers")
		}
	case compiler.BuiltinNumLt:
		if ok := vm.relNumOp(func(a, b float64) bool { return a < b }); !ok {
			return vm.runtimeError("< is only defined for numbers")
		}
	case compiler.BuiltinNumLte:
		if ok := vm.relNumOp(func(a, b float64) bool { return a <= b }); !ok {
			return vm.runtimeError("<= is only defined for numbers")
		}
	case compiler.BuiltinNumGt:
		if ok := vm.relNumOp(func(a, b float64) bool { return a > b }); !ok {
			return vm.runtimeError("> is only defined for numbers")
		}
	case compiler.BuiltinNumGte:
		if ok := vm.relNumOp(func(a, b float64) bool { return a >= b }); !ok {
			return vm.runtimeError(">= is only defined for numbers")
		}
	case compiler.BuiltinEq:
		r, l := vm.pop2()
		if l == r {
			vm.push(compiler.True)
		} else {
			vm.push(compiler.Nil)
		}
	case compiler.BuiltinIsAtom:
		if vm.pop().IsAtom() {
			vm.push(compiler.True)
		} else {
			vm.push(compiler.Nil)
		}
	case compiler.BuiltinIsNil:
		if isTruthy(vm.pop()) {
			vm.push(compiler.Nil)
		} else {
			vm.push(compiler.True)
		}
	case compiler.BuiltinIsPair:
		if vm.pop().IsPair() {
			vm.push(compiler.True)
		} else {
			vm.push(compiler.Nil)
		}
	case compiler.BuiltinSetCar:
		car := vm.pop()
		pair, ok := vm.pop().AsPair()
		if !ok {
			return vm.runtimeError("set-car! is only defined for pairs")
		}
		pair.Car = car
		vm.push(compiler.Nil)
	case compiler.BuiltinSetCdr:
		cdr := vm.pop()
		pair, ok := vm.pop().AsPair()
		if !ok {
			return vm.runtimeError("set-cdr! is only defined for pairs")
		}
		pair.Cdr = cdr
		vm.push(compiler.Nil)
	case compiler.BuiltinDisplay:
		fmt.Println(vm.pop())
		vm.push(compiler.Nil)
	case compiler.BuiltinNewline:
		fmt.Println()
		vm.push(compiler.Nil)
	default:
		return vm.runtimeError("Unknown builtin: %v", builtin)
	}
	return nil
}

func (vm *VM) varNumOp(init float64, argCount int, op func(float64, float64) float64) bool {
	result := init
	for _, arg := range vm.stack[len(vm.stack)-argCount:] {
		num, ok := arg.AsNumber()
		if !ok {
			return false
		}
		result = op(result, num)
	}
	vm.stack = vm.stack[:len(vm.stack)-argCount]
	vm.push(compiler.MakeNumber(result))
	return true
}

func (vm *VM) relNumOp(op func(float64, float64) bool) bool {
	r, l := vm.pop2()
	rNum, ok2 := r.AsNumber()
	lNum, ok1 := l.AsNumber()
	if !ok1 || !ok2 {
		return false
	}
	if op(lNum, rNum) {
		vm.push(compiler.True)
	} else {
		vm.push(compiler.Nil)
	}
	return true
}
