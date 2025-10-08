package vm

import (
	"fmt"

	"tomaskala.com/glisp/internal/compiler"
)

func (vm *VM) pushBool(b bool) {
	if b {
		vm.push(compiler.True)
	} else {
		vm.push(compiler.Nil)
	}
}

func (vm *VM) pushNumber(n float64) {
	vm.push(compiler.MakeNumber(n))
}

// TODO: Check arity - might have to emit the builtin's arity as an opcode.
// TODO: Don't forget that +, -, *, / require at least one arg.
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
		if ok := vm.varNumOp(argCount, func(acc, n float64) float64 { return acc + n }); !ok {
			return vm.runtimeError("+ is only defined for numbers")
		}
	case compiler.BuiltinSub:
		if argCount == 1 {
			num, ok := vm.pop().AsNumber()
			if !ok {
				return vm.runtimeError("- is only defined for numbers")
			}
			vm.pushNumber(-num)
		} else if ok := vm.varNumOp(argCount, func(acc, n float64) float64 { return acc - n }); !ok {
			return vm.runtimeError("- is only defined for numbers")
		}
	case compiler.BuiltinMul:
		if ok := vm.varNumOp(argCount, func(acc, n float64) float64 { return acc * n }); !ok {
			return vm.runtimeError("* is only defined for numbers")
		}
	case compiler.BuiltinDiv:
		if argCount == 1 {
			num, ok := vm.pop().AsNumber()
			if !ok {
				return vm.runtimeError("/ is only defined for numbers")
			}
			vm.pushNumber(1 / num)
		} else if ok := vm.varNumOp(argCount, func(acc, n float64) float64 { return acc / n }); !ok {
			return vm.runtimeError("/ is only defined for numbers")
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
		vm.pushBool(l == r)
	case compiler.BuiltinIsAtom:
		vm.pushBool(vm.pop().IsAtom())
	case compiler.BuiltinIsNil:
		vm.pushBool(!isTruthy(vm.pop()))
	case compiler.BuiltinIsPair:
		vm.pushBool(vm.pop().IsPair())
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
