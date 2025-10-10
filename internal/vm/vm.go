package vm

import (
	"fmt"
	"slices"
	"strconv"
	"strings"

	"tomaskala.com/glisp/internal/runtime"
)

const (
	framesMax = 1024
	stackInit = 8192
)

type RuntimeError struct {
	Message string
}

func (e *RuntimeError) Error() string {
	return e.Message
}

type Frame struct {
	closure *runtime.Closure // The currently evaluated closure.
	ip      int              // Instruction pointer into closure.Function.Chunk.Code.
	base    int              // Base index inside the VM's stack where this frame's locals start.
}

type VM struct {
	numFrames int
	frames    [framesMax]Frame

	stackTop int
	stack    []runtime.Value

	openUpvalues []*runtime.Upvalue

	globals map[runtime.Atom]runtime.Value
}

func NewVM() *VM {
	stack := make([]runtime.Value, 0, stackInit)
	globals := runtime.LoadBuiltins()
	return &VM{stack: stack, globals: globals}
}

func (vm *VM) push(v runtime.Value) {
	stackTop := vm.stackTop
	if stackTop >= len(vm.stack) {
		vm.stack = append(vm.stack, runtime.Nil)
	}
	vm.stack[stackTop] = v
	stackTop++
	vm.stackTop = stackTop
}

func (vm *VM) peek(dist int) runtime.Value {
	return vm.stack[vm.stackTop-1-dist]
}

func (vm *VM) pop() runtime.Value {
	vm.stackTop--
	return vm.stack[vm.stackTop]
}

func (vm *VM) pushFrame(closure *runtime.Closure, base int) error {
	if vm.numFrames == framesMax {
		return vm.runtimeError("stack overflow")
	}
	vm.frames[vm.numFrames] = Frame{closure: closure, base: base}
	vm.numFrames++
	return nil
}
func (vm *VM) peekFrame() *Frame { return &vm.frames[vm.numFrames-1] }
func (vm *VM) popFrame()         { vm.numFrames-- }

func (vm *VM) runtimeError(format string, args ...any) error {
	var builder strings.Builder
	builder.WriteString(fmt.Sprintf(format, args...))
	builder.WriteByte('\n')
	for _, frame := range slices.Backward(vm.frames[:vm.numFrames]) {
		function := frame.closure.Function
		builder.WriteString("  in ")
		builder.WriteString(function.Name.Value())
		builder.WriteString(" (line ")
		builder.WriteString(strconv.Itoa(function.Chunk.Lines[frame.ip-1]))
		builder.WriteString(")\n")
	}
	vm.numFrames = 0
	vm.stackTop = 0
	vm.openUpvalues = vm.openUpvalues[:0]
	return &RuntimeError{builder.String()}
}

func (vm *VM) captureUpvalue(idx int) *runtime.Upvalue {
	for _, uv := range vm.openUpvalues {
		if !uv.IsClosed && uv.Index == idx {
			return uv
		}
	}
	uv := &runtime.Upvalue{Index: idx}
	vm.openUpvalues = append(vm.openUpvalues, uv)
	return uv
}

func (vm *VM) closeUpvalues(last int) {
	remaining := vm.openUpvalues[:0]
	for _, uv := range vm.openUpvalues {
		if !uv.IsClosed && uv.Index >= last {
			uv.IsClosed = true
			uv.ClosedVal = vm.stack[uv.Index]
			uv.Index = -1
		} else {
			remaining = append(remaining, uv)
		}
	}
	vm.openUpvalues = remaining
}

func (vm *VM) checkArgs(closure *runtime.Closure, argCount int) error {
	function := closure.Function
	if !function.HasRestParam && argCount != function.Arity {
		return vm.runtimeError("%s expects %d arguments, got %d", function.Name.Value(), function.Arity, argCount)
	}
	if function.HasRestParam && argCount < function.Arity {
		return vm.runtimeError(
			"%s expects at least %d arguments, got %d",
			function.Name.Value(),
			function.Arity,
			argCount,
		)
	}
	return nil
}

func (vm *VM) callValue(callee runtime.Value, argCount int) error {
	if builtin, ok := callee.AsBuiltin(); ok {
		return vm.callBuiltin(builtin, argCount)
	}
	if closure, ok := callee.AsClosure(); ok {
		if err := vm.checkArgs(closure, argCount); err != nil {
			return err
		}
		return vm.callClosure(closure, argCount)
	}
	return vm.runtimeError("attempting to call %v", callee)
}

func (vm *VM) callBuiltin(builtin *runtime.Builtin, argCount int) error {
	if builtin.Arity >= 0 && builtin.Arity != argCount {
		return vm.runtimeError("%s expects %d arguments, got %d", builtin.Name.Value(), builtin.Arity, argCount)
	} else if builtin.Arity == -1 && argCount == 0 {
		return vm.runtimeError("%s expects at least 1 argument", builtin.Name.Value())
	}
	base := vm.stackTop - argCount
	result, err := builtin.Function(vm.stack[base : base+argCount])
	if err != nil {
		return vm.runtimeError("%s", err.Error())
	}
	vm.stackTop = base - 1
	vm.push(result)
	return nil
}

func (vm *VM) callClosure(closure *runtime.Closure, argCount int) error {
	function := closure.Function
	base := vm.stackTop - argCount
	if function.HasRestParam {
		restArgs := runtime.Nil
		for i := argCount - 1; i >= function.Arity; i-- {
			arg := vm.stack[base+i]
			restArgs = runtime.Cons(arg, restArgs)
		}
		vm.stackTop = base + function.Arity
		vm.push(restArgs)
	}
	return vm.pushFrame(closure, base)
}

func (vm *VM) tailCallValue(callee runtime.Value, argCount int) error {
	if builtin, ok := callee.AsBuiltin(); ok {
		return vm.callBuiltin(builtin, argCount)
	}
	if closure, ok := callee.AsClosure(); ok {
		if err := vm.checkArgs(closure, argCount); err != nil {
			return err
		}
		vm.tailCallClosure(closure, argCount)
		return nil
	}
	return vm.runtimeError("attempting to call %v", callee)
}

func (vm *VM) tailCallClosure(closure *runtime.Closure, argCount int) {
	function := closure.Function
	currentFrame := vm.peekFrame()
	newArgCount := argCount
	if function.HasRestParam {
		base := vm.stackTop - argCount
		restArgs := runtime.Nil
		for i := argCount - 1; i >= function.Arity; i-- {
			arg := vm.stack[base+i]
			restArgs = runtime.Cons(arg, restArgs)
		}
		vm.stackTop = base + function.Arity
		vm.push(restArgs)
		newArgCount = function.Arity + 1
	}
	vm.closeUpvalues(currentFrame.base)
	newBase := currentFrame.base
	oldStackTop := vm.stackTop
	newArgsStart := oldStackTop - newArgCount
	for i := range newArgCount {
		vm.stack[newBase+i] = vm.stack[newArgsStart+i]
	}
	vm.stackTop = newBase + newArgCount
	currentFrame.closure = closure
	currentFrame.ip = 0
}

func readOpCode(frame *Frame) runtime.OpCode {
	code := frame.closure.Function.Chunk.Code[frame.ip]
	frame.ip++
	return code
}

func readConstant(frame *Frame) runtime.Value {
	idx := int(readOpCode(frame))
	return frame.closure.Function.Chunk.Constants[idx]
}

func readAtom(frame *Frame) runtime.Atom {
	atom, _ := readConstant(frame).AsAtom()
	return atom
}

func readFunction(frame *Frame) *runtime.Function {
	f, _ := readConstant(frame).AsFunction()
	return f
}

func (vm *VM) Run(program *runtime.Program) (runtime.Value, error) {
	vm.pushFrame(&runtime.Closure{Function: program.Function}, 0)
	for {
		frame := vm.peekFrame()
		opcode := readOpCode(frame)
		switch opcode {
		case runtime.OpConstant:
			constant := readConstant(frame)
			vm.push(constant)
		case runtime.OpNil:
			vm.push(runtime.Nil)
		case runtime.OpCall:
			argCount := int(readOpCode(frame))
			callee := vm.peek(argCount)
			if err := vm.callValue(callee, argCount); err != nil {
				return runtime.Nil, err
			}
		case runtime.OpTailCall:
			argCount := int(readOpCode(frame))
			callee := vm.peek(argCount)
			if err := vm.tailCallValue(callee, argCount); err != nil {
				return runtime.Nil, err
			}
		case runtime.OpReturn:
			result := vm.pop()
			vm.closeUpvalues(frame.base)
			vm.popFrame()
			if vm.numFrames == 0 {
				return result, nil
			}
			vm.stackTop = frame.base - 1
			vm.push(result)
		case runtime.OpGetLocal:
			slot := int(readOpCode(frame))
			vm.push(vm.stack[frame.base+slot])
		case runtime.OpSetLocal:
			slot := int(readOpCode(frame))
			vm.stack[frame.base+slot] = vm.pop()
			vm.push(runtime.Nil)
		case runtime.OpGetUpvalue:
			slot := int(readOpCode(frame))
			upvalue := frame.closure.Upvalues[slot]
			if upvalue.IsClosed {
				vm.push(upvalue.ClosedVal)
			} else {
				vm.push(vm.stack[upvalue.Index])
			}
		case runtime.OpSetUpvalue:
			slot := int(readOpCode(frame))
			upvalue := frame.closure.Upvalues[slot]
			if upvalue.IsClosed {
				upvalue.ClosedVal = vm.pop()
			} else {
				vm.stack[upvalue.Index] = vm.pop()
			}
			vm.push(runtime.Nil)
		case runtime.OpGetGlobal:
			atom := readAtom(frame)
			value, ok := vm.globals[atom]
			if !ok {
				return runtime.Nil, vm.runtimeError("undefined variable: %s", atom.Value())
			}
			vm.push(value)
		case runtime.OpDefineGlobal:
			name := readAtom(frame)
			vm.globals[name] = vm.pop()
		case runtime.OpSetGlobal:
			atom := readAtom(frame)
			if _, ok := vm.globals[atom]; !ok {
				return runtime.Nil, vm.runtimeError("attempting to set an undefined variable: %s", atom.Value())
			}
			vm.globals[atom] = vm.pop()
			vm.push(runtime.Nil)
		case runtime.OpClosure:
			function := readFunction(frame)
			closure := &runtime.Closure{
				Function: function,
				Upvalues: make([]*runtime.Upvalue, len(function.Upvalues)),
			}
			for i, uv := range function.Upvalues {
				if uv.IsLocal {
					closure.Upvalues[i] = vm.captureUpvalue(frame.base + uv.Index)
				} else {
					closure.Upvalues[i] = frame.closure.Upvalues[uv.Index]
				}
			}
			vm.push(runtime.MakeClosure(closure))
		case runtime.OpPop:
			vm.pop()
		case runtime.OpJump:
			offset := int(readOpCode(frame))
			frame.ip += offset
		case runtime.OpJumpIfFalse:
			offset := int(readOpCode(frame))
			if !runtime.IsTruthy(vm.peek(0)) {
				frame.ip += offset
			}
		case runtime.OpJumpIfTrue:
			offset := int(readOpCode(frame))
			if runtime.IsTruthy(vm.peek(0)) {
				frame.ip += offset
			}
		default:
			return runtime.Nil, vm.runtimeError("unknown opcode: %v", opcode)
		}
	}
}
