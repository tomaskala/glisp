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
	NumFrames int
	frames    [framesMax]Frame

	StackTop int
	Stack    []runtime.Value

	OpenUpvalues []*runtime.Upvalue

	globals map[runtime.Atom]runtime.Value
}

func NewVM() *VM {
	stack := make([]runtime.Value, 0, stackInit)
	globals := runtime.LoadBuiltins()
	return &VM{Stack: stack, globals: globals}
}

func (vm *VM) push(v runtime.Value) {
	stackTop := vm.StackTop
	if stackTop >= len(vm.Stack) {
		vm.Stack = append(vm.Stack, runtime.Nil)
	}
	vm.Stack[stackTop] = v
	stackTop++
	vm.StackTop = stackTop
}

func (vm *VM) peek(dist int) runtime.Value {
	return vm.Stack[vm.StackTop-1-dist]
}

func (vm *VM) pop() runtime.Value {
	vm.StackTop--
	return vm.Stack[vm.StackTop]
}

func (vm *VM) pushFrame(closure *runtime.Closure, base int) error {
	if vm.NumFrames == framesMax {
		return vm.runtimeError("stack overflow")
	}
	vm.frames[vm.NumFrames] = Frame{closure: closure, base: base}
	vm.NumFrames++
	return nil
}
func (vm *VM) peekFrame() *Frame { return &vm.frames[vm.NumFrames-1] }
func (vm *VM) popFrame()         { vm.NumFrames-- }

func (vm *VM) runtimeError(format string, args ...any) error {
	var builder strings.Builder
	builder.WriteString(fmt.Sprintf(format, args...))
	builder.WriteByte('\n')
	for _, frame := range slices.Backward(vm.frames[:vm.NumFrames]) {
		function := frame.closure.Function
		builder.WriteString("  in ")
		builder.WriteString(function.Name.Value())
		builder.WriteString(" (line ")
		builder.WriteString(strconv.Itoa(function.Chunk.Lines[frame.ip-1]))
		builder.WriteString(")\n")
	}
	vm.NumFrames = 0
	vm.StackTop = 0
	vm.OpenUpvalues = vm.OpenUpvalues[:0]
	return &RuntimeError{builder.String()}
}

func (vm *VM) captureUpvalue(idx int) *runtime.Upvalue {
	for _, uv := range vm.OpenUpvalues {
		if !uv.IsClosed && uv.Index == idx {
			return uv
		}
	}
	uv := &runtime.Upvalue{Index: idx}
	vm.OpenUpvalues = append(vm.OpenUpvalues, uv)
	return uv
}

func (vm *VM) closeUpvalues(last int) {
	remaining := vm.OpenUpvalues[:0]
	for _, uv := range vm.OpenUpvalues {
		if !uv.IsClosed && uv.Index >= last {
			uv.IsClosed = true
			uv.ClosedVal = vm.Stack[uv.Index]
			uv.Index = -1
		} else {
			remaining = append(remaining, uv)
		}
	}
	vm.OpenUpvalues = remaining
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
	if callee.IsBuiltin() {
		return vm.callBuiltin(callee.AsBuiltin(), argCount)
	}
	if callee.IsClosure() {
		closure := callee.AsClosure()
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
	base := vm.StackTop - argCount
	result, err := builtin.Function(vm.Stack[base : base+argCount])
	if err != nil {
		return vm.runtimeError("%s", err.Error())
	}
	vm.StackTop = base - 1
	vm.push(result)
	return nil
}

func (vm *VM) callClosure(closure *runtime.Closure, argCount int) error {
	function := closure.Function
	base := vm.StackTop - argCount
	if function.HasRestParam {
		restArgs := runtime.Nil
		for i := argCount - 1; i >= function.Arity; i-- {
			arg := vm.Stack[base+i]
			restArgs = runtime.Cons(arg, restArgs)
		}
		vm.StackTop = base + function.Arity
		vm.push(restArgs)
	}
	return vm.pushFrame(closure, base)
}

func (vm *VM) tailCallValue(callee runtime.Value, argCount int) error {
	if callee.IsBuiltin() {
		return vm.callBuiltin(callee.AsBuiltin(), argCount)
	}
	if callee.IsClosure() {
		closure := callee.AsClosure()
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
		base := vm.StackTop - argCount
		restArgs := runtime.Nil
		for i := argCount - 1; i >= function.Arity; i-- {
			arg := vm.Stack[base+i]
			restArgs = runtime.Cons(arg, restArgs)
		}
		vm.StackTop = base + function.Arity
		vm.push(restArgs)
		newArgCount = function.Arity + 1
	}
	vm.closeUpvalues(currentFrame.base)
	newBase := currentFrame.base
	oldStackTop := vm.StackTop
	newArgsStart := oldStackTop - newArgCount
	for i := range newArgCount {
		vm.Stack[newBase+i] = vm.Stack[newArgsStart+i]
	}
	vm.StackTop = newBase + newArgCount
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
	return readConstant(frame).AsAtom()
}

func readFunction(frame *Frame) *runtime.Function {
	return readConstant(frame).AsFunction()
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
		case runtime.OpApply:
			argCount := int(readOpCode(frame))
			listLen, err := vm.unpackList()
			if err != nil {
				return runtime.Nil, err
			}
			totalArgCount := argCount - 1 + listLen
			callee := vm.peek(totalArgCount)
			if err := vm.callValue(callee, totalArgCount); err != nil {
				return runtime.Nil, err
			}
		case runtime.OpTailApply:
			argCount := int(readOpCode(frame))
			listLen, err := vm.unpackList()
			if err != nil {
				return runtime.Nil, err
			}
			totalArgCount := argCount - 1 + listLen
			callee := vm.peek(totalArgCount)
			if err := vm.tailCallValue(callee, totalArgCount); err != nil {
				return runtime.Nil, err
			}
		case runtime.OpReturn:
			result := vm.pop()
			vm.closeUpvalues(frame.base)
			vm.popFrame()
			if vm.NumFrames == 0 {
				return result, nil
			}
			vm.StackTop = frame.base - 1
			vm.push(result)
		case runtime.OpGetLocal:
			slot := int(readOpCode(frame))
			vm.push(vm.Stack[frame.base+slot])
		case runtime.OpSetLocal:
			slot := int(readOpCode(frame))
			vm.Stack[frame.base+slot] = vm.pop()
			vm.push(runtime.Nil)
		case runtime.OpGetUpvalue:
			slot := int(readOpCode(frame))
			upvalue := frame.closure.Upvalues[slot]
			if upvalue.IsClosed {
				vm.push(upvalue.ClosedVal)
			} else {
				vm.push(vm.Stack[upvalue.Index])
			}
		case runtime.OpSetUpvalue:
			slot := int(readOpCode(frame))
			upvalue := frame.closure.Upvalues[slot]
			if upvalue.IsClosed {
				upvalue.ClosedVal = vm.pop()
			} else {
				vm.Stack[upvalue.Index] = vm.pop()
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

func (vm *VM) unpackList() (int, error) {
	argList := vm.pop()
	listLen := 0
	for argList.IsPair() {
		pair := argList.AsPair()
		vm.push(pair.Car)
		listLen++
		argList = pair.Cdr
	}
	if !argList.IsNil() {
		return 0, vm.runtimeError("apply expects a proper list")
	}
	return listLen, nil
}
