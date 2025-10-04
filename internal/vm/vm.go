package vm

import (
	"fmt"
	"slices"
	"strconv"
	"strings"

	"tomaskala.com/glisp/internal/compiler"
)

const framesMax = 1024

type RuntimeError struct {
	Message string
}

func (e *RuntimeError) Error() string {
	return e.Message
}

type Frame struct {
	closure *compiler.Closure // The currently evaluated closure.
	ip      int               // Instruction pointer into closure.Function.Chunk.Code.
	base    int               // Base index inside the VM's stack where this frame's locals start.
}

type VM struct {
	numFrames    int
	frames       [framesMax]Frame
	stack        []compiler.Value
	openUpvalues []*compiler.Upvalue
	globals      map[compiler.Atom]compiler.Value
}

func NewVM() *VM {
	return &VM{globals: loadNatives()}
}

func (vm *VM) push(v compiler.Value)        { vm.stack = append(vm.stack, v) }
func (vm *VM) peek(dist int) compiler.Value { return vm.stack[len(vm.stack)-1-dist] }
func (vm *VM) pop() compiler.Value {
	v := vm.stack[len(vm.stack)-1]
	vm.stack = vm.stack[:len(vm.stack)-1]
	return v
}

func (vm *VM) pushFrame(closure *compiler.Closure, base int) error {
	if vm.numFrames == framesMax {
		return vm.runtimeError("Stack overflow")
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
	vm.stack = vm.stack[:0]
	vm.openUpvalues = vm.openUpvalues[:0]
	return &RuntimeError{builder.String()}
}

func (vm *VM) captureUpvalue(idx int) *compiler.Upvalue {
	for _, uv := range vm.openUpvalues {
		if !uv.IsClosed && uv.Index == idx {
			return uv
		}
	}
	uv := &compiler.Upvalue{Index: idx}
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

func (vm *VM) checkArgs(closure *compiler.Closure, argCount int) error {
	function := closure.Function
	if !function.HasRestParam && argCount != function.Arity {
		return vm.runtimeError("Function %s expects %d arguments, got %d", function.Name.Value(), function.Arity, argCount)
	}
	if function.HasRestParam && argCount < function.Arity {
		return vm.runtimeError("Function %s expects at least %d arguments, got %d", function.Name.Value(), function.Arity, argCount)
	}
	return nil
}

func (vm *VM) callValue(callee compiler.Value, argCount int) error {
	if f, ok := callee.AsNative(); ok {
		if f.Arity >= 0 && f.Arity != argCount {
			return vm.runtimeError("%s expects %d arguments, got %d", f.Name.Value(), f.Arity, argCount)
		}
		result, err := f.Func(vm.stack[len(vm.stack)-argCount:])
		if err != nil {
			return vm.runtimeError("%s", err.Error())
		}
		vm.stack = vm.stack[:len(vm.stack)-argCount-1]
		vm.push(result)
		return nil
	}
	if f, ok := callee.AsClosure(); ok {
		if err := vm.checkArgs(f, argCount); err != nil {
			return err
		}
		return vm.callClosure(f, argCount)
	}
	return vm.runtimeError("Attempting to call %v", callee)
}

func (vm *VM) tailCallValue(callee compiler.Value, argCount int) error {
	if ok := callee.IsNative(); ok {
		return vm.callValue(callee, argCount)
	}
	if f, ok := callee.AsClosure(); ok {
		if err := vm.checkArgs(f, argCount); err != nil {
			return err
		}
		vm.tailCallClosure(f, argCount)
		return nil
	}
	return vm.runtimeError("Attempting to call %v", callee)
}

func (vm *VM) callClosure(closure *compiler.Closure, argCount int) error {
	function := closure.Function
	base := len(vm.stack) - argCount
	if function.HasRestParam {
		var restArgs compiler.Value = compiler.Nil
		for i := argCount - 1; i >= function.Arity; i-- {
			arg := vm.stack[base+i]
			restArgs = compiler.Cons(arg, restArgs)
		}
		vm.stack = vm.stack[:base+function.Arity]
		vm.push(restArgs)
	}
	return vm.pushFrame(closure, base)
}

func (vm *VM) tailCallClosure(closure *compiler.Closure, argCount int) {
	function := closure.Function
	currentFrame := vm.peekFrame()
	newArgCount := argCount
	if function.HasRestParam {
		base := len(vm.stack) - argCount
		var restArgs compiler.Value = compiler.Nil
		for i := argCount - 1; i >= function.Arity; i-- {
			arg := vm.stack[base+i]
			restArgs = compiler.Cons(arg, restArgs)
		}
		vm.stack = vm.stack[:base+function.Arity]
		vm.push(restArgs)
		newArgCount = function.Arity + 1
	}
	vm.closeUpvalues(currentFrame.base)
	newBase := currentFrame.base
	oldStackTop := len(vm.stack)
	newArgsStart := oldStackTop - newArgCount
	for i := 0; i < newArgCount; i++ {
		vm.stack[newBase+i] = vm.stack[newArgsStart+i]
	}
	vm.stack = vm.stack[:newBase+newArgCount]
	currentFrame.closure = closure
	currentFrame.ip = 0
}

func readOpCode(frame *Frame) compiler.OpCode {
	code := frame.closure.Function.Chunk.Code[frame.ip]
	frame.ip++
	return code
}

func readConstant(frame *Frame) compiler.Value {
	idx := int(readOpCode(frame))
	return frame.closure.Function.Chunk.Constants[idx]
}

func readAtom(frame *Frame) compiler.Atom {
	atom, _ := readConstant(frame).AsAtom()
	return atom
}

func readFunction(frame *Frame) *compiler.Function {
	f, _ := readConstant(frame).AsFunction()
	return f
}

func (vm *VM) Run(program *compiler.Program) (compiler.Value, error) {
	closure := &compiler.Closure{Function: program.Function}
	vm.pushFrame(closure, 0)
	for {
		frame := vm.peekFrame()
		opcode := readOpCode(frame)
		switch opcode {
		case compiler.OpConstant:
			constant := readConstant(frame)
			vm.push(constant)
		case compiler.OpNil:
			vm.push(compiler.Nil)
		case compiler.OpCall:
			argCount := int(readOpCode(frame))
			callee := vm.peek(argCount)
			if err := vm.callValue(callee, argCount); err != nil {
				return compiler.Nil, err
			}
		case compiler.OpTailCall:
			argCount := int(readOpCode(frame))
			callee := vm.peek(argCount)
			if err := vm.tailCallValue(callee, argCount); err != nil {
				return compiler.Nil, err
			}
		case compiler.OpReturn:
			result := vm.pop()
			vm.closeUpvalues(frame.base)
			vm.popFrame()
			if vm.numFrames == 0 {
				return result, nil
			}
			vm.stack = vm.stack[:frame.base-1]
			vm.push(result)
		case compiler.OpGetLocal:
			slot := int(readOpCode(frame))
			vm.push(vm.stack[frame.base+slot])
		case compiler.OpSetLocal:
			slot := int(readOpCode(frame))
			vm.stack[frame.base+slot] = vm.pop()
			vm.push(compiler.Nil)
		case compiler.OpGetUpvalue:
			slot := int(readOpCode(frame))
			upvalue := frame.closure.Upvalues[slot]
			if upvalue.IsClosed {
				vm.push(upvalue.ClosedVal)
			} else {
				vm.push(vm.stack[upvalue.Index])
			}
		case compiler.OpSetUpvalue:
			slot := int(readOpCode(frame))
			upvalue := frame.closure.Upvalues[slot]
			if upvalue.IsClosed {
				upvalue.ClosedVal = vm.pop()
			} else {
				vm.stack[upvalue.Index] = vm.pop()
			}
			vm.push(compiler.Nil)
		case compiler.OpGetGlobal:
			atom := readAtom(frame)
			value, ok := vm.globals[atom]
			if !ok {
				return compiler.Nil, vm.runtimeError("Undefined variable: %s", atom.Value())
			}
			vm.push(value)
		case compiler.OpDefineGlobal:
			name := readAtom(frame)
			vm.globals[name] = vm.pop()
		case compiler.OpSetGlobal:
			atom := readAtom(frame)
			if _, ok := vm.globals[atom]; !ok {
				return compiler.Nil, vm.runtimeError("Attempting to set an undefined variable: %s", atom.Value())
			}
			vm.globals[atom] = vm.pop()
			vm.push(compiler.Nil)
		case compiler.OpClosure:
			function := readFunction(frame)
			closure := &compiler.Closure{
				Function: function,
				Upvalues: make([]*compiler.Upvalue, len(function.Upvalues)),
			}
			for i, uv := range function.Upvalues {
				if uv.IsLocal {
					closure.Upvalues[i] = vm.captureUpvalue(frame.base + uv.Index)
				} else {
					closure.Upvalues[i] = frame.closure.Upvalues[uv.Index]
				}
			}
			vm.push(compiler.MakeClosure(closure))
		case compiler.OpPop:
			vm.pop()
		case compiler.OpJump:
			offset := int(readOpCode(frame))
			frame.ip += offset
		case compiler.OpJumpIfFalse:
			offset := int(readOpCode(frame))
			if !compiler.IsTruthy(vm.peek(0)) {
				frame.ip += offset
			}
		case compiler.OpJumpIfTrue:
			offset := int(readOpCode(frame))
			if compiler.IsTruthy(vm.peek(0)) {
				frame.ip += offset
			}
		default:
			return compiler.Nil, vm.runtimeError("Unknown opcode: %v", opcode)
		}
	}
}
