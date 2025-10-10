package compiler

import (
	"fmt"
	"io"

	"tomaskala.com/glisp/internal/runtime"
)

type Disassembler struct {
	chunk  runtime.Chunk
	w      io.Writer
	ip     int
	opAddr int
}

func Disassemble(p *runtime.Program, w io.Writer) {
	d := &Disassembler{chunk: p.Function.Chunk, w: w}
	d.disassemble("")
}

func (d *Disassembler) readOpCode() runtime.OpCode {
	op := d.chunk.Code[d.ip]
	d.ip++
	return op
}

func (d *Disassembler) writeF(format string, args ...any) {
	fmt.Fprintf(d.w, format, args...)
}

func (d *Disassembler) writeOpf(format string, args ...any) {
	addrStr := fmt.Sprintf("%04x", d.opAddr)
	fmt.Fprintf(d.w, addrStr+"    "+format+"\n", args...)
}

func (d *Disassembler) disassemble(prefix string) {
	if prefix != "" {
		d.writeF("        // %s begin\n", prefix)
	}
	for d.ip < len(d.chunk.Code) {
		d.opAddr = d.ip
		switch op := d.readOpCode(); op {
		case runtime.OpConstant:
			idx := int(d.readOpCode())
			constant := d.chunk.Constants[idx]
			d.writeOpf("Constant %v (%d)", constant, idx)
		case runtime.OpNil:
			d.writeOpf("Nil")
		case runtime.OpCall:
			argCount := int(d.readOpCode())
			d.writeOpf("Call/%d", argCount)
		case runtime.OpTailCall:
			argCount := int(d.readOpCode())
			d.writeOpf("TailCall/%d", argCount)
		case runtime.OpCallBuiltin:
			builtin := d.readOpCode()
			d.writeOpf("CallBuiltin (%d)", builtin)
			d.disassembleBuiltin(runtime.Builtin(builtin))
		case runtime.OpReturn:
			d.writeOpf("Return")
		case runtime.OpGetLocal:
			idx := int(d.readOpCode())
			d.writeOpf("GetLocal (%d)", idx)
		case runtime.OpSetLocal:
			idx := int(d.readOpCode())
			d.writeOpf("SetLocal (%d)", idx)
		case runtime.OpGetUpvalue:
			idx := int(d.readOpCode())
			d.writeOpf("GetUpvalue (%d)", idx)
		case runtime.OpSetUpvalue:
			idx := int(d.readOpCode())
			d.writeOpf("SetUpvalue (%d)", idx)
		case runtime.OpGetGlobal:
			idx := int(d.readOpCode())
			atom, _ := d.chunk.Constants[idx].AsAtom()
			d.writeOpf("GetGlobal %s (%d)", atom.Value(), idx)
		case runtime.OpDefineGlobal:
			idx := int(d.readOpCode())
			atom, _ := d.chunk.Constants[idx].AsAtom()
			d.writeOpf("DefineGlobal %s (%d)", atom.Value(), idx)
		case runtime.OpSetGlobal:
			idx := int(d.readOpCode())
			atom, _ := d.chunk.Constants[idx].AsAtom()
			d.writeOpf("SetGlobal %s (%d)", atom.Value(), idx)
		case runtime.OpClosure:
			idx := int(d.readOpCode())
			function, _ := d.chunk.Constants[idx].AsFunction()
			d.writeOpf("Closure %v (%d)", function, idx)
			funcD := &Disassembler{chunk: function.Chunk, w: d.w}
			funcD.disassemble(function.Name.Value())
		case runtime.OpPop:
			d.writeOpf("Pop")
		case runtime.OpJump:
			offset := int(d.readOpCode())
			target := d.ip + offset
			d.writeOpf("Jump -> %04x (offset %d)", target, offset)
		case runtime.OpJumpIfFalse:
			offset := int(d.readOpCode())
			target := d.ip + offset
			d.writeOpf("JumpIfFalse -> %04x (offset %d)", target, offset)
		case runtime.OpJumpIfTrue:
			offset := int(d.readOpCode())
			target := d.ip + offset
			d.writeOpf("JumpIfTrue -> %04x (offset %d)", target, offset)
		default:
			panic(fmt.Sprintf("Unknown opcode: %v", op))
		}
	}
	if prefix != "" {
		d.writeF("        // %s end\n", prefix)
	}
}

func (d *Disassembler) disassembleBuiltin(builtin runtime.Builtin) {
	argCount := int(d.readOpCode())
	switch builtin {
	case runtime.BuiltinCons:
		d.writeOpf("Cons/%d", argCount)
	case runtime.BuiltinCar:
		d.writeOpf("Car/%d", argCount)
	case runtime.BuiltinCdr:
		d.writeOpf("Cdr/%d", argCount)
	case runtime.BuiltinAdd:
		d.writeOpf("Add/%d", argCount)
	case runtime.BuiltinSub:
		d.writeOpf("Sub/%d", argCount)
	case runtime.BuiltinMul:
		d.writeOpf("Mul/%d", argCount)
	case runtime.BuiltinDiv:
		d.writeOpf("Div/%d", argCount)
	case runtime.BuiltinNumEq:
		d.writeOpf("NumEq/%d", argCount)
	case runtime.BuiltinNumLt:
		d.writeOpf("NumLt/%d", argCount)
	case runtime.BuiltinNumLte:
		d.writeOpf("NumLte/%d", argCount)
	case runtime.BuiltinNumGt:
		d.writeOpf("NumGt/%d", argCount)
	case runtime.BuiltinNumGte:
		d.writeOpf("NumGte/%d", argCount)
	case runtime.BuiltinEq:
		d.writeOpf("Eq/%d", argCount)
	case runtime.BuiltinIsAtom:
		d.writeOpf("IsAtom/%d", argCount)
	case runtime.BuiltinIsNil:
		d.writeOpf("IsNil/%d", argCount)
	case runtime.BuiltinIsPair:
		d.writeOpf("IsPair/%d", argCount)
	case runtime.BuiltinSetCar:
		d.writeOpf("SetCar/%d", argCount)
	case runtime.BuiltinSetCdr:
		d.writeOpf("SetCdr/%d", argCount)
	case runtime.BuiltinDisplay:
		d.writeOpf("Display/%d", argCount)
	case runtime.BuiltinNewline:
		d.writeOpf("Newline/%d", argCount)
	default:
		panic(fmt.Sprintf("Unknown builtin: %v", builtin))
	}
}
