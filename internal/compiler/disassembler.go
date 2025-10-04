package compiler

import (
	"fmt"
	"io"
)

type Disassembler struct {
	chunk  Chunk
	w      io.Writer
	ip     int
	opAddr int
}

func (p *Program) Disassemble(w io.Writer) {
	d := &Disassembler{chunk: p.Function.Chunk, w: w}
	d.disassemble("")
}

func (d *Disassembler) readOpCode() OpCode {
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
		case OpConstant:
			idx := int(d.readOpCode())
			constant := d.chunk.Constants[idx]
			d.writeOpf("Constant %v (%d)", constant, idx)
		case OpNil:
			d.writeOpf("Nil")
		case OpCall:
			argCount := int(d.readOpCode())
			d.writeOpf("Call/%d", argCount)
		case OpTailCall:
			argCount := int(d.readOpCode())
			d.writeOpf("TailCall/%d", argCount)
		case OpReturn:
			d.writeOpf("Return")
		case OpGetLocal:
			idx := int(d.readOpCode())
			d.writeOpf("GetLocal (%d)", idx)
		case OpSetLocal:
			idx := int(d.readOpCode())
			d.writeOpf("SetLocal (%d)", idx)
		case OpGetUpvalue:
			idx := int(d.readOpCode())
			d.writeOpf("GetUpvalue (%d)", idx)
		case OpSetUpvalue:
			idx := int(d.readOpCode())
			d.writeOpf("SetUpvalue (%d)", idx)
		case OpGetGlobal:
			idx := int(d.readOpCode())
			atom, _ := d.chunk.Constants[idx].AsAtom()
			d.writeOpf("GetGlobal %s (%d)", atom.Value(), idx)
		case OpDefineGlobal:
			idx := int(d.readOpCode())
			atom, _ := d.chunk.Constants[idx].AsAtom()
			d.writeOpf("DefineGlobal %s (%d)", atom.Value(), idx)
		case OpSetGlobal:
			idx := int(d.readOpCode())
			atom, _ := d.chunk.Constants[idx].AsAtom()
			d.writeOpf("SetGlobal %s (%d)", atom.Value(), idx)
		case OpClosure:
			idx := int(d.readOpCode())
			function, _ := d.chunk.Constants[idx].AsFunction()
			d.writeOpf("Closure %v (%d)", function, idx)
			funcD := &Disassembler{chunk: function.Chunk, w: d.w}
			funcD.disassemble(function.Name.Value())
		case OpPop:
			d.writeOpf("Pop")
		case OpJump:
			offset := int(d.readOpCode())
			target := d.ip + offset
			d.writeOpf("Jump -> %04x (offset %d)", target, offset)
		case OpJumpIfFalse:
			offset := int(d.readOpCode())
			target := d.ip + offset
			d.writeOpf("JumpIfFalse -> %04x (offset %d)", target, offset)
		case OpJumpIfTrue:
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
