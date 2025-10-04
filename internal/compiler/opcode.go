package compiler

//go:generate stringer -output opcode_string.go -type OpCode

import "math"

type Chunk struct {
	Code      []OpCode
	Constants []Value
	Lines     []int
}

func (c *Chunk) write(code OpCode, line int) {
	c.Code = append(c.Code, code)
	c.Lines = append(c.Lines, line)
}

func (c *Chunk) addConstant(constant Value) int {
	c.Constants = append(c.Constants, constant)
	return len(c.Constants) - 1
}

type OpCode int32

const (
	OpCodeMin = math.MinInt32
	OpCodeMax = math.MaxInt32
)

const (
	OpConstant OpCode = iota
	OpNil
	OpCall
	OpTailCall
	OpCallBuiltin
	OpReturn
	OpGetLocal
	OpSetLocal
	OpGetUpvalue
	OpSetUpvalue
	OpGetGlobal
	OpDefineGlobal
	OpSetGlobal
	OpClosure
	OpPop
	OpJump
	OpJumpIfFalse
	OpJumpIfTrue
)

type Builtin OpCode

const (
	BuiltinCons Builtin = iota
	BuiltinCar
	BuiltinCdr
	BuiltinAdd
	BuiltinSub
	BuiltinMul
	BuiltinDiv
	BuiltinNumEq
	BuiltinNumLt
	BuiltinNumLte
	BuiltinNumGt
	BuiltinNumGte
	BuiltinEq
	BuiltinIsAtom
	BuiltinIsNil
	BuiltinIsPair
	BuiltinSetCar
	BuiltinSetCdr
	BuiltinDisplay
	BuiltinNewline
)
