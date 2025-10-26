package runtime

import "math"

type Chunk struct {
	Code      []OpCode
	Constants []Value
	Lines     []int
}

func (c *Chunk) Write(code OpCode, line int) {
	c.Code = append(c.Code, code)
	c.Lines = append(c.Lines, line)
}

func (c *Chunk) AddConstant(constant Value) int {
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
	OpApply
	OpTailApply
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
