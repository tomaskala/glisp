package compiler

import (
	"math"

	"tomaskala.com/glisp/internal/tokenizer"
)

type Chunk struct {
	Code      []OpCode
	Constants []Value
	Positions []tokenizer.Position
}

func (c *Chunk) write(code OpCode, pos tokenizer.Position) {
	c.Code = append(c.Code, code)
	c.Positions = append(c.Positions, pos)
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
	OpReturn
	OpGetLocal
	OpGetUpvalue
	OpGetGlobal
	OpSetLocal
	OpSetGlobal
	OpClosure
	OpCloseUpvalue
	OpPop
)
