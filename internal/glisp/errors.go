package glisp

import (
	"fmt"
	"strings"
)

type ParseError struct {
	name  string
	msg   string
	token token
}

func NewParseError(name, msg string, token token) ParseError {
	return ParseError{name, msg, token}
}

func (e ParseError) Error() string {
	return fmt.Sprintf("%s: %s on line %d, token '%s'\n", e.name, e.msg, e.token.line, e.token.val)
}

type RuntimeError struct {
	name   string
	msg    string
	frames []Frame
}

func NewRuntimeError(name, msg string, frames []Frame) RuntimeError {
	return RuntimeError{name, msg, frames}
}

func (e RuntimeError) Error() string {
	var builder strings.Builder
	// TODO: Include the line numbers.
	builder.WriteString(fmt.Sprintf("%s: %s\n", e.name, e.msg))
	for _, frame := range e.frames {
		builder.WriteString(fmt.Sprintf("\tin %s\n", frame.closure.name))
	}
	return builder.String()
}
