package glisp

import "fmt"

type ErrorType int

const (
	ErrorEOF ErrorType = iota
	ErrorParse
	ErrorEval
)

type LispError struct {
	Type  ErrorType
	msg   string
	name  string
	token token
}

func (e LispError) Error() string {
	return fmt.Sprintf("%s: %s on line %d, token '%s'", e.name, e.msg, e.token.line, e.token.val)
}

func NewEOFError(name string, token token) error {
	return LispError{ErrorEOF, "EOF", name, token}
}

// TODO: Include the token
// TODO: Include the name
func NewEvalError(msg string) error {
	return LispError{Type: ErrorEval, msg: msg}
}

func NewParseError(msg, name string, token token) error {
	return LispError{ErrorParse, msg, name, token}
}
