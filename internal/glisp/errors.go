package glisp

type LispError struct {
	msg  string
	name string
	pos  pos
	line int
}

type EOFError struct{}

func (EOFError) Error() string {
	return "EOF"
}

func (e LispError) Error() string {
	return e.msg
}

func NewEvalError(msg string) error {
	return LispError{msg: msg}
}

func NewParseError(msg, name string, token token) error {
	return LispError{msg: msg, name: name, pos: token.pos, line: token.line}
}
