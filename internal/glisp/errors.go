package glisp

type ErrorType int

const (
	ErrorEOF ErrorType = iota
	ErrorParse
	ErrorEval
)

type LispError struct {
	Type ErrorType
	Msg  string
	Name string
	Pos  position
	Line int
}

func (e LispError) Error() string {
	return e.Msg
}

func NewEOFError(name string, token token) error {
	return LispError{ErrorEOF, "EOF", name, token.pos, token.line}
}

func NewEvalError(msg string) error {
	return LispError{Type: ErrorEval, Msg: msg}
}

func NewParseError(msg, name string, token token) error {
	return LispError{ErrorParse, msg, name, token.pos, token.line}
}
