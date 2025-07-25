package glisp

type LispError struct {
	msg string
}

func (e LispError) Error() string {
	return e.msg
}

func NewEvalError(msg string) error {
	return LispError{msg: msg}
}
