package glisp

import "fmt"

type Number struct {
	value float64
}

func (n *Number) String() string {
	return fmt.Sprintf("%g", n.value)
}

func (n *Number) Equal(o Expr) bool {
	if o, ok := o.(*Number); ok {
		return n.value == o.value
	}
	return false
}

func (n *Number) Eval(env *Env) (Expr, error) {
	return n, nil
}
