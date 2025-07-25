package glisp

import "strings"

type Cons struct {
	car Expr
	cdr Expr
}

var Nil *Cons = nil

func (c *Cons) String() string {
	if c == Nil {
		return "()"
	}
	var curr Expr = c
	var sb strings.Builder
	for sb.WriteByte('('); ; sb.WriteByte(' ') {
		if cons, ok := curr.(*Cons); ok {
			sb.WriteString(cons.car.String())
			curr = cons.cdr
			if curr == Nil {
				break
			}
		}
		if _, ok := curr.(*Cons); !ok {
			sb.WriteString(" . ")
			sb.WriteString(curr.String())
			break
		}
	}
	sb.WriteByte(')')
	return sb.String()
}

func (c *Cons) Equal(o Expr) bool {
	if c == Nil || o == Nil {
		return c == o
	}
	if o, ok := o.(*Cons); ok {
		return c.car.Equal(o.car) && c.cdr.Equal(o.cdr)
	}
	return false
}

func (c *Cons) Eval(env *Env) (Expr, error) {
	if c == Nil {
		return Nil, nil
	}
	fun, err := c.car.Eval(env)
	if err != nil {
		return Nil, err
	}
	return apply(fun, c.cdr, env)
}
