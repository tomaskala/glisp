package glisp

import (
	"fmt"
	"strings"
)

type Expr interface {
	String() string
	Equal(Expr) bool
}

type Atom struct {
	name string
}

var True = &Atom{"#t"}

func (a *Atom) String() string {
	return a.name
}

func (a *Atom) Equal(o Expr) bool {
	if o, ok := o.(*Atom); ok {
		return a.name == o.name
	}
	return false
}

type Builtin struct {
	name string
	fun  func(Expr, *Env) (Expr, error)
}

func (b *Builtin) String() string {
	return b.name
}

func (b *Builtin) Equal(o Expr) bool {
	if o, ok := o.(*Builtin); ok {
		return b.name == o.name
	}
	return false
}

type Closure struct {
	param Expr
	body  Expr
	env   *Env
}

func (c *Closure) String() string {
	return fmt.Sprintf("<lambda %s>", c.param.String())
}

func (c *Closure) Equal(o Expr) bool {
	if o, ok := o.(*Closure); ok {
		return c == o
	}
	return false
}

type Cons struct {
	car Expr
	cdr Expr
}

func (c *Cons) String() string {
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
	if o, ok := o.(*Cons); ok {
		return c.car.Equal(o.car) && c.cdr.Equal(o.cdr)
	}
	return false
}

type NilExpr struct{}

var Nil = &NilExpr{}

func (n *NilExpr) String() string {
	return "()"
}

func (n *NilExpr) Equal(o Expr) bool {
	return o == Nil
}

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
