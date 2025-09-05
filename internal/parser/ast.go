package parser

import (
	"fmt"
	"strings"
)

type Expr interface {
	String() string
	Equal(Expr) bool
}

type Program struct {
	Exprs []Expr
}

type Nil struct{}

type Atom struct {
	Name string
}

type Number struct {
	Value float64
}

type Quote struct {
	Value Expr
}

type Call struct {
	Func Expr   // The expression being called (not an Atom, there is NamedCall for that).
	Args []Expr // The arguments provided to the expression being called.
}

type NamedCall struct {
	Func string
	Args []Expr
}

type Function struct {
	Name      string   // Empty for anonymous functions, otherwise spliced in from "define".
	Params    []string // Names of the function's parameters, in order.
	RestParam string   // Name of the optional rest parameter, or empty if none.
	Body      Expr     // The body to be evaluated.
}

type Define struct {
	Name  string
	Value Expr
}

type Let struct {
	Bindings []Binding
	Body     Expr
}

type Binding struct {
	Name  string
	Value Expr
}

func (p *Program) String() string {
	var builder strings.Builder
	for i, expr := range p.Exprs {
		if i > 0 {
			builder.WriteByte(' ')
		}
		builder.WriteString(expr.String())
	}
	return builder.String()
}

func (p *Program) Equal(other Expr) bool {
	p2, ok := other.(*Program)
	if !ok {
		return false
	}
	if len(p.Exprs) != len(p2.Exprs) {
		return false
	}
	for i := range p.Exprs {
		if !p.Exprs[i].Equal(p2.Exprs[i]) {
			return false
		}
	}
	return true
}

func (Nil) String() string {
	return "()"
}

func (Nil) Equal(other Expr) bool {
	_, ok := other.(*Nil)
	return ok
}

func (a *Atom) String() string {
	return a.Name
}

func (a *Atom) Equal(other Expr) bool {
	a2, ok := other.(*Atom)
	if !ok {
		return false
	}
	return a.Name == a2.Name
}

func (n *Number) String() string {
	return fmt.Sprintf("%g", n.Value)
}

func (n *Number) Equal(other Expr) bool {
	n2, ok := other.(*Number)
	if !ok {
		return false
	}
	return n.Value == n2.Value
}

func (q *Quote) String() string {
	return fmt.Sprintf("'%s", q.Value.String())
}

func (q *Quote) Equal(other Expr) bool {
	q2, ok := other.(*Quote)
	if !ok {
		return false
	}
	return q.Value.Equal(q2.Value)
}

func (c *Call) String() string {
	var builder strings.Builder
	builder.WriteByte('(')
	builder.WriteString(c.Func.String())
	for _, arg := range c.Args {
		builder.WriteByte(' ')
		builder.WriteString(arg.String())
	}
	builder.WriteByte(')')
	return builder.String()
}

func (c *Call) Equal(other Expr) bool {
	c2, ok := other.(*Call)
	if !ok {
		return false
	}
	if !c.Func.Equal(c2.Func) {
		return false
	}
	if len(c.Args) != len(c2.Args) {
		return false
	}
	for i := range c.Args {
		if !c.Args[i].Equal(c2.Args[i]) {
			return false
		}
	}
	return true
}

func (n *NamedCall) String() string {
	var builder strings.Builder
	builder.WriteByte('(')
	builder.WriteString(n.Func)
	for _, arg := range n.Args {
		builder.WriteByte(' ')
		builder.WriteString(arg.String())
	}
	builder.WriteByte(')')
	return builder.String()
}

func (n *NamedCall) Equal(other Expr) bool {
	n2, ok := other.(*NamedCall)
	if !ok {
		return false
	}
	if n.Func != n2.Func {
		return false
	}
	if len(n.Args) != len(n2.Args) {
		return false
	}
	for i := range n.Args {
		if !n.Args[i].Equal(n2.Args[i]) {
			return false
		}
	}
	return true
}

func (f *Function) String() string {
	if f.Name == "" {
		return "<lambda>"
	}
	return fmt.Sprintf("<function %s>", f.Name)
}

func (f *Function) Equal(other Expr) bool {
	f2, ok := other.(*Function)
	if !ok {
		return false
	}
	if f.Name != f2.Name {
		return false
	}
	if len(f.Params) != len(f2.Params) {
		return false
	}
	for i := range f.Params {
		if f.Params[i] != f2.Params[i] {
			return false
		}
	}
	if f.RestParam != f2.RestParam {
		return false
	}
	return f.Body.Equal(f2.Body)
}

func (d *Define) String() string {
	var builder strings.Builder
	builder.WriteString("(define ")
	builder.WriteString(d.Name)
	builder.WriteByte(' ')
	builder.WriteString(d.Value.String())
	builder.WriteByte(')')
	return builder.String()
}

func (d *Define) Equal(other Expr) bool {
	d2, ok := other.(*Define)
	if !ok {
		return false
	}
	if d.Name != d2.Name {
		return false
	}
	return d.Value.Equal(d2.Value)
}

func (l *Let) String() string {
	var builder strings.Builder
	builder.WriteString("((")
	for i, binding := range l.Bindings {
		if i > 0 {
			builder.WriteByte(' ')
		}
		builder.WriteByte('(')
		builder.WriteString(binding.Name)
		builder.WriteByte(' ')
		builder.WriteString(binding.Value.String())
		builder.WriteByte(')')
	}
	builder.WriteString(") ")
	builder.WriteString(l.Body.String())
	builder.WriteByte(')')
	return builder.String()
}

func (l *Let) Equal(other Expr) bool {
	l2, ok := other.(*Let)
	if !ok {
		return false
	}
	if len(l.Bindings) != len(l2.Bindings) {
		return false
	}
	for i := range l.Bindings {
		if l.Bindings[i].Name != l2.Bindings[i].Name {
			return false
		}
		if !l.Bindings[i].Value.Equal(l2.Bindings[i].Value) {
			return false
		}
	}
	return l.Body.Equal(l2.Body)
}
