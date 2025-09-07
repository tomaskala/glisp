package parser

import (
	"fmt"
	"strings"

	"tomaskala.com/glisp/internal/tokenizer"
)

type Expr interface {
	isExpr() // Marker method.
	String() string
	Equal(Expr) bool
	Token() tokenizer.Token
}

type Program struct {
	Exprs []Expr
	token tokenizer.Token
}

type Nil struct {
	token tokenizer.Token
}

type Atom struct {
	Name  string
	token tokenizer.Token
}

type Number struct {
	Value float64
	token tokenizer.Token
}

type Quote struct {
	Value Expr
	token tokenizer.Token
}

type Call struct {
	Func  Expr   // The expression being called.
	Args  []Expr // The arguments provided to the expression being called.
	token tokenizer.Token
}

type Function struct {
	Name      string   // Empty for anonymous functions, otherwise spliced in from "define".
	Params    []string // Names of the function's parameters, in order.
	RestParam string   // Name of the optional rest parameter, or empty if none.
	Body      Expr     // The body to be evaluated.
	token     tokenizer.Token
}

type Define struct {
	Name  string
	Value Expr
	token tokenizer.Token
}

type Let struct {
	Bindings []Binding
	Body     Expr
	token    tokenizer.Token
}

type Binding struct {
	Name  string
	Value Expr
}

func (*Program) isExpr()  {}
func (*Nil) isExpr()      {}
func (*Atom) isExpr()     {}
func (*Number) isExpr()   {}
func (*Quote) isExpr()    {}
func (*Call) isExpr()     {}
func (*Function) isExpr() {}
func (*Define) isExpr()   {}
func (*Let) isExpr()      {}

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
func (*Nil) String() string {
	return "()"
}
func (a *Atom) String() string {
	return a.Name
}
func (n *Number) String() string {
	return fmt.Sprintf("%g", n.Value)
}
func (q *Quote) String() string {
	return fmt.Sprintf("'%s", q.Value.String())
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
func (f *Function) String() string {
	if f.Name == "" {
		return "<lambda>"
	}
	return fmt.Sprintf("<function %s>", f.Name)
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
func (*Nil) Equal(other Expr) bool {
	_, ok := other.(*Nil)
	return ok
}
func (a *Atom) Equal(other Expr) bool {
	a2, ok := other.(*Atom)
	if !ok {
		return false
	}
	return a.Name == a2.Name
}
func (n *Number) Equal(other Expr) bool {
	n2, ok := other.(*Number)
	if !ok {
		return false
	}
	return n.Value == n2.Value
}
func (q *Quote) Equal(other Expr) bool {
	q2, ok := other.(*Quote)
	if !ok {
		return false
	}
	return q.Value.Equal(q2.Value)
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

func (p *Program) Token() tokenizer.Token {
	return p.token
}
func (n *Nil) Token() tokenizer.Token {
	return n.token
}
func (a *Atom) Token() tokenizer.Token {
	return a.token
}
func (n *Number) Token() tokenizer.Token {
	return n.token
}
func (q *Quote) Token() tokenizer.Token {
	return q.token
}
func (c *Call) Token() tokenizer.Token {
	return c.token
}
func (f *Function) Token() tokenizer.Token {
	return f.token
}
func (d *Define) Token() tokenizer.Token {
	return d.token
}
func (l *Let) Token() tokenizer.Token {
	return l.token
}
