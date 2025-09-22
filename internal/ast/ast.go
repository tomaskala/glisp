package ast

import (
	"fmt"
	"strings"

	"tomaskala.com/glisp/internal/tokenizer"
)

type Node interface {
	String() string
	Equal(Node) bool
	Token() tokenizer.Token
}

type Program struct {
	Exprs []Node
	Tok   tokenizer.Token
}

type Nil struct {
	Tok tokenizer.Token
}

type Atom struct {
	Name string
	Tok  tokenizer.Token
}

type Number struct {
	Value float64
	Tok   tokenizer.Token
}

type Quote struct {
	Value Node
	Tok   tokenizer.Token
}

type Call struct {
	Func Node   // The expression being called.
	Args []Node // The arguments provided to the expression being called.
	Tok  tokenizer.Token
}

type Function struct {
	Name      string   // Empty for anonymous functions, otherwise spliced in from "define".
	Params    []string // Names of the function's parameters, in order.
	RestParam string   // Name of the optional rest parameter, or empty if none.
	Body      Node     // The body to be evaluated.
	Tok       tokenizer.Token
}

type Define struct {
	Name  string
	Value Node
	Tok   tokenizer.Token
}

type LetKind int

const (
	LetPlain LetKind = iota
	LetStar
	LetRec
)

type Let struct {
	Kind     LetKind
	Bindings []Binding
	Body     Node
	Tok      tokenizer.Token
}

type Binding struct {
	Name  string
	Value Node
}

type If struct {
	Cond Node
	Then Node
	Else Node
	Tok  tokenizer.Token
}

type Cond struct {
	Clauses []CondClause
	Tok     tokenizer.Token
}

type CondClause struct {
	Cond  Node
	Value Node
}

type And struct {
	Exprs []Node
	Tok   tokenizer.Token
}

type Or struct {
	Exprs []Node
	Tok   tokenizer.Token
}

type Set struct {
	Variable string
	Value    Node
	Tok      tokenizer.Token
}

type Begin struct {
	Exprs []Node
	Tail  Node
	Tok   tokenizer.Token
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
	switch l.Kind {
	case LetPlain:
		builder.WriteString("(let (")
	case LetStar:
		builder.WriteString("(let* (")
	case LetRec:
		builder.WriteString("(letrec (")
	}
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
func (i *If) String() string {
	var builder strings.Builder
	builder.WriteString("(if ")
	builder.WriteString(i.Cond.String())
	builder.WriteByte(' ')
	builder.WriteString(i.Then.String())
	builder.WriteByte(' ')
	builder.WriteString(i.Else.String())
	builder.WriteByte(')')
	return builder.String()
}
func (c *Cond) String() string {
	var builder strings.Builder
	builder.WriteString("(cond")
	for _, clause := range c.Clauses {
		builder.WriteString(" (")
		builder.WriteString(clause.Cond.String())
		builder.WriteByte(' ')
		builder.WriteString(clause.Value.String())
		builder.WriteByte(')')
	}
	builder.WriteByte(')')
	return builder.String()
}
func (a *And) String() string {
	var builder strings.Builder
	builder.WriteString("(and")
	for _, expr := range a.Exprs {
		builder.WriteByte(' ')
		builder.WriteString(expr.String())
	}
	builder.WriteByte(')')
	return builder.String()
}
func (o *Or) String() string {
	var builder strings.Builder
	builder.WriteString("(or")
	for _, expr := range o.Exprs {
		builder.WriteByte(' ')
		builder.WriteString(expr.String())
	}
	builder.WriteByte(')')
	return builder.String()
}
func (s *Set) String() string {
	var builder strings.Builder
	builder.WriteString("(set! ")
	builder.WriteString(s.Variable)
	builder.WriteByte(' ')
	builder.WriteString(s.Value.String())
	builder.WriteByte(')')
	return builder.String()
}
func (b *Begin) String() string {
	var builder strings.Builder
	builder.WriteString("(begin")
	for _, expr := range b.Exprs {
		builder.WriteByte(' ')
		builder.WriteString(expr.String())
	}
	builder.WriteByte(' ')
	builder.WriteString(b.Tail.String())
	builder.WriteByte(')')
	return builder.String()
}

func (p *Program) Equal(other Node) bool {
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
func (*Nil) Equal(other Node) bool {
	_, ok := other.(*Nil)
	return ok
}
func (a *Atom) Equal(other Node) bool {
	a2, ok := other.(*Atom)
	if !ok {
		return false
	}
	return a.Name == a2.Name
}
func (n *Number) Equal(other Node) bool {
	n2, ok := other.(*Number)
	if !ok {
		return false
	}
	return n.Value == n2.Value
}
func (q *Quote) Equal(other Node) bool {
	q2, ok := other.(*Quote)
	if !ok {
		return false
	}
	return q.Value.Equal(q2.Value)
}
func (c *Call) Equal(other Node) bool {
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
func (f *Function) Equal(other Node) bool {
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
func (d *Define) Equal(other Node) bool {
	d2, ok := other.(*Define)
	if !ok {
		return false
	}
	if d.Name != d2.Name {
		return false
	}
	return d.Value.Equal(d2.Value)
}
func (l *Let) Equal(other Node) bool {
	l2, ok := other.(*Let)
	if !ok {
		return false
	}
	if l.Kind != l2.Kind {
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
func (i *If) Equal(other Node) bool {
	i2, ok := other.(*If)
	if !ok {
		return false
	}
	if !i.Cond.Equal(i2.Cond) {
		return false
	}
	if !i.Then.Equal(i2.Then) {
		return false
	}
	return i.Else.Equal(i2.Else)
}
func (c *Cond) Equal(other Node) bool {
	c2, ok := other.(*Cond)
	if !ok {
		return false
	}
	if len(c.Clauses) != len(c2.Clauses) {
		return false
	}
	for i := range c.Clauses {
		if !c.Clauses[i].Cond.Equal(c2.Clauses[i].Cond) {
			return false
		}
		if !c.Clauses[i].Value.Equal(c2.Clauses[i].Value) {
			return false
		}
	}
	return true
}
func (a *And) Equal(other Node) bool {
	a2, ok := other.(*And)
	if !ok {
		return false
	}
	if len(a.Exprs) != len(a2.Exprs) {
		return false
	}
	for i := range a.Exprs {
		if !a.Exprs[i].Equal(a2.Exprs[i]) {
			return false
		}
	}
	return true
}
func (o *Or) Equal(other Node) bool {
	o2, ok := other.(*Or)
	if !ok {
		return false
	}
	if len(o.Exprs) != len(o2.Exprs) {
		return false
	}
	for i := range o.Exprs {
		if !o.Exprs[i].Equal(o2.Exprs[i]) {
			return false
		}
	}
	return true
}
func (s *Set) Equal(other Node) bool {
	s2, ok := other.(*Set)
	if !ok {
		return false
	}
	return s.Variable == s2.Variable && s.Value.Equal(s2.Value)
}
func (b *Begin) Equal(other Node) bool {
	b2, ok := other.(*Begin)
	if !ok {
		return false
	}
	if len(b.Exprs) != len(b2.Exprs) {
		return false
	}
	for i := range b.Exprs {
		if !b.Exprs[i].Equal(b2.Exprs[i]) {
			return false
		}
	}
	return b.Tail.Equal(b2.Tail)
}

func (p *Program) Token() tokenizer.Token {
	return p.Tok
}
func (n *Nil) Token() tokenizer.Token {
	return n.Tok
}
func (a *Atom) Token() tokenizer.Token {
	return a.Tok
}
func (n *Number) Token() tokenizer.Token {
	return n.Tok
}
func (q *Quote) Token() tokenizer.Token {
	return q.Tok
}
func (c *Call) Token() tokenizer.Token {
	return c.Tok
}
func (f *Function) Token() tokenizer.Token {
	return f.Tok
}
func (d *Define) Token() tokenizer.Token {
	return d.Tok
}
func (l *Let) Token() tokenizer.Token {
	return l.Tok
}
func (i *If) Token() tokenizer.Token {
	return i.Tok
}
func (c *Cond) Token() tokenizer.Token {
	return c.Tok
}
func (a *And) Token() tokenizer.Token {
	return a.Tok
}
func (o *Or) Token() tokenizer.Token {
	return o.Tok
}
func (s *Set) Token() tokenizer.Token {
	return s.Tok
}
func (b *Begin) Token() tokenizer.Token {
	return b.Tok
}
