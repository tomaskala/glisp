package ast

import (
	"fmt"

	"tomaskala.com/glisp/internal/tokenizer"
)

func Token(node Node) tokenizer.Token {
	switch node := node.(type) {
	case *Program:
		return node.Tok
	case *Nil:
		return node.Tok
	case *Atom:
		return node.Tok
	case *Number:
		return node.Tok
	case *Quote:
		return node.Tok
	case *Call:
		return node.Tok
	case *Function:
		return node.Tok
	case *Define:
		return node.Tok
	case *Let:
		return node.Tok
	case *If:
		return node.Tok
	case *Cond:
		return node.Tok
	case *And:
		return node.Tok
	case *Or:
		return node.Tok
	case *Set:
		return node.Tok
	case *Begin:
		return node.Tok
	default:
		panic(fmt.Sprintf("unrecognized node type: %v", node))
	}
}

type Node any

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
