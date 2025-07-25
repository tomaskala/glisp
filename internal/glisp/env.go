package glisp

import "fmt"

type Env struct {
	symbols map[string]Expr
	parent  *Env
}

func NewEnv(parent *Env) *Env {
	return &Env{
		symbols: make(map[string]Expr),
		parent:  parent,
	}
}

func (e *Env) Get(atom *Atom) (Expr, error) {
	curr := e
	for curr != nil {
		if expr, ok := curr.symbols[atom.name]; ok {
			return expr, nil
		}
		curr = curr.parent
	}
	return Nil, NewEvalError(fmt.Sprintf("Undefined name: %s", atom.name))
}

func (e *Env) Set(atom string, expr Expr) {
	e.symbols[atom] = expr
}

func (e *Env) SetTop(atom string, expr Expr) {
	curr := e
	for curr.parent != nil {
		curr = curr.parent
	}
	curr.symbols[atom] = expr
}
