package glisp

import "fmt"

type Expr interface {
	String() string
	Equal(Expr) bool
	Eval(*Env) (Expr, error)
}

func apply(callable, arg Expr, env *Env) (Expr, error) {
	switch callable := callable.(type) {
	case *Builtin:
		return callable.fun(arg, env)
	case *Closure:
		return reduce(callable, arg, env)
	default:
		return Nil, NewEvalError(fmt.Sprintf("Attempting to evaluate %v, expected \"Builtin\" or \"Closure\"", callable))
	}
}
