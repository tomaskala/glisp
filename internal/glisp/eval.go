package glisp

import "fmt"

func Eval(expr Expr, env *Env) (Expr, error) {
	switch expr := expr.(type) {
	case *Builtin, *Closure, *NilExpr, *Number:
		return expr, nil
	case *Atom:
		if expr == True {
			return True, nil
		}
		val, err := env.Get(expr)
		if err != nil {
			return Nil, err
		}
		return val, nil
	case *Cons:
		fun, err := Eval(expr.car, env)
		if err != nil {
			return Nil, err
		}
		return apply(fun, expr.cdr, env)
	default:
		return Nil, NewEvalError(fmt.Sprintf("Unrecognized expression type: %v", expr))
	}
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

func reduce(fun *Closure, arg Expr, env *Env) (Expr, error) {
	evaluatedArgs, err := evalArg(arg, env)
	if err != nil {
		return Nil, err
	}
	closureEnv := NewEnv(fun.env)
	if err := bind(fun.param, evaluatedArgs, closureEnv); err != nil {
		return Nil, err
	}
	return Eval(fun.body, closureEnv)
}

func evalArg(arg Expr, env *Env) ([]Expr, error) {
	var result []Expr
	curr := arg
	for {
		cons, ok := curr.(*Cons)
		if !ok {
			break
		}
		expr, err := Eval(cons.car, env)
		if err != nil {
			return nil, err
		}
		result = append(result, expr)
		curr = cons.cdr
	}
	// Handle the case when a lambda is applied to a list of arguments, such as
	// (f . args).
	if atom, ok := curr.(*Atom); ok {
		expr, err := env.Get(atom)
		if err != nil {
			return nil, err
		}
		result = append(result, expr)
	}
	return result, nil
}

func bind(param Expr, args []Expr, env *Env) error {
	curr := param
	i := 0
	for {
		switch expr := curr.(type) {
		case *NilExpr:
			if i < len(args) {
				return NewEvalError(fmt.Sprintf("The function expects fewer arguments, %d given", len(args)))
			}
			return nil
		case *Cons:
			if atom, ok := expr.car.(*Atom); ok {
				if i == len(args) {
					return NewEvalError(fmt.Sprintf("The function expects more arguments, %d given", len(args)))
				}
				env.Set(atom.name, args[i])
				curr = expr.cdr
				i++
			} else {
				return NewEvalError(fmt.Sprintf("A list of function parameters must consist of atoms, got %v", expr.car))
			}
		case *Atom:
			// Handle the case when the lambda arguments aren't a proper list, such as
			// (lambda args args) or (lambda (x . args) args), in which case all the remaining
			// arguments have to be bound to the last parameter.
			env.Set(expr.name, sliceToCons(args[i:]))
			return nil
		default:
			return NewEvalError(fmt.Sprintf("Function parameter must be either an atom or a list of atoms, got %v", curr))
		}
	}
}

func sliceToCons(expr []Expr) Expr {
	var cons Expr = Nil
	for i := len(expr) - 1; i >= 0; i-- {
		cons = &Cons{expr[i], cons}
	}
	return cons
}
