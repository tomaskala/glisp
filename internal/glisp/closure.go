package glisp

import "fmt"

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

func (c *Closure) Eval(env *Env) (Expr, error) {
	return c, nil
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
	return fun.body.Eval(closureEnv)
}

func evalArg(arg Expr, env *Env) ([]Expr, error) {
	var result []Expr
	curr := arg
	for {
		cons, ok := curr.(*Cons)
		if !ok || cons == Nil {
			break
		}
		expr, err := cons.car.Eval(env)
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
		case *Cons:
			if expr == Nil {
				if i < len(args) {
					return NewEvalError(fmt.Sprintf("The function expects fewer arguments, %d given", len(args)))
				}
				return nil
			}
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

func sliceToCons(expr []Expr) *Cons {
	cons := Nil
	for i := len(expr) - 1; i >= 0; i-- {
		cons = &Cons{expr[i], cons}
	}
	return cons
}
