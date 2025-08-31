package glisp

import "fmt"

type Frame struct {
	name    string          // The name of the function being executed, for error reporting.
	symbols map[string]Expr // The environment in which the function executes.
}

type Evaluator struct {
	frames []Frame
}

func NewEvaluator() *Evaluator {
	e := &Evaluator{}
	e.pushFrame(Frame{"global", make(map[string]Expr)})
	return e
}

func (e *Evaluator) currentFrame() *Frame {
	return &e.frames[len(e.frames)-1]
}

func (e *Evaluator) pushFrame(frame Frame) {
	e.frames = append(e.frames, frame)
}

func (e *Evaluator) popFrame() {
	e.frames = e.frames[:len(e.frames)-1]
}

func (e *Evaluator) lookup(atom *Atom) (Expr, error) {
	for i := len(e.frames) - 1; i >= 0; i-- {
		if expr, ok := e.frames[i].symbols[atom.name]; ok {
			return expr, nil
		}
	}
	return Nil, NewEvalError(fmt.Sprintf("Undefined name: '%s'", atom.name))
}

func (e *Evaluator) store(atom string, expr Expr) {
	e.currentFrame().symbols[atom] = expr
}

func (e *Evaluator) Eval(expr Expr) (Expr, error) {
	switch expr := expr.(type) {
	case *Builtin, *Closure, *NilExpr, *Number:
		return expr, nil
	case *Atom:
		if expr == True {
			return True, nil
		}
		return e.lookup(expr)
	case *Cons:
		fun, err := e.Eval(expr.car)
		if err != nil {
			return Nil, err
		}
		return e.apply(fun, expr.cdr)
	default:
		return Nil, NewEvalError(fmt.Sprintf("Unrecognized expression type: %v", expr))
	}
}

func (e *Evaluator) apply(callable, arg Expr) (Expr, error) {
	switch callable := callable.(type) {
	case *Builtin:
		return callable.fun(arg, e.currentFrame())
	case *Closure:
		return e.reduce(callable, arg)
	default:
		return Nil, NewEvalError(fmt.Sprintf("Attempting to evaluate %v, expected \"Builtin\" or \"Closure\"", callable))
	}
}

func (e *Evaluator) reduce(fun *Closure, arg Expr) (Expr, error) {
	evaluatedArgs, err := e.evalArg(arg)
	if err != nil {
		return Nil, err
	}
	closureEnv := make(map[string]Expr)
	for name, value := range fun.captured {
		closureEnv[name] = value
	}
	e.pushFrame(Frame{"<lambda>", closureEnv}) // TODO: Store the name if known
	defer e.popFrame()
	if err := e.bind(fun.param, evaluatedArgs); err != nil {
		return Nil, err
	}
	return e.Eval(fun.body)
}

func (e *Evaluator) evalArg(arg Expr) ([]Expr, error) {
	var result []Expr
	curr := arg
	for {
		cons, ok := curr.(*Cons)
		if !ok {
			break
		}
		expr, err := e.Eval(cons.car)
		if err != nil {
			return nil, err
		}
		result = append(result, expr)
		curr = cons.cdr
	}
	// Handle the case when a lambda is applied to a list of arguments, such as
	// (f . args).
	if atom, ok := curr.(*Atom); ok {
		expr, err := e.lookup(atom)
		if err != nil {
			return nil, err
		}
		result = append(result, expr)
	}
	return result, nil
}

func (e *Evaluator) bind(param Expr, args []Expr) error {
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
				e.store(atom.name, args[i])
				curr = expr.cdr
				i++
			} else {
				return NewEvalError(fmt.Sprintf("A list of function parameters must consist of atoms, got %v", expr.car))
			}
		case *Atom:
			// Handle the case when the lambda arguments aren't a proper list, such as
			// (lambda args args) or (lambda (x . args) args), in which case all the remaining
			// arguments have to be bound to the last parameter.
			e.store(expr.name, sliceToCons(args[i:]))
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
