package glisp

import "fmt"

type Frame struct {
	closure *Closure        // The closure being evaluated.
	symbols map[string]Expr // The environment in which the function executes.
}

type Evaluator struct {
	name   string  // Name of the source being evaluated, used for error reporting.
	frames []Frame // Frame stack.
}

func NewEvaluator(name string, builtins map[string]Expr) *Evaluator {
	if builtins == nil {
		builtins = make(map[string]Expr)
	}
	e := &Evaluator{name: name}
	e.pushFrame(Frame{&Closure{name: "global"}, builtins})
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
	return Nil, e.runtimeError("Undefined name: '%s'", atom.name)
}

func (e *Evaluator) store(atom string, expr Expr) {
	e.currentFrame().symbols[atom] = expr
}

func (e *Evaluator) storeGlobal(atom string, expr Expr) {
	e.frames[0].symbols[atom] = expr
}

func (e *Evaluator) runtimeError(format string, args ...any) error {
	return NewRuntimeError(e.name, fmt.Sprintf(format, args...), e.frames)
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
		return Nil, e.runtimeError("Unrecognized expression type: %v", expr)
	}
}

func (e *Evaluator) apply(callable, arg Expr) (Expr, error) {
	switch callable := callable.(type) {
	case *Builtin:
		return callable.fun(arg, e)
	case *Closure:
		return e.reduce(callable, arg)
	default:
		return Nil, e.runtimeError("Attempting to evaluate %v, expected a builtin or a closure", callable)
	}
}

func (e *Evaluator) reduce(fun *Closure, arg Expr) (Expr, error) {
	evaluatedArgs, err := e.evalList(arg)
	if err != nil {
		return Nil, err
	}
	closureEnv := make(map[string]Expr)
	for name, value := range fun.captured {
		closureEnv[name] = value
	}
	e.pushFrame(Frame{fun, closureEnv})
	defer e.popFrame()
	if err := e.bind(fun.param, evaluatedArgs); err != nil {
		return Nil, err
	}
	return e.Eval(fun.body)
}

func (e *Evaluator) evalList(arg Expr) (Expr, error) {
	var list Expr = Nil
	curr := &list
	for {
		if cons, ok := arg.(*Cons); ok {
			val, err := e.Eval(cons.car)
			if err != nil {
				return Nil, err
			}
			next := &Cons{val, Nil}
			*curr = next
			curr = &next.cdr
			arg = cons.cdr
		} else {
			break
		}
	}
	if atom, ok := arg.(*Atom); ok {
		var err error
		*curr, err = e.lookup(atom)
		if err != nil {
			return Nil, err
		}
	}
	return list, nil
}

func (e *Evaluator) bind(param Expr, args Expr) error {
	currParam := param
	currArg := args
	totalArgs := 0
	for {
		switch expr := currParam.(type) {
		case *NilExpr:
			// No more parameters - check if we have remaining arguments
			if currArg != Nil {
				// Count remaining arguments for error message
				temp := currArg
				for temp != Nil {
					if cons, ok := temp.(*Cons); ok {
						totalArgs++
						temp = cons.cdr
					} else {
						totalArgs++
						break
					}
				}
				return e.runtimeError("The function expects fewer arguments, %d given", totalArgs)
			}
			return nil
		case *Cons:
			// Regular parameter in parameter list
			atom, ok := expr.car.(*Atom)
			if !ok {
				return e.runtimeError("A list of function parameters must consist of atoms, got %v", expr.car)
			}
			// Check if we have an argument available
			if currArg == Nil {
				return e.runtimeError("The function expects more arguments, %d given", totalArgs)
			}
			if argCons, ok := currArg.(*Cons); ok {
				// Bind the parameter to the argument
				e.store(atom.name, argCons.car)
				currParam = expr.cdr
				currArg = argCons.cdr
				totalArgs++
			} else {
				// This shouldn't happen with proper evalList, but handle it
				e.store(atom.name, currArg)
				currParam = expr.cdr
				currArg = Nil
			}
		case *Atom:
			// Rest parameter - bind all remaining arguments to this parameter
			e.store(expr.name, currArg)
			return nil
		default:
			return e.runtimeError("Function parameter must be either an atom or a list of atoms, got %v", currParam)
		}
	}
}
