package vm

import (
	"fmt"

	"tomaskala.com/glisp/internal/compiler"
	"tomaskala.com/glisp/internal/runtime"
)

// GetMacro implements the compiler.MacroRegistry interface.
func (vm *VM) GetMacro(name runtime.Atom) (runtime.Macro, bool) {
	macro, ok := vm.macros[name]
	return macro, ok
}

// StoreMacro implements the compiler.MacroRegistry interface.
func (vm *VM) StoreMacro(name runtime.Atom, macro runtime.Macro) {
	vm.macros[name] = macro
}

// ExpandMacro implements the compiler.MacroRegistry interface.
func (vm *VM) ExpandMacro(m runtime.Macro, args runtime.Value) (runtime.Value, error) {
	subst := make(map[runtime.Atom]runtime.Value)
	arg := args

	for _, param := range m.Params {
		if !arg.IsPair() {
			return runtime.MakeNil(), fmt.Errorf(
				"macro %s expects at least %d arguments",
				m.Name.Value(),
				len(m.Params),
			)
		}

		pair := arg.AsPair()
		subst[param] = pair.Car
		arg = pair.Cdr
	}

	if m.RestParam != runtime.EmptyAtom {
		subst[m.RestParam] = arg
	} else if !arg.IsNil() {
		return runtime.MakeNil(), fmt.Errorf("macro %s expects %d arguments, got more", m.Name.Value(), len(m.Params))
	}

	substituted, err := substitute(m.Body, subst)
	if err != nil {
		return runtime.MakeNil(), err
	}

	c := compiler.NewCompiler(m.Name.Value(), vm)
	prog, err := c.Compile(substituted)
	if err != nil {
		return runtime.MakeNil(), err
	}

	result, err := vm.Run(prog)
	if err != nil {
		return runtime.MakeNil(), fmt.Errorf("macro expansion %s failed: %w", m.Name.Value(), err)
	}

	return result, nil
}

func substitute(expr runtime.Value, subst map[runtime.Atom]runtime.Value) (runtime.Value, error) {
	switch {
	case expr.IsNil():
		return expr, nil
	case expr.IsAtom():
		atom := expr.AsAtom()
		if val, ok := subst[atom]; ok {
			return runtime.Cons(runtime.MakeAtom("quote"), runtime.Cons(val, runtime.MakeNil())), nil
		}

		return expr, nil
	case expr.IsNumber():
		return expr, nil
	case expr.IsPair():
		pair := expr.AsPair()

		car, err := substitute(pair.Car, subst)
		if err != nil {
			return runtime.MakeNil(), err
		}

		cdr, err := substitute(pair.Cdr, subst)
		if err != nil {
			return runtime.MakeNil(), err
		}

		return runtime.Cons(car, cdr), nil
	default:
		return runtime.MakeNil(), fmt.Errorf("unexpected expression: %v", expr)
	}
}
