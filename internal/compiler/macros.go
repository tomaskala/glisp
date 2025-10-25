package compiler

import "tomaskala.com/glisp/internal/runtime"

var macroRegistry = make(map[runtime.Atom]*macro)

type macro struct {
	name      runtime.Atom
	params    []runtime.Atom
	restParam runtime.Atom
	body      runtime.Value
}

// Macro:
//
// "(" "macro" (atom | "(" expr* ")" | "(" expr+ "." expr ")") expr ")".
func (c *Compiler) compileMacro(expr runtime.Value, nameHint runtime.Atom) {
	params, body := c.extract2(expr, "macro")
	macro := &macro{name: nameHint, restParam: runtime.EmptyAtom, body: body}

	for params.IsPair() {
		param := params.AsPair()
		if !param.Car.IsAtom() {
			panic(c.errorf("macro parameter expects atom"))
		}
		atom := param.Car.AsAtom()

		macro.params = append(macro.params, atom)
		params = param.Cdr
	}

	if params.IsAtom() {
		atom := params.AsAtom()
		macro.restParam = atom
	} else if !params.IsNil() {
		panic(c.errorf("macro parameter expects atom"))
	}

	macroRegistry[nameHint] = macro
	c.emit(runtime.OpNil)
}

func (c *Compiler) getMacro(expr runtime.Value) *macro {
	if !expr.IsAtom() {
		return nil
	}
	return macroRegistry[expr.AsAtom()]
}

func (c *Compiler) expandMacro(m *macro, args runtime.Value) runtime.Value {
	subst := make(map[runtime.Atom]runtime.Value)
	arg := args

	for _, param := range m.params {
		if !arg.IsPair() {
			panic(c.errorf("macro %s expects at least %d arguments", m.name.Value(), len(m.params)))
		}

		pair := arg.AsPair()
		subst[param] = pair.Car
		arg = pair.Cdr
	}

	if m.restParam != runtime.EmptyAtom {
		subst[m.restParam] = arg
	} else if !arg.IsNil() {
		panic(c.errorf("macro %s expects %d arguments, got more", m.name.Value(), len(m.params)))
	}

	substituted := c.substitute(m.body, subst)

	compiler := newCompiler(c, m.name, c.vm)
	compiler.compileExpr(substituted, runtime.EmptyAtom, false)
	function := compiler.end()

	result, err := c.vm.Run(&runtime.Program{Function: function})
	if err != nil {
		panic(c.errorf("macro expansion %s failed: %v", m.name.Value(), err))
	}

	return result
}

func (c *Compiler) substitute(expr runtime.Value, subst map[runtime.Atom]runtime.Value) runtime.Value {
	switch {
	case expr.IsNil():
		return expr
	case expr.IsAtom():
		atom := expr.AsAtom()
		if val, ok := subst[atom]; ok {
			return runtime.Cons(runtime.MakeAtom("quote"), runtime.Cons(val, runtime.MakeNil()))
		}
		return expr
	case expr.IsNumber():
		return expr
	case expr.IsPair():
		pair := expr.AsPair()
		car := c.substitute(pair.Car, subst)
		cdr := c.substitute(pair.Cdr, subst)
		return runtime.Cons(car, cdr)
	default:
		panic(c.errorf("unexpected expression: %v", expr))
	}
}
