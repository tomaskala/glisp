package glisp

import "testing"

func TestBuiltinString(t *testing.T) {
	a1 := &Builtin{"first", func(e Expr, env *Env) (Expr, error) { return e, nil }}
	a2 := &Builtin{"second", func(e Expr, env *Env) (Expr, error) { return e, nil }}

	if a1.String() != "first" {
		t.Errorf("Expected \"first\", got \"%s\"", a1.String())
	}
	if a2.String() != "second" {
		t.Errorf("Expected \"second\", got \"%s\"", a2.String())
	}
}

func TestBuiltinEqual(t *testing.T) {
	a11 := &Builtin{"first", func(e Expr, env *Env) (Expr, error) { return e, nil }}
	a12 := &Builtin{"first", func(e Expr, env *Env) (Expr, error) { return e, nil }}
	a2 := &Builtin{"second", func(e Expr, env *Env) (Expr, error) { return e, nil }}

	if !a11.Equal(a11) {
		t.Errorf("Builtin must be equal to itself")
	}
	if !a11.Equal(a12) {
		t.Errorf("Builtin must be equal to another with the same name")
	}
	if a11.Equal(a2) {
		t.Errorf("Builtin must not be equal to another with a different name")
	}
}

func TestBuiltinEval(t *testing.T) {
	env := NewEnv(nil)

	a := &Builtin{"first", func(e Expr, env *Env) (Expr, error) { return e, nil }}

	aEval, err := a.Eval(env)
	if err != nil {
		t.Fatalf("Error when evaluating a builtin: %v", err)
	}

	if aEval != a {
		t.Errorf("Builtin evaluated to something else than itself: %v", aEval)
	}
}
