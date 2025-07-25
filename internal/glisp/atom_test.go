package glisp

import "testing"

func TestAtomString(t *testing.T) {
	a1 := &Atom{"first"}
	a2 := &Atom{"second"}

	if a1.String() != "first" {
		t.Errorf("Expected \"first\", got \"%s\"", a1.String())
	}
	if a2.String() != "second" {
		t.Errorf("Expected \"second\", got \"%s\"", a2.String())
	}
}

func TestAtomEqual(t *testing.T) {
	a11 := &Atom{"first"}
	a12 := &Atom{"first"}
	a2 := &Atom{"second"}

	if !a11.Equal(a11) {
		t.Errorf("Atom must be equal to itself")
	}
	if !a11.Equal(a12) {
		t.Errorf("Atom must be equal to another with the same name")
	}
	if a11.Equal(a2) {
		t.Errorf("Atom must not be equal to another with a different name")
	}
}

func TestAtomEval(t *testing.T) {
	env := NewEnv(nil)

	trueEval, err := True.Eval(env)
	if err != nil {
		t.Fatalf("Error when evaluating True: %v", err)
	}

	if trueEval != True {
		t.Errorf("True evaluated to something else than itself: %v", trueEval)
	}

	a := &Atom{"atom"}
	env.Set("atom", a)

	aEval, err := a.Eval(env)
	if err != nil {
		t.Fatalf("Error when evaluating an atom: %v", err)
	}

	if aEval != a {
		t.Errorf("Atom evaluated to something else than itself: %v", aEval)
	}

	b := &Atom{"another"}
	bEval, err := b.Eval(env)
	if err == nil {
		t.Errorf("Expected an error when evaluating an undefined atom, received %v", bEval)
	}
}
