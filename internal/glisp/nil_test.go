package glisp

import "testing"

func TestNilString(t *testing.T) {
	n := Nil

	if n.String() != "()" {
		t.Errorf("Expected \"()\", got \"%s\"", n.String())
	}
}

func TestNilEqual(t *testing.T) {
	n := Nil

	if !n.Equal(n) {
		t.Errorf("Nil must be equal to itself")
	}
}

func TestNilEval(t *testing.T) {
	env := NewEnv(nil)

	n := Nil

	nEval, err := n.Eval(env)
	if err != nil {
		t.Fatalf("Error when evaluating nil: %v", err)
	}

	if nEval != Nil {
		t.Errorf("Nil evaluated to something else than itself: %v", nEval)
	}
}
