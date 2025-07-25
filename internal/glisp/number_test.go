package glisp

import "testing"

func TestNumberString(t *testing.T) {
	a1 := &Number{123}
	a2 := &Number{123.456}

	if a1.String() != "123" {
		t.Errorf("Expected \"123\", got \"%s\"", a1.String())
	}
	if a2.String() != "123.456" {
		t.Errorf("Expected \"123.456\", got \"%s\"", a2.String())
	}
}

func TestNumberEqual(t *testing.T) {
	a11 := &Number{123}
	a12 := &Number{123}
	a2 := &Number{567}

	if !a11.Equal(a11) {
		t.Errorf("Number must be equal to itself")
	}
	if !a11.Equal(a12) {
		t.Errorf("Number must be equal to another with the same value")
	}
	if a11.Equal(a2) {
		t.Errorf("Number must not be equal to another with a different value")
	}
}

func TestNumberEval(t *testing.T) {
	env := NewEnv(nil)

	a := &Number{123}

	aEval, err := a.Eval(env)
	if err != nil {
		t.Fatalf("Error when evaluating a number: %v", err)
	}

	if aEval != a {
		t.Errorf("Number evaluated to something else than itself: %v", aEval)
	}
}
