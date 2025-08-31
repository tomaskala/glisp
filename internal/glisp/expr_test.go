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

func TestBuiltinString(t *testing.T) {
	a1 := &Builtin{"first", func(e Expr, frame *Frame) (Expr, error) { return e, nil }}
	a2 := &Builtin{"second", func(e Expr, frame *Frame) (Expr, error) { return e, nil }}

	if a1.String() != "first" {
		t.Errorf("Expected \"first\", got \"%s\"", a1.String())
	}
	if a2.String() != "second" {
		t.Errorf("Expected \"second\", got \"%s\"", a2.String())
	}
}

func TestBuiltinEqual(t *testing.T) {
	a11 := &Builtin{"first", func(e Expr, frame *Frame) (Expr, error) { return e, nil }}
	a12 := &Builtin{"first", func(e Expr, frame *Frame) (Expr, error) { return e, nil }}
	a2 := &Builtin{"second", func(e Expr, frame *Frame) (Expr, error) { return e, nil }}

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

func TestClosureString(t *testing.T) {
	env := make(map[string]Expr)

	a1 := &Closure{"", Nil, Nil, env}
	a2 := &Closure{"", &Atom{"param"}, Nil, env}
	a3 := &Closure{"", &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, Nil}}, Nil, env}
	s := "<lambda>"

	if a1.String() != s {
		t.Errorf("Expected \"%s\", got \"%s\"", s, a1.String())
	}

	if a2.String() != s {
		t.Errorf("Expected \"%s\", got \"%s\"", s, a2.String())
	}

	if a3.String() != s {
		t.Errorf("Expected \"%s\", git \"%s\"", s, a3.String())
	}
}

func TestClosureEqual(t *testing.T) {
	env := make(map[string]Expr)

	a11 := &Closure{"", Nil, Nil, env}
	a12 := &Closure{"", Nil, Nil, env}
	a2 := &Closure{"", Nil, Nil, env}

	if !a11.Equal(a11) {
		t.Errorf("Closure must be equal to itself")
	}
	if a11.Equal(a12) {
		t.Errorf("Closure must not be equal to another, even with the same name")
	}
	if a11.Equal(a2) {
		t.Errorf("Closure must not be equal to another with a different name")
	}
}

func TestConsString(t *testing.T) {
	a1 := Nil
	a2 := &Cons{&Atom{"head"}, Nil}
	a3 := &Cons{&Atom{"a"}, &Cons{&Atom{"b"}, Nil}}
	a4 := &Cons{&Atom{"first"}, &Atom{"second"}}
	a5 := &Cons{
		&Cons{
			&Atom{"a"},
			&Atom{"b"},
		},
		&Cons{
			&Atom{"c"},
			&Cons{
				&Atom{"d"},
				Nil,
			},
		},
	}

	s1 := "()"
	if a1.String() != s1 {
		t.Errorf("Expected \"%s\", got \"%s\"", s1, a1.String())
	}

	s2 := "(head)"
	if a2.String() != s2 {
		t.Errorf("Expected \"%s\", got \"%s\"", s2, a2.String())
	}

	s3 := "(a b)"
	if a3.String() != s3 {
		t.Errorf("Expected \"%s\", got \"%s\"", s3, a3.String())
	}

	s4 := "(first . second)"
	if a4.String() != s4 {
		t.Errorf("Expected \"%s\", got \"%s\"", s4, a4.String())
	}

	s5 := "((a . b) c d)"
	if a5.String() != s5 {
		t.Errorf("Expected \"%s\", got \"%s\"", s5, a5.String())
	}
}

func TestConsEqual(t *testing.T) {
	for _, equals := range map[string]struct {
		fst Expr
		snd Expr
	}{
		"nil":               {Nil, Nil},
		"single element":    {&Cons{&Atom{"head"}, Nil}, &Cons{&Atom{"head"}, Nil}},
		"multiple elements": {&Cons{&Atom{"a"}, &Cons{&Atom{"b"}, Nil}}, &Cons{&Atom{"a"}, &Cons{&Atom{"b"}, Nil}}},
		"improper list":     {&Cons{&Atom{"a"}, &Atom{"b"}}, &Cons{&Atom{"a"}, &Atom{"b"}}},
		"nested": {
			&Cons{
				&Cons{
					&Atom{"a"},
					&Atom{"b"},
				},
				&Cons{
					&Atom{"c"},
					&Cons{
						&Atom{"d"},
						Nil,
					},
				},
			},
			&Cons{
				&Cons{
					&Atom{"a"},
					&Atom{"b"},
				},
				&Cons{
					&Atom{"c"},
					&Cons{
						&Atom{"d"},
						Nil,
					},
				},
			},
		},
	} {
		if !equals.fst.Equal(equals.snd) {
			t.Errorf("%v is not equal to %v", equals.fst, equals.snd)
		}
	}

	for _, notEquals := range map[string]struct {
		fst Expr
		snd Expr
	}{
		"nils":            {Nil, &Cons{Nil, Nil}},
		"different atom":  {&Cons{&Atom{"head1"}, Nil}, &Cons{&Atom{"head2"}, Nil}},
		"flat and nested": {&Cons{&Atom{"a"}, &Atom{"b"}}, &Cons{&Cons{&Atom{"a"}, Nil}, &Cons{&Atom{"b"}, Nil}}},
		"atom and number": {&Cons{&Atom{"1"}, Nil}, &Cons{&Number{1}, Nil}},
		"nested": {
			&Cons{
				&Cons{
					&Atom{"a"},
					&Atom{"b"},
				},
				&Cons{
					&Atom{"c"},
					&Cons{
						&Atom{"d"},
						Nil,
					},
				},
			},
			&Cons{
				&Cons{
					&Atom{"a"},
					&Atom{"b"},
				},
				&Cons{
					&Atom{"c"},
					&Cons{
						&Atom{"e"},
						Nil,
					},
				},
			},
		},
	} {
		if notEquals.fst.Equal(notEquals.snd) {
			t.Errorf("%v is equal to %v", notEquals.fst, notEquals.snd)
		}
	}
}

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
