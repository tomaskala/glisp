package glisp

import "testing"

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

func TestConsEval(t *testing.T) {
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
