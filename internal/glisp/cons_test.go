package glisp

import "testing"

func TestConsString(t *testing.T) {
	a1 := Nil
	a2 := &Cons{car: &Atom{"head"}, cdr: Nil}
	a3 := &Cons{car: &Atom{"a"}, cdr: &Cons{car: &Atom{"b"}, cdr: Nil}}
	a4 := &Cons{car: &Atom{"first"}, cdr: &Atom{"second"}}
	a5 := &Cons{
		car: &Cons{
			car: &Atom{"a"},
			cdr: &Atom{"b"},
		},
		cdr: &Cons{
			car: &Atom{"c"},
			cdr: &Cons{
				car: &Atom{"d"},
				cdr: Nil,
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
		fst *Cons
		snd *Cons
	}{
		"nil":               {Nil, Nil},
		"single element":    {&Cons{car: &Atom{"head"}, cdr: Nil}, &Cons{car: &Atom{"head"}, cdr: Nil}},
		"multiple elements": {&Cons{car: &Atom{"a"}, cdr: &Cons{car: &Atom{"b"}, cdr: Nil}}, &Cons{car: &Atom{"a"}, cdr: &Cons{car: &Atom{"b"}, cdr: Nil}}},
		"improper list":     {&Cons{&Atom{"a"}, &Atom{"b"}}, &Cons{&Atom{"a"}, &Atom{"b"}}},
		"nested": {
			&Cons{
				car: &Cons{
					car: &Atom{"a"},
					cdr: &Atom{"b"},
				},
				cdr: &Cons{
					car: &Atom{"c"},
					cdr: &Cons{
						car: &Atom{"d"},
						cdr: Nil,
					},
				},
			},
			&Cons{
				car: &Cons{
					car: &Atom{"a"},
					cdr: &Atom{"b"},
				},
				cdr: &Cons{
					car: &Atom{"c"},
					cdr: &Cons{
						car: &Atom{"d"},
						cdr: Nil,
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
		fst *Cons
		snd *Cons
	}{
		"nils":            {Nil, &Cons{car: Nil, cdr: Nil}},
		"different atom":  {&Cons{car: &Atom{"head1"}, cdr: Nil}, &Cons{car: &Atom{"head2"}, cdr: Nil}},
		"flat and nested": {&Cons{car: &Atom{"a"}, cdr: &Atom{"b"}}, &Cons{car: &Cons{car: &Atom{"a"}, cdr: Nil}, cdr: &Cons{car: &Atom{"b"}, cdr: Nil}}},
		"atom and number": {&Cons{car: &Atom{"1"}, cdr: Nil}, &Cons{car: &Number{1}, cdr: Nil}},
		"nested": {
			&Cons{
				car: &Cons{
					car: &Atom{"a"},
					cdr: &Atom{"b"},
				},
				cdr: &Cons{
					car: &Atom{"c"},
					cdr: &Cons{
						car: &Atom{"d"},
						cdr: Nil,
					},
				},
			},
			&Cons{
				car: &Cons{
					car: &Atom{"a"},
					cdr: &Atom{"b"},
				},
				cdr: &Cons{
					car: &Atom{"c"},
					cdr: &Cons{
						car: &Atom{"e"},
						cdr: Nil,
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
