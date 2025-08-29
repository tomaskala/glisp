package glisp

import (
	"fmt"
	"testing"
)

func TestClosureString(t *testing.T) {
	env := NewEnv(nil)

	a1 := &Closure{Nil, Nil, env}
	a2 := &Closure{&Atom{"param"}, Nil, env}
	a3 := &Closure{&Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, Nil}}, Nil, env}

	s1 := "<lambda ()>"
	if a1.String() != s1 {
		t.Errorf("Expected \"%s\", got \"%s\"", s1, a1.String())
	}

	s2 := "<lambda param>"
	if a2.String() != s2 {
		t.Errorf("Expected \"%s\", got \"%s\"", s2, a2.String())
	}

	s3 := "<lambda (fst snd)>"
	if a3.String() != s3 {
		t.Errorf("Expected \"%s\", git \"%s\"", s3, a3.String())
	}
}

func TestClosureEqual(t *testing.T) {
	env := NewEnv(nil)

	a11 := &Closure{Nil, Nil, env}
	a12 := &Closure{Nil, Nil, env}
	a2 := &Closure{Nil, Nil, env}

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

func TestClosureEval(t *testing.T) {
	env := NewEnv(nil)
	a := &Closure{Nil, Nil, env}

	aEval, err := a.Eval(env)
	if err != nil {
		t.Fatalf("Error when evaluating a closure: %v", err)
	}

	if aEval != a {
		t.Errorf("Closure evaluated to something else than itself: %v", aEval)
	}
}

func TestReduce(t *testing.T) {
	for name, tc := range map[string]struct {
		param Expr
		body  Expr

		arg Expr

		result Expr
		err    error
	}{
		"constant nil, no args": {
			param: Nil,
			body:  Nil,

			arg: Nil,

			result: Nil,
		},
		"constant nil, args": {
			param: &Cons{&Atom{"a"}, &Cons{&Atom{"b"}, Nil}},
			body:  Nil,

			arg: &Cons{&Number{1}, &Cons{&Number{2}, Nil}},

			result: Nil,
		},
		"identity": {
			param: &Cons{&Atom{"x"}, Nil},
			body:  &Atom{"x"},

			arg: &Cons{&Number{1337}, Nil},

			result: &Number{1337},
		},
		"return first arg": {
			param: &Cons{&Atom{"x"}, &Cons{&Atom{"y"}, Nil}},
			body:  &Atom{"x"},

			arg: &Cons{&Number{1337}, &Cons{&Number{12}, Nil}},

			result: &Number{1337},
		},
		"return second arg": {
			param: &Cons{&Atom{"x"}, &Cons{&Atom{"y"}, Nil}},
			body:  &Atom{"y"},

			arg: &Cons{&Number{1337}, &Cons{&Number{12}, Nil}},

			result: &Number{12},
		},
		"return catch-all args": {
			param: &Atom{"args"},
			body:  &Atom{"args"},

			arg: &Cons{&Number{1337}, &Cons{&Number{12}, Nil}},

			result: &Cons{&Number{1337}, &Cons{&Number{12}, Nil}},
		},
		"improper list, return first arg": {
			param: &Cons{&Atom{"x"}, &Cons{&Atom{"y"}, &Atom{"rest"}}},
			body:  &Atom{"x"},

			arg: &Cons{&Atom{"one"}, &Cons{&Number{2}, &Cons{&Number{3}, &Cons{&Number{4}, Nil}}}},

			result: &Number{1},
		},
		"improper list, return second arg": {
			param: &Cons{&Atom{"x"}, &Cons{&Atom{"y"}, &Atom{"rest"}}},
			body:  &Atom{"y"},

			arg: &Cons{&Number{1}, &Cons{&Atom{"two"}, &Cons{&Number{3}, &Cons{&Number{4}, Nil}}}},

			result: &Number{2},
		},
		"improper list, return catch-all": {
			param: &Cons{&Atom{"x"}, &Cons{&Atom{"y"}, &Atom{"rest"}}},
			body:  &Atom{"rest"},

			arg: &Cons{&Number{1}, &Cons{&Number{2}, &Cons{&Number{3}, &Cons{&Number{4}, Nil}}}},

			result: &Cons{&Number{3}, &Cons{&Number{4}, Nil}},
		},
		"improper list, return unused catch-all": {
			param: &Cons{&Atom{"x"}, &Cons{&Atom{"y"}, &Atom{"rest"}}},
			body:  &Atom{"rest"},

			arg: &Cons{&Atom{"one"}, &Cons{&Number{2}, Nil}},

			result: Nil,
		},
		"uses the closure scope": {
			param: &Cons{&Atom{"x"}, &Cons{&Atom{"y"}, &Atom{"rest"}}},
			body:  &Atom{"one"},

			arg: &Cons{&Atom{"one"}, &Cons{&Atom{"two"}, Nil}},

			result: &Number{111},
		},
		"too few arguments": {
			param: &Cons{&Atom{"x"}, &Cons{&Atom{"y"}, Nil}},
			body:  &Atom{"y"},

			arg: &Cons{&Number{1337}, Nil},

			err: NewEvalError("The function expects more arguments, 1 given"),
		},
		"too many arguments": {
			param: &Cons{&Atom{"x"}, &Cons{&Atom{"y"}, Nil}},
			body:  &Atom{"y"},

			arg: &Cons{&Atom{"one"}, &Cons{&Number{2}, &Cons{&Number{3}, &Cons{&Number{4}, Nil}}}},

			err: NewEvalError("The function expects fewer arguments, 4 given"),
		},
		"non-atom in parameter list": {
			param: &Cons{&Atom{"x"}, &Cons{&Number{15}, Nil}},
			body:  &Atom{"y"},

			arg: &Cons{&Number{1337}, Nil},

			err: NewEvalError(fmt.Sprintf("A list of function parameters must consist of atoms, got %v", &Number{15})),
		},
		"non-atom parameter list": {
			param: &Number{123},
			body:  &Number{456},

			arg: &Cons{&Number{1337}, Nil},

			err: NewEvalError(fmt.Sprintf("Function parameter must be either an atom or a list of atoms, got %v", &Number{123})),
		},
		"undefined atom in the body": {
			param: &Cons{&Atom{"x"}, Nil},
			body:  &Atom{"undefined"},

			arg: &Cons{&Number{1337}, Nil},

			err: NewEvalError(fmt.Sprintf("Undefined name: '%s'", &Atom{"undefined"})),
		},
	} {
		t.Run(name, func(t *testing.T) {
			closureEnv := NewEnv(nil)
			closureEnv.Set("one", &Number{111})
			closureEnv.Set("two", &Number{222})

			argEnv := NewEnv(nil)
			argEnv.Set("one", &Number{1})
			argEnv.Set("two", &Number{2})

			closure := &Closure{param: tc.param, body: tc.body, env: closureEnv}
			result, err := reduce(closure, tc.arg, argEnv)
			if err != tc.err {
				t.Errorf("Expected error %v, got %v", tc.err, err)
			}

			if err == nil && !result.Equal(tc.result) {
				t.Errorf("Expected result %v, got %v", result, tc.result)
			}
		})
	}
}

func TestEvalArg(t *testing.T) {
	slicesEqual := func(s1, s2 []Expr) bool {
		if len(s1) != len(s2) {
			return false
		}

		for i := range s1 {
			if !s1[i].Equal(s2[i]) {
				return false
			}
		}

		return true
	}

	t.Run("no error", func(t *testing.T) {
		env := NewEnv(nil)
		env.Set("fst", &Atom{"first atom"})
		env.Set("snd", &Number{127})

		for _, tc := range map[string]struct {
			arg   Expr
			slice []Expr
		}{
			"nil":           {Nil, []Expr{}},
			"single atom":   {&Atom{"fst"}, []Expr{&Atom{"first atom"}}},
			"single number": {&Cons{&Number{99}, Nil}, []Expr{&Number{99}}},
			"multiple atoms and numbers, proper list": {
				&Cons{&Number{99}, &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, Nil}}},
				[]Expr{&Number{99}, &Atom{"first atom"}, &Number{127}},
			},
			"multiple atoms and numbers, improper list": {
				&Cons{&Number{99}, &Cons{&Atom{"fst"}, &Atom{"snd"}}},
				[]Expr{&Number{99}, &Atom{"first atom"}, &Number{127}},
			},
		} {
			result, err := evalArg(tc.arg, env)
			if err != nil {
				t.Errorf("Expected nil error, got %v", err)
			}
			if !slicesEqual(tc.slice, result) {
				t.Errorf("evalArg(%v): expected %v, got %v", tc.arg, tc.slice, result)
			}
		}
	})

	t.Run("undefined symbol", func(t *testing.T) {
		env := NewEnv(nil)

		_, err := evalArg(&Cons{&Atom{"a"}, Nil}, env)
		if err == nil {
			t.Errorf("Expected error due to undefined \"a\", got no error")
		}
	})
}

func TestBind(t *testing.T) {
	for name, tc := range map[string]struct {
		param Expr
		args  []Expr

		paramsList   []string
		expectedVals []Expr
		err          error
	}{
		"no parameters": {
			param: Nil,
		},
		"single parameter": {
			param:        &Cons{&Atom{"fst"}, Nil},
			args:         []Expr{&Number{1}},
			paramsList:   []string{"fst"},
			expectedVals: []Expr{&Number{1}},
		},
		"multiple parameters": {
			param:        &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, Nil}},
			args:         []Expr{&Number{10}, &Number{20}},
			paramsList:   []string{"fst", "snd"},
			expectedVals: []Expr{&Number{10}, &Number{20}},
		},
		"improper parameter list": {
			param:        &Cons{&Atom{"head"}, &Atom{"tail"}},
			args:         []Expr{&Number{10}, &Number{20}},
			paramsList:   []string{"head", "tail"},
			expectedVals: []Expr{&Number{10}, &Cons{&Number{20}, Nil}},
		},
		"improper parameter list, empty tail": {
			param:        &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, &Atom{"rest"}}},
			args:         []Expr{&Number{10}, &Number{20}},
			paramsList:   []string{"fst", "snd", "rest"},
			expectedVals: []Expr{&Number{10}, &Number{20}, Nil},
		},
		"too few arguments 1": {
			param: &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, Nil}},
			args:  []Expr{&Number{1}},
			err:   NewEvalError("The function expects more arguments, 1 given"),
		},
		"too few arguments 2": {
			param: &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, &Cons{&Atom{"thd"}, &Cons{&Atom{"frh"}, Nil}}}},
			args:  []Expr{&Number{1}, &Number{2}},
			err:   NewEvalError("The function expects more arguments, 2 given"),
		},
		"too many arguments": {
			param: &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, Nil}},
			args:  []Expr{&Number{1}, &Number{2}, &Number{3}},
			err:   NewEvalError("The function expects fewer arguments, 3 given"),
		},
		"non-atom in parameter list": {
			param: &Cons{&Number{1}, Nil},
			err:   NewEvalError(fmt.Sprintf("A list of function parameters must consist of atoms, got %v", &Number{1})),
		},
		"non-atom parameter list": {
			param: &Number{1},
			err:   NewEvalError(fmt.Sprintf("Function parameter must be either an atom or a list of atoms, got %v", &Number{1})),
		},
		"empty parameter list, args given": {
			param: Nil,
			args:  []Expr{&Number{1}, &Number{2}},
			err:   NewEvalError("The function expects fewer arguments, 2 given"),
		},
	} {
		t.Run(name, func(t *testing.T) {
			env := NewEnv(nil)
			err := bind(tc.param, tc.args, env)
			if err != tc.err {
				t.Errorf("Expected error %v, got %v", tc.err, err)
			}

			if len(tc.paramsList) != len(tc.expectedVals) {
				t.Fatalf("Different length of the test case's paramsList (%d) and args (%d)", len(tc.paramsList), len(tc.args))
			}

			for i, param := range tc.paramsList {
				val, err := env.Get(&Atom{param})
				if err != nil {
					t.Errorf("The parameter %s is unbound", param)
				}

				if !val.Equal(tc.expectedVals[i]) {
					t.Errorf("The parameter %s is bound to %v, expected %v", param, val, tc.expectedVals[i])
				}
			}
		})
	}
}

func TestSliceToCons(t *testing.T) {
	for _, tc := range map[string]struct {
		slice []Expr
		cons  Expr
	}{
		"nil":            {[]Expr{}, Nil},
		"single element": {[]Expr{&Atom{"a"}}, &Cons{&Atom{"a"}, Nil}},
		"multiple elements": {
			[]Expr{&Atom{"a"}, &Atom{"b"}, &Atom{"c"}},
			&Cons{&Atom{"a"}, &Cons{&Atom{"b"}, &Cons{&Atom{"c"}, Nil}}},
		},
	} {
		cons := sliceToCons(tc.slice)
		if !cons.Equal(tc.cons) {
			t.Errorf("sliceToCons of %v: expected %v, got %v", tc.slice, tc.cons, cons)
		}
	}
}
