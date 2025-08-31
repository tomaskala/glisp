package glisp

import (
	"fmt"
	"testing"
)

func TestAtomEval(t *testing.T) {
	e := NewEvaluator("TestAtomEval", nil)

	trueEval, err := e.Eval(True)
	if err != nil {
		t.Fatalf("Error when evaluating True: %v", err)
	}

	if trueEval != True {
		t.Errorf("True evaluated to something else than itself: %v", trueEval)
	}

	a := &Atom{"atom"}
	env := make(map[string]Expr)
	env["atom"] = a
	e.pushFrame(Frame{&Closure{name: "TestAtomEval"}, env})

	aEval, err := e.Eval(a)
	if err != nil {
		t.Fatalf("Error when evaluating an atom: %v", err)
	}

	if aEval != a {
		t.Errorf("Atom evaluated to something else than itself: %v", aEval)
	}

	b := &Atom{"another"}
	_, err = e.Eval(b)
	expectedErr := NewRuntimeError("TestAtomEval", "Undefined name: 'another'", nil)
	if !runtimeErrorsEqual(err, expectedErr) {
		t.Errorf("Expected error %v when evaluating an undefined atom, got %v", expectedErr, err)
	}
}

func TestBuiltinEval(t *testing.T) {
	e := NewEvaluator("TestBuiltinEval", nil)
	a := &Builtin{"first", func(e Expr, evaluator *Evaluator) (Expr, error) { return e, nil }}

	aEval, err := e.Eval(a)
	if err != nil {
		t.Fatalf("Error when evaluating a builtin: %v", err)
	}

	if aEval != a {
		t.Errorf("Builtin evaluated to something else than itself: %v", aEval)
	}
}

func TestClosureEval(t *testing.T) {
	e := NewEvaluator("TestClosureEval", nil)

	env := make(map[string]Expr)
	a := &Closure{"", Nil, Nil, env}

	aEval, err := e.Eval(a)
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

			err: NewRuntimeError("TestReduce", "The function expects more arguments, 1 given", nil),
		},
		"too many arguments": {
			param: &Cons{&Atom{"x"}, &Cons{&Atom{"y"}, Nil}},
			body:  &Atom{"y"},

			arg: &Cons{&Atom{"one"}, &Cons{&Number{2}, &Cons{&Number{3}, &Cons{&Number{4}, Nil}}}},

			err: NewRuntimeError("TestReduce", "The function expects fewer arguments, 4 given", nil),
		},
		"non-atom in parameter list": {
			param: &Cons{&Atom{"x"}, &Cons{&Number{15}, Nil}},
			body:  &Atom{"y"},

			arg: &Cons{&Number{1337}, Nil},

			err: NewRuntimeError("TestReduce", fmt.Sprintf("A list of function parameters must consist of atoms, got %v", &Number{15}), nil),
		},
		"non-atom parameter list": {
			param: &Number{123},
			body:  &Number{456},

			arg: &Cons{&Number{1337}, Nil},

			err: NewRuntimeError("TestReduce", fmt.Sprintf("Function parameter must be either an atom or a list of atoms, got %v", &Number{123}), nil),
		},
		"undefined atom in the body": {
			param: &Cons{&Atom{"x"}, Nil},
			body:  &Atom{"undefined"},

			arg: &Cons{&Number{1337}, Nil},

			err: NewRuntimeError("TestReduce", fmt.Sprintf("Undefined name: '%s'", &Atom{"undefined"}), nil),
		},
	} {
		t.Run(name, func(t *testing.T) {
			e := NewEvaluator("TestReduce", nil)
			closureEnv := make(map[string]Expr)
			closureEnv["one"] = &Number{111}
			closureEnv["two"] = &Number{222}

			argEnv := make(map[string]Expr)
			argEnv["one"] = &Number{1}
			argEnv["two"] = &Number{2}
			e.pushFrame(Frame{&Closure{name: "TestReduceArg"}, argEnv})

			closure := &Closure{param: tc.param, body: tc.body, captured: closureEnv}
			result, err := e.reduce(closure, tc.arg)
			if !runtimeErrorsEqual(err, tc.err) {
				t.Errorf("Expected error %v, got %v", tc.err, err)
			}

			if err == nil && !result.Equal(tc.result) {
				t.Errorf("Expected result %v, got %v", result, tc.result)
			}
		})
	}
}

func TestEvalList(t *testing.T) {
	t.Run("no error", func(t *testing.T) {
		env := make(map[string]Expr)
		env["fst"] = &Atom{"first-atom"}
		env["snd"] = &Number{127}
		e := NewEvaluator("TestEvalArg", nil)
		e.pushFrame(Frame{&Closure{name: "TestEvalArg"}, env})

		for _, tc := range map[string]struct {
			arg   Expr
			slice Expr
		}{
			"nil":           {Nil, Nil},
			"single atom":   {&Atom{"fst"}, &Atom{"first-atom"}},
			"single number": {&Cons{&Number{99}, Nil}, &Cons{&Number{99}, Nil}},
			"multiple atoms and numbers, proper list": {
				&Cons{&Number{99}, &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, Nil}}},
				&Cons{&Number{99}, &Cons{&Atom{"first-atom"}, &Cons{&Number{127}, Nil}}},
			},
			"multiple atoms and numbers, improper list": {
				&Cons{&Number{99}, &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, Nil}}},
				&Cons{&Number{99}, &Cons{&Atom{"first-atom"}, &Cons{&Number{127}, Nil}}},
			},
		} {
			result, err := e.evalList(tc.arg)
			if err != nil {
				t.Errorf("Expected nil error, got %v", err)
			}
			if !tc.slice.Equal(result) {
				t.Errorf("evalArg(%v): expected %v, got %v", tc.arg, tc.slice, result)
			}
		}
	})

	t.Run("undefined symbol", func(t *testing.T) {
		e := NewEvaluator("TestEvalArg", nil)
		_, err := e.evalList(&Cons{&Atom{"a"}, Nil})
		expectedErr := NewRuntimeError("TestEvalArg", "Undefined name: 'a'", nil)
		if !runtimeErrorsEqual(err, expectedErr) {
			t.Errorf("Expected error due to undefined \"a\", got no error")
		}
	})
}

func TestBind(t *testing.T) {
	for name, tc := range map[string]struct {
		param Expr
		args  Expr

		paramsList   []string
		expectedVals []Expr
		err          error
	}{
		"no parameters": {
			param: Nil,
			args:  Nil,
		},
		"single parameter": {
			param:        &Cons{&Atom{"fst"}, Nil},
			args:         &Cons{&Number{1}, Nil},
			paramsList:   []string{"fst"},
			expectedVals: []Expr{&Number{1}},
		},
		"multiple parameters": {
			param:        &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, Nil}},
			args:         &Cons{&Number{10}, &Cons{&Number{20}, Nil}},
			paramsList:   []string{"fst", "snd"},
			expectedVals: []Expr{&Number{10}, &Number{20}},
		},
		"improper parameter list": {
			param:        &Cons{&Atom{"head"}, &Atom{"tail"}},
			args:         &Cons{&Number{10}, &Cons{&Number{20}, Nil}},
			paramsList:   []string{"head", "tail"},
			expectedVals: []Expr{&Number{10}, &Cons{&Number{20}, Nil}},
		},
		"improper parameter list, empty tail": {
			param:        &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, &Atom{"rest"}}},
			args:         &Cons{&Number{10}, &Cons{&Number{20}, Nil}},
			paramsList:   []string{"fst", "snd", "rest"},
			expectedVals: []Expr{&Number{10}, &Number{20}, Nil},
		},
		"too few arguments 1": {
			param: &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, Nil}},
			args:  &Cons{&Number{1}, Nil},
			err:   NewRuntimeError("TestBind", "The function expects more arguments, 1 given", nil),
		},
		"too few arguments 2": {
			param: &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, &Cons{&Atom{"thd"}, &Cons{&Atom{"frh"}, Nil}}}},
			args:  &Cons{&Number{1}, &Cons{&Number{2}, Nil}},
			err:   NewRuntimeError("TestBind", "The function expects more arguments, 2 given", nil),
		},
		"too many arguments": {
			param: &Cons{&Atom{"fst"}, &Cons{&Atom{"snd"}, Nil}},
			args:  &Cons{&Number{1}, &Cons{&Number{2}, &Cons{&Number{3}, Nil}}},
			err:   NewRuntimeError("TestBind", "The function expects fewer arguments, 3 given", nil),
		},
		"non-atom in parameter list": {
			param: &Cons{&Number{1}, Nil},
			err:   NewRuntimeError("TestBind", fmt.Sprintf("A list of function parameters must consist of atoms, got %v", &Number{1}), nil),
		},
		"non-atom parameter list": {
			param: &Number{1},
			err:   NewRuntimeError("TestBind", fmt.Sprintf("Function parameter must be either an atom or a list of atoms, got %v", &Number{1}), nil),
		},
		"empty parameter list, args given": {
			param: Nil,
			args:  &Cons{&Number{1}, &Cons{&Number{2}, Nil}},
			err:   NewRuntimeError("TestBind", "The function expects fewer arguments, 2 given", nil),
		},
	} {
		t.Run(name, func(t *testing.T) {
			e := NewEvaluator("TestBind", nil)
			err := e.bind(tc.param, tc.args)
			if !runtimeErrorsEqual(err, tc.err) {
				t.Errorf("Expected error %v, got %v", tc.err, err)
			}

			for i, param := range tc.paramsList {
				val, err := e.lookup(&Atom{param})
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

func TestConsEval(t *testing.T) {
	e := NewEvaluator("TestConsEval", nil)

	trueEval, err := e.Eval(True)
	if err != nil {
		t.Fatalf("Error when evaluating True: %v", err)
	}

	if trueEval != True {
		t.Errorf("True evaluated to something else than itself: %v", trueEval)
	}

	a := &Atom{"atom"}
	env := make(map[string]Expr)
	env["atom"] = a
	e.pushFrame(Frame{&Closure{name: "TestConsEval"}, env})

	aEval, err := e.Eval(a)
	if err != nil {
		t.Fatalf("Error when evaluating an atom: %v", err)
	}

	if aEval != a {
		t.Errorf("Atom evaluated to something else than itself: %v", aEval)
	}

	b := &Atom{"another"}
	_, err = e.Eval(b)
	expectedErr := NewRuntimeError("TestConsEval", "Undefined name: 'another'", nil)
	if err == nil {
		t.Errorf("Expected error %v when evaluating an undefined atom, got %v", expectedErr, err)
	}
}

func TestNilEval(t *testing.T) {
	e := NewEvaluator("TestNilEval", nil)
	n := Nil

	nEval, err := e.Eval(n)
	if err != nil {
		t.Fatalf("Error when evaluating nil: %v", err)
	}

	if nEval != Nil {
		t.Errorf("Nil evaluated to something else than itself: %v", nEval)
	}
}

func TestNumberEval(t *testing.T) {
	e := NewEvaluator("TestNumberEval", nil)
	a := &Number{123}

	aEval, err := e.Eval(a)
	if err != nil {
		t.Fatalf("Error when evaluating a number: %v", err)
	}

	if aEval != a {
		t.Errorf("Number evaluated to something else than itself: %v", aEval)
	}
}

func runtimeErrorsEqual(err1, err2 error) bool {
	if err1 == nil && err2 == nil {
		return true
	}
	runtimeError1, ok := err1.(RuntimeError)
	if !ok {
		return false
	}
	runtimeError2, ok := err2.(RuntimeError)
	if !ok {
		return false
	}
	return runtimeError1.name == runtimeError2.name && runtimeError1.msg == runtimeError2.msg
}
