package vm

import (
	_ "embed"
	"strings"
	"testing"

	"tomaskala.com/glisp/internal/compiler"
	"tomaskala.com/glisp/internal/runtime"
)

func evaluate(evaluator *VM, source string) (runtime.Value, error) {
	p := compiler.NewParser("test", source)
	c := compiler.NewCompiler("test", evaluator)
	result := runtime.MakeNil()

	for !p.AtEOF() {
		expr, err := p.Expression()
		if err != nil {
			return result, err
		}

		program, err := c.Compile(expr)
		if err != nil {
			return result, err
		}

		result, err = evaluator.Run(program)
		if err != nil {
			return result, err
		}
	}

	return result, nil
}

func evalExpr(t *testing.T, source string) runtime.Value {
	t.Helper()

	evaluator := NewVM()
	_, err := evaluate(evaluator, runtime.Stdlib)
	if err != nil {
		t.Fatalf("loading stdlib failed: %v", err)
	}

	result, err := evaluate(evaluator, source)
	if err != nil {
		t.Fatalf("VM execution failed: %v", err)
	}

	// Assert VM state is clean after evaluation
	if evaluator.StackTop != 0 {
		t.Errorf("VM stack not empty after evaluation: %d items remaining", len(evaluator.Stack))
	}
	if evaluator.NumFrames != 0 {
		t.Errorf("VM frames not empty after evaluation: %d frames remaining", evaluator.NumFrames)
	}
	if len(evaluator.OpenUpvalues) != 0 {
		t.Errorf("VM open upvalues not empty after evaluation: %d upvalues remaining", len(evaluator.OpenUpvalues))
	}

	return result
}

func expectError(t *testing.T, source string, expectedErrorSubstring string) {
	t.Helper()

	evaluator := NewVM()
	_, err := evaluate(evaluator, runtime.Stdlib)
	if err != nil {
		t.Fatalf("loading stdlib failed: %v", err)
	}

	_, err = evaluate(evaluator, source)
	if err != nil {
		if strings.Contains(err.Error(), expectedErrorSubstring) {
			return
		}
		t.Errorf("Expected error containing '%s', got: %v", expectedErrorSubstring, err)
		return
	}

	t.Errorf("Expected error containing '%s', but evaluation succeeded", expectedErrorSubstring)
}

func TestBasicLiterals(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"nil", "()", "()"},
		{"number_int", "42", "42"},
		{"number_float", "3.14", "3.14"},
		{"number_negative", "-17", "-17"},
		{"atom", "'hello", "hello"},
		{"boolean_true", "#t", "#t"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestArithmetic(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"add_empty", "(+)", "0"},
		{"add_basic", "(+ 2 3)", "5"},
		{"add_multiple", "(+ 1 2 3 4)", "10"},
		{"add_single", "(+ 5)", "5"},
		{"sub_basic", "(- 10 3)", "7"},
		{"sub_multiple", "(- 20 5 3)", "12"},
		{"sub_negate", "(- 5)", "-5"},
		{"mul_empty", "(*)", "1"},
		{"mul_basic", "(* 4 5)", "20"},
		{"mul_multiple", "(* 2 3 4)", "24"},
		{"mul_single", "(* 7)", "7"},
		{"div_basic", "(/ 12 3)", "4"},
		{"div_multiple", "(/ 24 2 3)", "4"},
		{"div_invert", "(/ 4)", "0.25"},
		{"nested_arithmetic", "(+ (* 2 3) (/ 8 2))", "10"},
		{"complex_expression", "(- (* (+ 2 3) 4) (/ 10 2))", "15"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestArithmeticErrors(t *testing.T) {
	tests := []struct {
		name        string
		source      string
		expectedErr string
	}{
		{"sub_no_args", "(-)", "- expects at least 1 argument"},
		{"div_no_args", "(/)", "/ expects at least 1 argument"},
		{"add_non_number", "(+ 5 'hello)", "+ is only defined for numbers"},
		{"sub_non_number", "(- 10 'world)", "- is only defined for numbers"},
		{"mul_non_number", "(* 3 ())", "* is only defined for numbers"},
		{"div_non_number", "(/ 6 #t)", "/ is only defined for numbers"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			expectError(t, tt.source, tt.expectedErr)
		})
	}
}

func TestComparison(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"num_eq_true", "(= 5 5)", "#t"},
		{"num_eq_false", "(= 3 7)", "()"},
		{"num_lt_true", "(< 3 5)", "#t"},
		{"num_lt_false", "(< 7 2)", "()"},
		{"num_lt_equal", "(< 4 4)", "()"},
		{"num_lte_true", "(<= 3 5)", "#t"},
		{"num_lte_equal", "(<= 4 4)", "#t"},
		{"num_lte_false", "(<= 7 2)", "()"},
		{"num_gt_true", "(> 8 3)", "#t"},
		{"num_gt_false", "(> 2 9)", "()"},
		{"num_gte_true", "(>= 8 3)", "#t"},
		{"num_gte_equal", "(>= 5 5)", "#t"},
		{"num_gte_false", "(>= 2 9)", "()"},
		{"eq_same_atom", "(eq? 'hello 'hello)", "#t"},
		{"eq_different_atom", "(eq? 'hello 'world)", "()"},
		{"eq_same_number", "(eq? 42 42)", "#t"},
		{"eq_different_number", "(eq? 42 17)", "()"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestComparisonErrors(t *testing.T) {
	tests := []struct {
		name        string
		source      string
		expectedErr string
	}{
		{"eq_atom_number", "(= 'hello 5)", "only defined for numbers"},
		{"lt_atom_number", "(< 'world 10)", "only defined for numbers"},
		{"lte_mixed", "(<= () 5)", "only defined for numbers"},
		{"gt_mixed", "(> #t 3)", "only defined for numbers"},
		{"gte_mixed", "(>= 7 'test)", "only defined for numbers"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			expectError(t, tt.source, tt.expectedErr)
		})
	}
}

func TestPredicates(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"atom_pred_true", "(atom? 'hello)", "#t"},
		{"atom_pred_false_number", "(atom? 42)", "()"},
		{"atom_pred_false_nil", "(atom? ())", "()"},
		{"nil_pred_true", "(nil? ())", "#t"},
		{"nil_pred_false_atom", "(nil? 'hello)", "()"},
		{"nil_pred_false_number", "(nil? 42)", "()"},
		{"not_true", "(not ())", "#t"},
		{"not_false", "(not 'something)", "()"},
		{"not_false_number", "(not 42)", "()"},
		{"pair_pred_true", "(pair? (cons 1 2))", "#t"},
		{"pair_pred_false_atom", "(pair? 'hello)", "()"},
		{"pair_pred_false_nil", "(pair? ())", "()"},
		{"pair_pred_false_number", "(pair? 123)", "()"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestPairs(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"cons_basic", "(cons 1 2)", "(1 . 2)"},
		{"cons_list", "(cons 1 ())", "(1)"},
		{"cons_nested", "(cons 1 (cons 2 ()))", "(1 2)"},
		{"car_pair", "(car (cons 'a 'b))", "a"},
		{"cdr_pair", "(cdr (cons 'a 'b))", "b"},
		{"car_list", "(car (cons 1 (cons 2 ())))", "1"},
		{"cdr_list", "(cdr (cons 1 (cons 2 ())))", "(2)"},
		{"nested_car_cdr", "(car (cdr (cons 1 (cons 2 (cons 3 ())))))", "2"},
		{"three_element_list", "(cons 'a (cons 'b (cons 'c ())))", "(a b c)"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestPairErrors(t *testing.T) {
	tests := []struct {
		name        string
		source      string
		expectedErr string
	}{
		{"car_non_pair", "(car 'hello)", "car expects a pair"},
		{"car_nil", "(car ())", "car expects a pair"},
		{"car_number", "(car 42)", "car expects a pair"},
		{"cdr_non_pair", "(cdr 'world)", "cdr expects a pair"},
		{"cdr_nil", "(cdr ())", "cdr expects a pair"},
		{"cdr_number", "(cdr 123)", "cdr expects a pair"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			expectError(t, tt.source, tt.expectedErr)
		})
	}
}

func TestMutatingPairs(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"set_car", "((lambda (p) (begin (set-car! p 'new) p)) (cons 'old 'value))", "(new . value)"},
		{"set_cdr", "((lambda (p) (begin (set-cdr! p 'new) p)) (cons 'value 'old))", "(value . new)"},
		{"set_both", "((lambda (p) (begin (set-car! p 'x) (set-cdr! p 'y) p)) (cons 'a 'b))", "(x . y)"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestMutatingPairErrors(t *testing.T) {
	tests := []struct {
		name        string
		source      string
		expectedErr string
	}{
		{"set_car_non_pair", "(set-car! 'hello 'world)", "set-car! expects a pair"},
		{"set_car_nil", "(set-car! () 'value)", "set-car! expects a pair"},
		{"set_cdr_non_pair", "(set-cdr! 42 'value)", "set-cdr! expects a pair"},
		{"set_cdr_nil", "(set-cdr! () 'value)", "set-cdr! expects a pair"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			expectError(t, tt.source, tt.expectedErr)
		})
	}
}

func TestFunctions(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"simple_lambda", "((lambda (x) x) 42)", "42"},
		{"lambda_addition", "((lambda (x y) (+ x y)) 3 4)", "7"},
		{"nested_lambda", "((lambda (x) ((lambda (y) (+ x y)) 5)) 3)", "8"},
		{"lambda_no_params", "((lambda () 'hello))", "hello"},
		{"lambda_multiple_body", "((lambda (x) (begin (+ x 1) (* x 2))) 5)", "10"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestVariadicFunctions(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"variadic_no_rest", "((lambda (x . rest) x) 1 2 3)", "1"},
		{"variadic_with_rest", "((lambda (x . rest) rest) 1 2 3)", "(2 3)"},
		{"variadic_only_rest", "((lambda rest rest) 1 2 3)", "(1 2 3)"},
		{"variadic_empty_rest", "((lambda (x . rest) rest) 1)", "()"},
		{"variadic_process_rest", "((lambda (x . rest) (car rest)) 1 2 3)", "2"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestFunctionErrors(t *testing.T) {
	tests := []struct {
		name        string
		source      string
		expectedErr string
	}{
		{"too_few_args", "((lambda (x y) (+ x y)) 1)", "expects 2 arguments, got 1"},
		{"too_many_args", "((lambda (x) x) 1 2)", "expects 1 arguments, got 2"},
		{"variadic_too_few", "((lambda (x y . rest) x) 1)", "expects at least 2 arguments, got 1"},
		{"call_non_function", "(42 1 2)", "attempting to call 42"},
		{"call_atom", "('hello)", "attempting to call hello"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			expectError(t, tt.source, tt.expectedErr)
		})
	}
}

func TestDefinitions(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"define_simple", "(define x 42) x", "42"},
		{"define_function", "(define f (lambda (x) (* x 2))) (f 5)", "10"},
		{"define_multiple", "(define a 1) (define b 2) (+ a b)", "3"},
		{"redefine", "(define x 10) (define x 20) x", "20"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestVariableErrors(t *testing.T) {
	tests := []struct {
		name        string
		source      string
		expectedErr string
	}{
		{"undefined_var", "nonexistent", "undefined variable: nonexistent"},
		{"set_undefined", "(set! undefined 42)", "undefined variable: undefined"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			expectError(t, tt.source, tt.expectedErr)
		})
	}
}

func TestSetBang(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"set_global", "(define x 1) (set! x 2) x", "2"},
		{"set_in_function", "(define x 1) ((lambda () (begin (set! x 10) x)))", "10"},
		{"set_local", "((lambda (x) (begin (set! x 20) x)) 5)", "20"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestClosures(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"simple_closure", "((lambda (x) (lambda (y) (+ x y))) 3)", "<lambda closure>"},
		{"closure_call", "(((lambda (x) (lambda (y) (+ x y))) 3) 4)", "7"},
		{"closure_multiple_vars", "(((lambda (x y) (lambda (z) (+ x y z))) 1 2) 3)", "6"},
		{"nested_closures", "((((lambda (a) (lambda (b) (lambda (c) (+ a b c)))) 1) 2) 3)", "6"},
		{"closure_modification", "((lambda (x) ((lambda () (begin (set! x 10) x)))) 5)", "10"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestConditionals(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"if_true", "(if #t 'yes 'no)", "yes"},
		{"if_false", "(if () 'yes 'no)", "no"},
		{"if_truthy", "(if 42 'yes 'no)", "yes"},
		{"if_nested", "(if (< 3 5) (if #t 'inner-yes 'inner-no) 'outer-no)", "inner-yes"},
		{"cond_first", "(cond (#t 'first) (() 'second))", "first"},
		{"cond_second", "(cond (() 'first) (#t 'second))", "second"},
		{"cond_none", "(cond (() 'first) (() 'second))", "()"},
		{"cond_multiple", "(cond ((< 1 0) 'negative) ((= 1 1) 'equal) (#t 'default))", "equal"},
		{"and_empty", "(and)", "#t"},
		{"and_single_true", "(and #t)", "#t"},
		{"and_single_false", "(and ())", "()"},
		{"and_all_true", "(and #t 'hello 42)", "42"},
		{"and_with_false", "(and #t () 'never)", "()"},
		{"and_short_circuit", "(and () (/ 1 0))", "()"},
		{"or_empty", "(or)", "()"},
		{"or_single_true", "(or #t)", "#t"},
		{"or_single_false", "(or ())", "()"},
		{"or_first_true", "(or 'first ())", "first"},
		{"or_second_true", "(or () 'second)", "second"},
		{"or_short_circuit", "(or 'found (/ 1 0))", "found"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestBegin(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"begin_single", "(begin 42)", "42"},
		{"begin_multiple", "(begin 1 2 3)", "3"},
		{"begin_with_effects", "(define x 0) (begin (set! x 1) (set! x 2) x)", "2"},
		{"begin_nested", "(begin (begin 1 2) (begin 3 4))", "4"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestLet(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"let_basic", "(let ((x 1) (y 2)) (+ x y))", "3"},
		{"let_empty", "(let () 42)", "42"},
		{"let_shadowing", "(define x 10) (let ((x 1)) x)", "1"},
		{"let_star_sequential", "(let* ((x 1) (y (+ x 1))) (+ x y))", "3"},
		{"let_star_empty", "(let* () 'empty)", "empty"},
		{"letrec_recursive", "(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (fact 5))", "120"},
		{
			"letrec_mutual",
			"(letrec ((even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))) (odd? (lambda (n) (if (= n 0) () (even? (- n 1)))))) (even? 4))",
			"#t",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestTailCallOptimization(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"tail_recursive_countdown", `
			(define countdown 
			  (lambda (n acc) 
			    (if (= n 0) 
			        acc 
			        (countdown (- n 1) (+ acc 1)))))
			(countdown 1500 0)`, "1500"},
		{"tail_recursive_factorial", `
			(define fact-tail
			  (lambda (n acc)
			    (if (= n 0)
			        acc
			        (fact-tail (- n 1) (* n acc)))))
			(fact-tail 6 1)`, "720"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestQuoting(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"quote_atom", "'hello", "hello"},
		{"quote_number", "'42", "42"},
		{"quote_nil", "'()", "()"},
		{"quote_list", "'(1 2 3)", "(1 2 3)"},
		{"quote_nested", "'(a (b c) d)", "(a (b c) d)"},
		{"quote_complex", "'(lambda (x) (+ x 1))", "(lambda (x) (+ x 1))"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestComplexPrograms(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"fibonacci", `
			(define fib
			  (lambda (n)
			    (if (<= n 1)
			        n
			        (+ (fib (- n 1)) (fib (- n 2))))))
			(fib 8)`, "21"},

		{"list_length", `
			(define length
			  (lambda (lst)
			    (if (nil? lst)
			        0
			        (+ 1 (length (cdr lst))))))
			(length '(a b c d e))`, "5"},

		{"higher_order", `
			(define map
			  (lambda (f lst)
			    (if (nil? lst)
			        ()
			        (cons (f (car lst)) (map f (cdr lst))))))
			(define square (lambda (x) (* x x)))
			(map square '(1 2 3 4))`, "(1 4 9 16)"},

		{"closure_counter", `
			(define make-counter
			  (lambda (start)
			    (lambda ()
				  (begin
			        (set! start (+ start 1))
			        start))))
			(define counter (make-counter 0))
			(begin (counter) (counter) (counter))`, "3"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestVMEdgeCases(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"multiple_expressions", "1 2 3", "3"},
		{"nested_quotes", "''hello", "(quote hello)"},
		{"zero_args_variadic", "((lambda args args))", "()"},
		{"complex_let_nesting", `
			(let ((x 1))
			  (let ((y 2))
			    (let ((z 3))
			      (+ x y z))))`, "6"},
		{"mixed_arithmetic", "(+ 1.5 2.5 (- 5 2) (* 2 1.5))", "10"},
		{"boolean_in_arithmetic", "(if #t (+ 1 2) (+ 3 4))", "3"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestStackOverflowPrevention(t *testing.T) {
	t.Run("deep_recursion", func(t *testing.T) {
		// This should cause a stack overflow and be caught
		source := `
			(define infinite-loop
			  (lambda (n)
			    (+ 1 (infinite-loop n))))
			(infinite-loop 0)`
		expectError(t, source, "stack overflow")
	})
}

func TestNativeFunctionsCoverage(t *testing.T) {
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"native_true", "#t", "#t"},
		{"native_cons", "(cons 'a 'b)", "(a . b)"},
		{"native_car", "(car (cons 'first 'second))", "first"},
		{"native_cdr", "(cdr (cons 'first 'second))", "second"},
		{"native_add", "(+ 1 2 3)", "6"},
		{"native_sub", "(- 10 3 2)", "5"},
		{"native_mul", "(* 2 3 4)", "24"},
		{"native_div", "(/ 12 3 2)", "2"},
		{"native_num_eq", "(= 5 5)", "#t"},
		{"native_num_lt", "(< 3 5)", "#t"},
		{"native_num_lte", "(<= 5 5)", "#t"},
		{"native_num_gt", "(> 7 3)", "#t"},
		{"native_num_gte", "(>= 5 5)", "#t"},
		{"native_eq", "(eq? 'same 'same)", "#t"},
		{"native_atom_pred", "(atom? 'hello)", "#t"},
		{"native_nil_pred", "(nil? ())", "#t"},
		{"native_pair_pred", "(pair? (cons 1 2))", "#t"},
		{"native_not", "(not ())", "#t"},
		{"native_set_car", "((lambda (p) (begin (set-car! p 'new) (car p))) (cons 'old 'value))", "new"},
		{"native_set_cdr", "((lambda (p) (begin (set-cdr! p 'new) (cdr p))) (cons 'value 'old))", "new"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestMemoryAndGarbageCollection(t *testing.T) {
	// Test that closures properly capture and close upvalues
	tests := []struct {
		name     string
		source   string
		expected string
	}{
		{"closure_captures_local", `
			(define make-adder
			  (lambda (n)
			    (lambda (x) (+ x n))))
			(define add5 (make-adder 5))
			(add5 10)`, "15"},

		{"multiple_closures_same_var", `
			(define make-funcs
			  (lambda (n)
			    (cons
			      (lambda () n)
			      (lambda (new-n) (begin (set! n new-n) n)))))
			(define funcs (make-funcs 100))
			(define getter (car funcs))
			(define setter (cdr funcs))
			(setter 200)
			(getter)`, "200"},

		{"closure_chain", `
			(define make-chain
			  (lambda (a)
			    (lambda (b)
			      (lambda (c)
			        (+ a b c)))))
			(((make-chain 1) 2) 3)`, "6"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := evalExpr(t, tt.source)
			if result.String() != tt.expected {
				t.Errorf("Expected %s, got %s", tt.expected, result.String())
			}
		})
	}
}

func TestErrorRecovery(t *testing.T) {
	// Test that VM properly resets state after errors
	t.Run("error_recovery", func(t *testing.T) {
		evaluator := NewVM()
		_, err := evaluate(evaluator, runtime.Stdlib)
		if err != nil {
			t.Fatalf("loading stdlib failed: %v", err)
		}

		// First, cause an error
		program1 := "a" // References an undefined atom.

		_, err = evaluate(evaluator, program1)
		if err == nil {
			t.Error("Expected division by zero error")
		}

		// VM should be reset and ready for next program
		if evaluator.StackTop != 0 {
			t.Errorf("VM stack not reset after error: %d items", len(evaluator.Stack))
		}
		if evaluator.NumFrames != 0 {
			t.Errorf("VM frames not reset after error: %d frames", evaluator.NumFrames)
		}

		// Should be able to run another program successfully
		program2 := "(+ 2 3)"

		result, err := evaluate(evaluator, program2)
		if err != nil {
			t.Errorf("Unexpected error on second program: %v", err)
		}
		if result.String() != "5" {
			t.Errorf("Expected 5, got %s", result.String())
		}
	})
}

func TestProgramsWithSideEffects(t *testing.T) {
	// Test display function and other side effects
	t.Run("display_function", func(t *testing.T) {
		result := evalExpr(t, "(display 'hello)")
		if result.String() != "()" {
			t.Errorf("Expected display to return (), got %s", result.String())
		}
	})

	t.Run("newline_function", func(t *testing.T) {
		result := evalExpr(t, "(newline)")
		if result.String() != "()" {
			t.Errorf("Expected newline to return (), got %s", result.String())
		}
	})

	t.Run("multiple_side_effects", func(t *testing.T) {
		source := `
			(define x 0)
			(begin 
			  (set! x (+ x 1))
			  (display x)
			  (newline)
			  (set! x (+ x 1))
			  (display x)
			  (set! x (+ x 1))
			  x)`
		result := evalExpr(t, source)
		if result.String() != "3" {
			t.Errorf("Expected 3, got %s", result.String())
		}
	})
}
