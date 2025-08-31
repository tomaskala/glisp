package glisp

import (
	"errors"
	"io"
	"testing"
)

func TestBuiltins(t *testing.T) {
	for name, tc := range map[string]struct {
		source string
		result string
		err    error
	}{
		// Eval & quote
		"eval1":  {"(eval (quote (+ 1 2)))", "3", nil},
		"eval2":  {"(eval (cons + '(1 2 3)))", "6", nil},
		"quote1": {"(quote (1 2 3))", "(1 2 3)", nil},
		"quote2": {"'(1 2 3)", "(1 2 3)", nil},

		// Lists
		"cons1": {"(cons 1 ())", "(1)", nil},
		"cons2": {"(cons 1 2)", "(1 . 2)", nil},
		"cons3": {"(cons 1 (cons 2 ()))", "(1 2)", nil},
		"car1":  {"(car '(1 2))", "1", nil},
		"car2":  {"(car '(1 . 2))", "1", nil},
		"cdr1":  {"(cdr '(1 2))", "(2)", nil},
		"cdr2":  {"(cdr '(1 . 2))", "2", nil},

		// Numbers
		"add1":        {"(+ 1 2 3 4)", "10", nil},
		"add2":        {"(+)", "0", nil},
		"add-define1": {"(define a 1) (define b 2) (+ a b)", "3", nil},
		"add-define2": {"(define nums '(1 2 3 4)) (+ . nums)", "10", nil},
		"sub1":        {"(- 3 2)", "1", nil},
		"sub2":        {"(-)", "", NewRuntimeError("sub2", "'-' requires at least one argument", nil)},
		"sub-define1": {"(define a 1) (define b 2) (- a b)", "-1", nil},
		"sub-define2": {"(define nums '(1 2 3 4)) (- . nums)", "-8", nil},
		"sub unary":   {"(- 3)", "-3", nil},
		"mul1":        {"(* 1 2 3 4)", "24", nil},
		"mul2":        {"(*)", "1", nil},
		"mul-define1": {"(define a 4) (define b 8) (* a b)", "32", nil},
		"mul-define2": {"(define nums '(1 2 3 4)) (* . nums)", "24", nil},
		"div1":        {"(/ 4 2)", "2", nil},
		"div unary":   {"(/ 4)", "0.25", nil},
		"div2":        {"(/)", "", NewRuntimeError("div2", "'/' requires at least one argument", nil)},
		"div-define1": {"(define a 8) (define b 4) (/ a b)", "2", nil},
		"div-define2": {"(define nums '(1 2 4)) (/ . nums)", "0.125", nil},
		"int":         {"(int 2.35)", "2", nil},

		// Logic
		"eq?1": {"(eq? 'a 'a)", "#t", nil},
		"eq?2": {"(eq? '(a) '(a))", "()", nil},
		"eq?3": {"(eq? 1 1)", "#t", nil},
		"eq?4": {"(eq? 1 2)", "()", nil},
		"lt1":  {"(< 1 1)", "()", nil},
		"lt2":  {"(< 1 2)", "#t", nil},
		"lt3":  {"(< 2 1)", "()", nil},
		"and1": {"(and #t ())", "()", nil},
		"and2": {"(and 1 2)", "2", nil},
		"or1":  {"(or #t ())", "#t", nil},
		"or2":  {"(or 1 2)", "1", nil},

		// Conditions
		"cond1": {"(cond ((eq? 'a 'b) 1) ((< 2 1) 2) (#t 3))", "3", nil},
		"if1":   {"(if (eq? 'a 'a) 1 2)", "1", nil},

		// Let* binding
		"let*1": {"(let* ((a 3) (b (* a a))) (+ a b))", "12", nil},
		"let*2": {"(let* ((a 3)) a)", "3", nil},
		"let*3": {"(let*)", "", NewRuntimeError("let*3", "'let*' expects arguments", nil)},
		"let*4": {"(let* ((3 4)) 2)", "", NewRuntimeError("let*4", "'let*' expects binding names to be atoms", nil)},
		"let*5": {"(let* ((3 a)) 2)", "", NewRuntimeError("let*5", "'let*' expects binding names to be atoms", nil)},
		"let*6": {"(let* ((a 3)) 1)", "1", nil},
		"let*7": {"(let* ((a 3)))", "", NewRuntimeError("let*7", "'let*' expects body", nil)},

		// Closures
		"lambda1": {"((lambda (x) (* x x)) 3)", "9", nil},
		"lambda2": {"((lambda (x y . args) args) 1 2 3 4)", "(3 4)", nil},
		"lambda3": {"((lambda (x y) (cons y (cons x ()))) 1 2)", "(2 1)", nil},

		// Definitions
		"define1": {"(define val 10) val", "10", nil},
		"define2": {"(define square (lambda (x) (* x x))) (square 3)", "9", nil},
		"define3": {"(define factorial (lambda (n) (if (< 1 n) (* n (factorial (- n 1))) 1))) (factorial 5)", "120", nil},
		"define4": {"(define list (lambda args args)) (list 10 20 30)", "(10 20 30)", nil},

		// Lexical scoping
		"one-level adder": {"(define adder (lambda (x) (lambda (y) (+ x y)))) ((adder 4) 5)", "9", nil},
		"two-level adder": {"(define adder (lambda (x) (lambda (y) (lambda (z) (+ x y z))))) (((adder 4) 5) 6)", "15", nil},

		// Basic Data Types and Constants
		"constant_true":   {"#t", "#t", nil},
		"constant_number": {"42", "42", nil},
		"constant_float":  {"3.14", "3.14", nil},
		"constant_nil":    {"()", "()", nil},

		// Quote Function
		"quote_atom":    {"(quote hello)", "hello", nil},
		"quote_list":    {"(quote (1 2 3))", "(1 2 3)", nil},
		"quote_nil":     {"(quote ())", "()", nil},
		"quote_true":    {"(quote #t)", "#t", nil},
		"quote_no_args": {"(quote)", "", NewRuntimeError("quote_no_args", "'quote' expects arguments", nil)},

		// Cons, Car, and Cdr
		"cons_basic":      {"(cons 1 2)", "(1 . 2)", nil},
		"cons_with_nil":   {"(cons 1 ())", "(1)", nil},
		"cons_nil_first":  {"(cons () 1)", "(() . 1)", nil},
		"cons_nested":     {"(cons 1 (cons 2 3))", "(1 2 . 3)", nil},
		"cons_list":       {"(cons 1 (cons 2 ()))", "(1 2)", nil},
		"cons_no_args":    {"(cons)", "", NewRuntimeError("cons_no_args", "'cons' expects two arguments", nil)},
		"cons_one_arg":    {"(cons 1)", "", NewRuntimeError("cons_one_arg", "'cons' expects two arguments", nil)},
		"cons_three_args": {"(cons 1 2 3)", "", NewRuntimeError("cons_three_args", "'cons' expects two arguments", nil)},

		"car_basic":    {"(car (cons 1 2))", "1", nil},
		"car_list":     {"(car (quote (a b c)))", "a", nil},
		"car_nil":      {"(car ())", "", NewRuntimeError("car_nil", "'car' expects a cons cell as its argument", nil)},
		"car_number":   {"(car 42)", "", NewRuntimeError("car_number", "'car' expects a cons cell as its argument", nil)},
		"car_no_args":  {"(car)", "", NewRuntimeError("car_no_args", "'car' expects one argument", nil)},
		"car_two_args": {"(car (cons 1 2) 'extra)", "", NewRuntimeError("car_two_args", "'car' expects one argument", nil)},

		"cdr_basic":   {"(cdr (cons 1 2))", "2", nil},
		"cdr_list":    {"(cdr (quote (a b c)))", "(b c)", nil},
		"cdr_single":  {"(cdr (quote (a)))", "()", nil},
		"cdr_nil":     {"(cdr ())", "", NewRuntimeError("cdr_nil", "'cdr' expects a cons cell as its argument", nil)},
		"cdr_number":  {"(cdr 42)", "", NewRuntimeError("cdr_number", "'cdr' expects a cons cell as its argument", nil)},
		"cdr_no_args": {"(cdr)", "", NewRuntimeError("cdr_no_args", "'cdr' expects one argument", nil)},

		// Arithmetic Functions - Addition
		"add_empty":     {"(+)", "0", nil},
		"add_single":    {"(+ 5)", "5", nil},
		"add_two":       {"(+ 1 2)", "3", nil},
		"add_multiple":  {"(+ 1 2 3 4)", "10", nil},
		"add_negative":  {"(+ -5 3)", "-2", nil},
		"add_float":     {"(+ 1.5 2.5)", "4", nil},
		"add_zeros":     {"(+ 0 0 0)", "0", nil},
		"add_with_atom": {"(+ 1 'hello)", "", NewRuntimeError("add_with_atom", "Expected a number, got hello", nil)},

		// Arithmetic Functions - Subtraction
		"sub_empty":     {"(-)", "", NewRuntimeError("sub_empty", "'-' requires at least one argument", nil)},
		"sub_unary":     {"(- 5)", "-5", nil},
		"sub_two":       {"(- 10 3)", "7", nil},
		"sub_multiple":  {"(- 10 3 2)", "5", nil},
		"sub_zero":      {"(- 0)", "0", nil},
		"sub_negative":  {"(- -5)", "5", nil},
		"sub_float":     {"(- 1.5 0.5)", "1", nil},
		"sub_with_atom": {"(- 5 'hello)", "", NewRuntimeError("sub_with_atom", "Expected a number, got hello", nil)},

		// Arithmetic Functions - Multiplication
		"mul_empty":     {"(*)", "1", nil},
		"mul_single":    {"(* 5)", "5", nil},
		"mul_two":       {"(* 2 3)", "6", nil},
		"mul_multiple":  {"(* 2 3 4)", "24", nil},
		"mul_negative":  {"(* -2 3)", "-6", nil},
		"mul_zero":      {"(* 0 100)", "0", nil},
		"mul_float":     {"(* 1.5 2)", "3", nil},
		"mul_with_atom": {"(* 2 'hello)", "", NewRuntimeError("mul_with_atom", "Expected a number, got hello", nil)},

		// Arithmetic Functions - Division
		"div_empty":      {"(/)", "", NewRuntimeError("div_empty", "'/' requires at least one argument", nil)},
		"div_unary":      {"(/ 4)", "0.25", nil},
		"div_two":        {"(/ 8 2)", "4", nil},
		"div_multiple":   {"(/ 12 3 2)", "2", nil},
		"div_fraction":   {"(/ 1 4)", "0.25", nil},
		"div_negative":   {"(/ -6 2)", "-3", nil},
		"div_by_zero":    {"(/ 5 0)", "", NewRuntimeError("div_by_zero", "Division by zero", nil)},
		"div_zero_unary": {"(/ 0)", "", NewRuntimeError("div_zero_unary", "Division by zero", nil)},
		"div_zero_multi": {"(/ 10 2 0)", "", NewRuntimeError("div_zero_multi", "Division by zero", nil)},
		"div_with_atom":  {"(/ 5 'hello)", "", NewRuntimeError("div_with_atom", "Expected a number, got hello", nil)},

		// Integer Conversion
		"int_float":      {"(int 3.14)", "3", nil},
		"int_round_down": {"(int 3.99)", "3", nil},
		"int_negative":   {"(int -2.7)", "-2", nil},
		"int_zero":       {"(int 0)", "0", nil},
		"int_integer":    {"(int 42)", "42", nil},
		"int_no_args":    {"(int)", "", NewRuntimeError("int_no_args", "'int' expects one argument", nil)},
		"int_atom":       {"(int 'hello)", "", NewRuntimeError("int_atom", "'int' expects a number as its argument", nil)},
		"int_two_args":   {"(int 1 2)", "", NewRuntimeError("int_two_args", "'int' expects one argument", nil)},

		// Comparison Functions
		"lt_true":        {"(< 1 2)", "#t", nil},
		"lt_false":       {"(< 2 1)", "()", nil},
		"lt_equal":       {"(< 5 5)", "()", nil},
		"lt_negative":    {"(< -1 0)", "#t", nil},
		"lt_float":       {"(< 1.5 2.5)", "#t", nil},
		"lt_no_args":     {"(<)", "", NewRuntimeError("lt_no_args", "'<' expects two arguments", nil)},
		"lt_one_arg":     {"(< 1)", "", NewRuntimeError("lt_one_arg", "'<' expects two arguments", nil)},
		"lt_three_args":  {"(< 1 2 3)", "", NewRuntimeError("lt_three_args", "'<' expects two arguments", nil)},
		"lt_first_atom":  {"(< 'hello 2)", "", NewRuntimeError("lt_first_atom", "'<' expects a number as its first argument", nil)},
		"lt_second_atom": {"(< 1 'hello)", "", NewRuntimeError("lt_second_atom", "'<' expects a number as its second argument", nil)},

		"eq_numbers_true":  {"(eq? 1 1)", "#t", nil},
		"eq_numbers_false": {"(eq? 1 2)", "()", nil},
		"eq_atoms_true":    {"(eq? 'hello 'hello)", "#t", nil},
		"eq_atoms_false":   {"(eq? 'hello 'world)", "()", nil},
		"eq_true_true":     {"(eq? #t #t)", "#t", nil},
		"eq_nil_nil":       {"(eq? () ())", "#t", nil},
		"eq_cons_false":    {"(eq? (cons 1 2) (cons 1 2))", "()", nil},
		"eq_no_args":       {"(eq?)", "", NewRuntimeError("eq_no_args", "'eq?' expects two arguments", nil)},
		"eq_one_arg":       {"(eq? 1)", "", NewRuntimeError("eq_one_arg", "'eq?' expects two arguments", nil)},

		// Type Predicates
		"pair_cons":     {"(pair? (cons 1 2))", "#t", nil},
		"pair_list":     {"(pair? (quote (1 2 3)))", "#t", nil},
		"pair_nil":      {"(pair? ())", "()", nil},
		"pair_number":   {"(pair? 42)", "()", nil},
		"pair_atom":     {"(pair? 'hello)", "()", nil},
		"pair_no_args":  {"(pair?)", "", NewRuntimeError("pair_no_args", "'pair?' expects one argument", nil)},
		"pair_two_args": {"(pair? (cons 1 2) 'extra)", "", NewRuntimeError("pair_two_args", "'pair?' expects one argument", nil)},

		// Logical Functions - Or
		"or_empty":        {"(or)", "()", nil},
		"or_true":         {"(or #t)", "#t", nil},
		"or_nil":          {"(or ())", "()", nil},
		"or_nil_true":     {"(or () #t)", "#t", nil},
		"or_true_nil":     {"(or #t ())", "#t", nil},
		"or_nil_nil_true": {"(or () () #t)", "#t", nil},
		"or_all_nil":      {"(or () () ())", "()", nil},
		"or_number_first": {"(or 42 #t)", "42", nil},
		"or_nil_number":   {"(or () 0 #t)", "0", nil},

		// Logical Functions - And
		"and_empty":         {"(and)", "#t", nil},
		"and_true":          {"(and #t)", "#t", nil},
		"and_nil":           {"(and ())", "()", nil},
		"and_true_true":     {"(and #t #t)", "#t", nil},
		"and_true_nil":      {"(and #t ())", "()", nil},
		"and_nil_true":      {"(and () #t)", "()", nil},
		"and_all_true":      {"(and #t #t #t)", "#t", nil},
		"and_number_true":   {"(and 42 #t)", "#t", nil},
		"and_number_number": {"(and 42 0)", "0", nil},

		// Logical Functions - Not
		"not_true":     {"(not #t)", "()", nil},
		"not_nil":      {"(not ())", "#t", nil},
		"not_number":   {"(not 42)", "()", nil},
		"not_zero":     {"(not 0)", "()", nil},
		"not_atom":     {"(not 'hello)", "()", nil},
		"not_no_args":  {"(not)", "", NewRuntimeError("not_no_args", "'not' expects one argument", nil)},
		"not_two_args": {"(not #t ())", "", NewRuntimeError("not_two_args", "'not' expects one argument", nil)},

		// Conditional Functions - If
		"if_true":       {"(if #t 1 2)", "1", nil},
		"if_false":      {"(if () 1 2)", "2", nil},
		"if_number":     {"(if 42 'yes 'no)", "yes", nil},
		"if_zero":       {"(if 0 'yes 'no)", "yes", nil},
		"if_comparison": {"(if (< 1 2) 'greater 'less)", "greater", nil},
		"if_no_args":    {"(if)", "", NewRuntimeError("if_no_args", "'if' expects arguments", nil)},
		"if_one_arg":    {"(if #t)", "", NewRuntimeError("if_one_arg", "'if' expects then branch", nil)},
		"if_two_args":   {"(if #t 1)", "", NewRuntimeError("if_two_args", "'if' expects else branch", nil)},
		"if_four_args":  {"(if #t 1 2 3)", "", NewRuntimeError("if_four_args", "'if' expects three arguments", nil)},

		// Conditional Functions - Cond
		"cond_first":      {"(cond (#t 42))", "42", nil},
		"cond_second":     {"(cond (() 1) (#t 2))", "2", nil},
		"cond_comparison": {"(cond ((< 1 2) 'first) (#t 'second))", "first", nil},
		"cond_later":      {"(cond ((> 1 2) 'first) ((< 3 4) 'second))", "second", nil},
		"cond_no_match":   {"(cond (() never))", "", NewRuntimeError("cond_no_match", "'cond' expects that at least one condition evaluates to #t", nil)},
		"cond_no_args":    {"(cond)", "", NewRuntimeError("cond_no_args", "'cond' expects that at least one condition evaluates to #t", nil)},
		"cond_atom":       {"(cond #t)", "", NewRuntimeError("cond_atom", "'cond' expects two-element cons cells as its arguments", nil)},
		"cond_empty":      {"(cond (()))", "", NewRuntimeError("cond_empty", "'cond' expects that at least one condition evaluates to #t", nil)},

		// Variable Binding - Let*
		"let_empty":        {"(let* () 42)", "42", nil},
		"let_single":       {"(let* ((x 1)) x)", "1", nil},
		"let_multiple":     {"(let* ((x 1) (y 2)) (+ x y))", "3", nil},
		"let_sequential":   {"(let* ((x 1) (y x)) y)", "1", nil},
		"let_shadow":       {"(let* ((x 1) (x 2)) x)", "2", nil},
		"let_nested":       {"(let* ((x 1)) (let* ((y 2)) (+ x y)))", "3", nil},
		"let_no_args":      {"(let*)", "", NewRuntimeError("let_no_args", "'let*' expects arguments", nil)},
		"let_no_body":      {"(let* ())", "", NewRuntimeError("let_no_body", "'let*' expects body", nil)},
		"let_three_args":   {"(let* () 1 2)", "", NewRuntimeError("let_three_args", "'let*' expects two arguments", nil)},
		"let_atom_binding": {"(let* (x) x)", "", NewRuntimeError("let_atom_binding", "'let*' expects bindings to be cons cells", nil)},
		"let_number_name":  {"(let* ((1 2)) 1)", "", NewRuntimeError("let_number_name", "'let*' expects binding names to be atoms", nil)},
		"let_no_expr":      {"(let* ((x)) x)", "", NewRuntimeError("let_no_expr", "'let*' expects bindings to have expressions", nil)},
		"let_too_many":     {"(let* ((x 1 2)) x)", "", NewRuntimeError("let_too_many", "'let*' expects bindings to consists of two values", nil)},

		// Functions and Closures - Lambda
		"lambda_no_args":   {"((lambda () 42))", "42", nil},
		"lambda_single":    {"((lambda (x) x) 5)", "5", nil},
		"lambda_multiple":  {"((lambda (x y) (+ x y)) 1 2)", "3", nil},
		"lambda_varargs":   {"((lambda x x) 1 2 3)", "(1 2 3)", nil},
		"lambda_rest":      {"((lambda (x . rest) rest) 1 2 3)", "(2 3)", nil},
		"lambda_no_params": {"(lambda)", "", NewRuntimeError("lambda_no_params", "'lambda' expects arguments", nil)},
		"lambda_no_body":   {"(lambda ())", "", NewRuntimeError("lambda_no_body", "'lambda' expects body", nil)},

		// Define and Function Calls
		"define_value":    {"(define x 42) x", "42", nil},
		"define_function": {"(define f (lambda (x) (+ x 1))) (f 5)", "6", nil},
		"define_no_args":  {"(define)", "", NewRuntimeError("define_no_args", "'define' expects arguments", nil)},
		"define_number":   {"(define 42 value)", "", NewRuntimeError("define_number", "'define' expects an atom as its first argument", nil)},
		"define_no_value": {"(define x)", "", NewRuntimeError("define_no_value", "'define' expects a value", nil)},

		// Lexical Scoping Tests
		"scope_simple":       {"(let* ((x 1)) ((lambda () x)))", "1", nil},
		"scope_param":        {"(let* ((x 1)) ((lambda (y) (+ x y)) 2))", "3", nil},
		"scope_nested":       {"(let* ((x 1)) (let* ((y 2)) ((lambda () (+ x y)))))", "3", nil},
		"scope_shadow_let":   {"(let* ((x 1)) (let* ((x 2)) ((lambda () x))))", "2", nil},
		"scope_return":       {"((let* ((x 1)) (lambda () x)))", "1", nil},
		"scope_param_shadow": {"((let* ((x 1)) (lambda (x) x)) 5)", "5", nil},

		// Complex Combinations
		"complex_math":   {"(+ (* 2 3) (- 10 5))", "11", nil},
		"complex_if":     {"(if (< (+ 1 2) (* 2 3)) 'yes 'no)", "yes", nil},
		"complex_list":   {"(car (cdr (quote (a b c))))", "b", nil},
		"complex_cons":   {"(cons (+ 1 2) (cons (* 2 3) ()))", "(3 6)", nil},
		"complex_lambda": {"(let* ((double (lambda (x) (* x 2)))) (double 21))", "42", nil},
		"complex_and":    {"(and (< 1 2) (> 3 2) (eq? 5 5))", "#t", nil},
		"complex_or":     {"(or (> 1 2) (< 3 2) (eq? 5 5))", "#t", nil},
		"complex_cond":   {"(cond ((> 1 2) 'impossible) ((pair? (cons 1 2)) 'possible))", "possible", nil},

		// Recursive Functions
		"factorial": {"(define fact (lambda (n) (if (< n 2) 1 (* n (fact (- n 1)))))) (fact 5)", "120", nil},
		"sum_list":  {"(define sum-list (lambda (lst) (if (pair? lst) (+ (car lst) (sum-list (cdr lst))) 0))) (sum-list (quote (1 2 3 4)))", "10", nil},

		// Error Handling and Edge Cases
		"undefined_function":  {"(unknown-function)", "", NewRuntimeError("undefined_function", "Undefined name: 'unknown-function'", nil)},
		"lambda_no_args_call": {"((lambda (x) x))", "", NewRuntimeError("lambda_no_args_call", "The function expects more arguments, 0 given", nil)},
		"lambda_too_many":     {"((lambda () 42) 1)", "", NewRuntimeError("lambda_too_many", "The function expects fewer arguments, 1 given", nil)},
		"call_number":         {"(1)", "", NewRuntimeError("call_number", "Attempting to evaluate 1, expected a builtin or a closure", nil)},
		"call_nil":            {"(() 1 2)", "", NewRuntimeError("call_nil", "Attempting to evaluate (), expected a builtin or a closure", nil)},

		// Eval Function
		"eval_basic":     {"(eval (quote (+ 1 2)))", "3", nil},
		"eval_undefined": {"(eval (quote x))", "", NewRuntimeError("eval_undefined", "Undefined name: 'x'", nil)},
		"eval_no_args":   {"(eval)", "", NewRuntimeError("eval_no_args", "'eval' expects one argument", nil)},
		"eval_two_args":  {"(eval 1 2)", "", NewRuntimeError("eval_two_args", "'eval' expects one argument", nil)},

		// Display Function
		"display_number":  {"(display 42)", "()", nil},
		"display_atom":    {"(define hello 'world) (display hello)", "()", nil},
		"display_no_args": {"(display)", "", NewRuntimeError("display_no_args", "'display' expects arguments", nil)},

		// Basic arithmetic with dotted calls
		"dot_add_list":   {"(define nums (quote (1 2 3 4))) (+ . nums)", "10", nil},
		"dot_add_empty":  {"(define nums (quote ())) (+ . nums)", "0", nil},
		"dot_add_single": {"(define nums (quote (5))) (+ . nums)", "5", nil},

		"dot_sub_list":   {"(define nums (quote (10 3 2))) (- . nums)", "5", nil},
		"dot_sub_single": {"(define nums (quote (5))) (- . nums)", "-5", nil},
		"dot_sub_empty":  {"(define nums (quote ())) (- . nums)", "", NewRuntimeError("dot_sub_empty", "'-' requires at least one argument", nil)},

		"dot_mul_list":   {"(define nums (quote (2 3 4))) (* . nums)", "24", nil},
		"dot_mul_empty":  {"(define nums (quote ())) (* . nums)", "1", nil},
		"dot_mul_single": {"(define nums (quote (7))) (* . nums)", "7", nil},

		"dot_div_list":   {"(define nums (quote (12 3 2))) (/ . nums)", "2", nil},
		"dot_div_single": {"(define nums (quote (4))) (/ . nums)", "0.25", nil},
		"dot_div_empty":  {"(define nums (quote ())) (/ . nums)", "", NewRuntimeError("dot_div_empty", "'/' requires at least one argument", nil)},
		"dot_div_zero":   {"(define nums (quote (10 2 0))) (/ . nums)", "", NewRuntimeError("dot_div_zero", "Division by zero", nil)},

		// Comparison functions with dotted calls
		"dot_lt_true":       {"(define nums (quote (1 2))) (< . nums)", "#t", nil},
		"dot_lt_false":      {"(define nums (quote (2 1))) (< . nums)", "()", nil},
		"dot_lt_wrong_args": {"(define nums (quote (1))) (< . nums)", "", NewRuntimeError("dot_lt_wrong_args", "'<' expects two arguments", nil)},

		"dot_eq_true":       {"(define nums (quote (5 5))) (eq? . nums)", "#t", nil},
		"dot_eq_false":      {"(define nums (quote (5 6))) (eq? . nums)", "()", nil},
		"dot_eq_wrong_args": {"(define nums (quote (5))) (eq? . nums)", "", NewRuntimeError("dot_eq_wrong_args", "'eq?' expects two arguments", nil)},

		// List manipulation with dotted calls
		"dot_cons":            {"(define pair (quote (1 2))) (cons . pair)", "(1 . 2)", nil},
		"dot_cons_wrong_args": {"(define nums (quote (1))) (cons . nums)", "", NewRuntimeError("dot_cons_wrong_args", "'cons' expects two arguments", nil)},

		// Logical functions with dotted calls
		"dot_and_all_true":   {"(define vals (quote (#t #t #t))) (and . vals)", "#t", NewRuntimeError("dot_and_all_true", "'and' expects arguments", nil)},
		"dot_and_with_false": {"(define vals (quote (#t () #t))) (and . vals)", "()", NewRuntimeError("dot_and_with_false", "'and' expects arguments", nil)},
		"dot_and_empty":      {"(define vals (quote ())) (and . vals)", "#t", NewRuntimeError("dot_and_empty", "'and' expects arguments", nil)},

		"dot_or_all_false": {"(define vals (quote (() () ()))) (or . vals)", "()", NewRuntimeError("dot_or_all_false", "'or' expects arguments", nil)},
		"dot_or_with_true": {"(define vals (quote (() () #t))) (or . vals)", "#t", NewRuntimeError("dot_or_with_true", "'or' expects arguments", nil)},
		"dot_or_empty":     {"(define vals (quote ())) (or . vals)", "()", NewRuntimeError("dot_or_empty", "'or' expects arguments", nil)},

		// Lambda functions with dotted calls
		"dot_lambda_call":    {"(define f (lambda (x y) (+ x y))) (define args (quote (3 4))) (f . args)", "7", nil},
		"dot_lambda_varargs": {"(define f (lambda args (+ . args))) (define nums (quote (1 2 3))) (f . nums)", "6", nil},
		"dot_lambda_mixed":   {"(define f (lambda (x . rest) (cons x rest))) (define args (quote (1 2 3))) (f . args)", "(1 2 3)", nil},

		// More complex examples
		"dot_nested":      {"(define inner (quote (2 3))) (define outer (cons + inner)) (eval outer)", "5", nil},
		"dot_constructed": {"(define op +) (define nums (quote (1 2 3 4))) (op . nums)", "10", nil},

		// Error cases with dotted calls
		"dot_builtin_wrong_type": {"(define not-list 42) (+ . not-list)", "", NewRuntimeError("dot_builtin_wrong_type", "Expected arguments", nil)},
		"dot_undefined_func":     {"(define args (quote (1 2))) (undefined-func . args)", "", NewRuntimeError("dot_undefined_func", "Undefined name: 'undefined-func'", nil)},
		"dot_non_callable":       {"(define args (quote (1 2))) (42 . args)", "", NewRuntimeError("dot_non_callable", "Attempting to evaluate 42, expected a builtin or a closure", nil)},

		// Mixed regular and dotted arguments (should not work)
		"dot_mixed_args": {"(define nums (quote (2 3))) (+ 1 . nums)", "6", nil},
	} {
		t.Run(name, func(t *testing.T) {
			builtins := LoadBuiltins()
			sourceParser := NewParser(name, tc.source)
			evaluator := NewEvaluator(name, builtins)

			var evalResult Expr
			var evalError error
			for {
				sourceExpr, err := sourceParser.NextExpr()
				if errors.Is(err, io.EOF) {
					break
				}
				if err != nil {
					t.Fatalf("Source parsing failed: %v", err)
				}
				evalResult, evalError = evaluator.Eval(sourceExpr)
			}

			if tc.err == nil {
				expectedResultParser := NewParser(name, tc.result)
				expectedResultExpr, err := expectedResultParser.NextExpr()
				if err != nil {
					t.Fatalf("Result parsing failed: %v", err)
				}
				last, err := expectedResultParser.NextExpr()
				if !errors.Is(err, io.EOF) {
					t.Fatalf("Expected EOF after parsing the result expression, found %v", last)
				}

				if !expectedResultExpr.Equal(evalResult) {
					t.Errorf("Expected result %v, got %v", expectedResultExpr, evalResult)
				}
			}

			if !runtimeErrorsEqual(tc.err, evalError) {
				t.Errorf("Expected error %v, got %v", tc.err, evalError)
			}
		})
	}
}
