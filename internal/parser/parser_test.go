package parser

import (
	"errors"
	"testing"

	"tomaskala.com/glisp/internal/tokenizer"
)

type parserTest struct {
	source string
	expr   Expr
	err    error
}

func newTestParser(name, source string) *Parser {
	tokenizer := tokenizer.NewTokenizer(source)
	parser := &Parser{name: name, tokenizer: tokenizer}
	parser.next() // Initialize the first token.
	return parser
}

func exprsEqual(expected, actual Expr) bool {
	return expected.Equal(actual)
}

func stringSlicesEqual(expected, actual []string) bool {
	if len(expected) != len(actual) {
		return false
	}
	for i := range expected {
		if expected[i] != actual[i] {
			return false
		}
	}
	return true
}

var atomTests = map[string]parserTest{
	"simple atom":    {source: "map", expr: &Atom{"map"}},
	"atom with dash": {source: "list-ref", expr: &Atom{"list-ref"}},
	"atom plus":      {source: "+", expr: &Atom{"+"}},
	"atom minus":     {source: "-", expr: &Atom{"-"}},
	"atom multiply":  {source: "*", expr: &Atom{"*"}},
	"atom divide":    {source: "/", expr: &Atom{"/"}},
	"atom question":  {source: "null?", expr: &Atom{"null?"}},
	"atom with nums": {source: "val2num", expr: &Atom{"val2num"}},
	"atom special":   {source: "odd@val-lisp", expr: &Atom{"odd@val-lisp"}},
	"atom long":      {source: "very-long-atom@with%some+signs", expr: &Atom{"very-long-atom@with%some+signs"}},
	"atom operator":  {source: "<*>", expr: &Atom{"<*>"}},
	"atom boolean":   {source: "#t", expr: &Atom{"#t"}},
	// Edge cases for atoms
	"atom underscore":    {source: "_", expr: &Atom{"_"}},
	"atom with numbers":  {source: "var123", expr: &Atom{"var123"}},
	"atom with unicode":  {source: "λ", expr: &Atom{"λ"}},
	"atom mixed unicode": {source: "test-λ-func", expr: &Atom{"test-λ-func"}},
	"atom exclamation":   {source: "not!", expr: &Atom{"not!"}},
	"atom colon":         {source: "keyword:", expr: &Atom{"keyword:"}},
	"atom pipe":          {source: "|special|", expr: &Atom{"|special|"}},
	"atom backslash":     {source: "\\", expr: &Atom{"\\"}},
	"atom caret":         {source: "^", expr: &Atom{"^"}},
	"atom tilde":         {source: "~", expr: &Atom{"~"}},
	"atom ampersand":     {source: "&", expr: &Atom{"&"}},
}

var numberTests = map[string]parserTest{
	"integer":         {source: "42", expr: &Number{42}},
	"negative int":    {source: "-17", expr: &Number{-17}},
	"positive int":    {source: "+123", expr: &Number{123}},
	"zero":            {source: "0", expr: &Number{0}},
	"float":           {source: "3.14159", expr: &Number{3.14159}},
	"negative float":  {source: "-2.718", expr: &Number{-2.718}},
	"positive float":  {source: "+1.414", expr: &Number{1.414}},
	"hex":             {source: "0xFF", expr: &Number{255}},
	"hex negative":    {source: "-0xABC", expr: &Number{-2748}},
	"octal":           {source: "0o755", expr: &Number{493}},
	"octal negative":  {source: "-0o644", expr: &Number{-420}},
	"binary":          {source: "0b1101", expr: &Number{13}},
	"binary negative": {source: "-0b1010", expr: &Number{-10}},
	"scientific":      {source: "1.5e3", expr: &Number{1500}},
	"scientific neg":  {source: "-2.5E-2", expr: &Number{-0.025}},
	"hex float":       {source: "0x1.8p3", expr: &Number{12}},
	// Edge cases for numbers
	"hex lowercase":            {source: "0xabc", expr: &Number{2748}},
	"hex uppercase":            {source: "0XDEF", expr: &Number{3567}},
	"octal lowercase":          {source: "0o123", expr: &Number{83}},
	"octal uppercase":          {source: "0O456", expr: &Number{302}},
	"binary lowercase":         {source: "0b101", expr: &Number{5}},
	"binary uppercase":         {source: "0B110", expr: &Number{6}},
	"zero float":               {source: "0.0", expr: &Number{0.0}},
	"negative zero":            {source: "-0", expr: &Number{0}},
	"positive zero":            {source: "+0", expr: &Number{0}},
	"scientific uppercase":     {source: "1E10", expr: &Number{1e10}},
	"hex scientific":           {source: "0x1P4", expr: &Number{16}},
	"hex float negative":       {source: "-0x1.5p-1", expr: &Number{-0.65625}},
	"large integer":            {source: "9223372036854775807", expr: &Number{9.223372036854776e+18}},
	"very small float":         {source: "1e-100", expr: &Number{1e-100}},
	"numbers with underscores": {source: "1_000_000", expr: &Number{1000000}},
	"hex with underscores":     {source: "0xFF_FF", expr: &Number{65535}},
	"float with underscores":   {source: "3.141_592", expr: &Number{3.141592}},
}

var nilTests = map[string]parserTest{
	"empty list": {source: "()", expr: &Nil{}},
}

var quoteTests = map[string]parserTest{
	"quote atom":      {source: "'x", expr: &Quote{&Atom{"x"}}},
	"quote number":    {source: "'42", expr: &Quote{&Number{42}}},
	"quote nil":       {source: "'()", expr: &Quote{&Nil{}}},
	"quote list":      {source: "'(a b c)", expr: &Quote{&NamedCall{"a", []Expr{&Atom{"b"}, &Atom{"c"}}}}},
	"long quote atom": {source: "(quote x)", expr: &Quote{&Atom{"x"}}},
	"long quote list": {source: "(quote (a b))", expr: &Quote{&NamedCall{"a", []Expr{&Atom{"b"}}}}},
	// Edge cases for quotes
	"nested quotes":      {source: "''x", expr: &Quote{&Quote{&Atom{"x"}}}},
	"quote complex expr": {source: "'(+ 1 (* 2 3))", expr: &Quote{&NamedCall{"+", []Expr{&Number{1}, &NamedCall{"*", []Expr{&Number{2}, &Number{3}}}}}}},
	"quote lambda":       {source: "'(lambda (x) x)", expr: &Quote{&Function{"", []string{"x"}, "", &Atom{"x"}}}},
	"long quote nested":  {source: "(quote (quote x))", expr: &Quote{&Quote{&Atom{"x"}}}},
}

var callTests = map[string]parserTest{
	"lambda call":         {source: "((lambda (x) x) 42)", expr: &Call{&Function{"", []string{"x"}, "", &Atom{"x"}}, []Expr{&Number{42}}}},
	"nested lambda call":  {source: "((lambda (f x) (f x)) + 5)", expr: &Call{&Function{"", []string{"f", "x"}, "", &NamedCall{"f", []Expr{&Atom{"x"}}}}, []Expr{&Atom{"+"}, &Number{5}}}},
	"quoted lambda call":  {source: "('(lambda (x) x) 'test)", expr: &Call{&Quote{&Function{"", []string{"x"}, "", &Atom{"x"}}}, []Expr{&Quote{&Atom{"test"}}}}},
	"let expression call": {source: "((let ((x 5)) (lambda (y) (+ x y))) 3)", expr: &Call{&Let{[]Binding{{"x", &Number{5}}}, &Function{"", []string{"y"}, "", &NamedCall{"+", []Expr{&Atom{"x"}, &Atom{"y"}}}}}, []Expr{&Number{3}}}},
	"call with no args":   {source: "((lambda () 42))", expr: &Call{&Function{"", []string{}, "", &Number{42}}, []Expr{}}},
	"call with many args": {source: "((lambda (a b c d) (+ a b c d)) 1 2 3 4)", expr: &Call{&Function{"", []string{"a", "b", "c", "d"}, "", &NamedCall{"+", []Expr{&Atom{"a"}, &Atom{"b"}, &Atom{"c"}, &Atom{"d"}}}}, []Expr{&Number{1}, &Number{2}, &Number{3}, &Number{4}}}},
	"nested call":         {source: "(((lambda (x) (lambda (y) (+ x y))) 5) 3)", expr: &Call{&Call{&Function{"", []string{"x"}, "", &Function{"", []string{"y"}, "", &NamedCall{"+", []Expr{&Atom{"x"}, &Atom{"y"}}}}}, []Expr{&Number{5}}}, []Expr{&Number{3}}}},
}

var namedCallTests = map[string]parserTest{
	"simple call":     {source: "(+ 1 2)", expr: &NamedCall{"+", []Expr{&Number{1}, &Number{2}}}},
	"nested call":     {source: "(+ (* 2 3) 4)", expr: &NamedCall{"+", []Expr{&NamedCall{"*", []Expr{&Number{2}, &Number{3}}}, &Number{4}}}},
	"no args":         {source: "(list)", expr: &NamedCall{"list", []Expr{}}},
	"one arg":         {source: "(car lst)", expr: &NamedCall{"car", []Expr{&Atom{"lst"}}}},
	"many args":       {source: "(+ 1 2 3 4 5)", expr: &NamedCall{"+", []Expr{&Number{1}, &Number{2}, &Number{3}, &Number{4}, &Number{5}}}},
	"call with quote": {source: "(cons 'a '())", expr: &NamedCall{"cons", []Expr{&Quote{&Atom{"a"}}, &Quote{&Nil{}}}}},
	// Edge cases for named calls
	"deeply nested":      {source: "(f (g (h (i j))))", expr: &NamedCall{"f", []Expr{&NamedCall{"g", []Expr{&NamedCall{"h", []Expr{&NamedCall{"i", []Expr{&Atom{"j"}}}}}}}}}},
	"mixed expressions":  {source: "(f 'a 42 (g x) '())", expr: &NamedCall{"f", []Expr{&Quote{&Atom{"a"}}, &Number{42}, &NamedCall{"g", []Expr{&Atom{"x"}}}, &Quote{&Nil{}}}}},
	"special characters": {source: "(+ -1 +2)", expr: &NamedCall{"+", []Expr{&Number{-1}, &Number{2}}}},
}

var defineTests = map[string]parserTest{
	"define var": {source: "(define x 42)", expr: &Define{"x", &Number{42}}},
	"define func": {source: "(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))",
		expr: &Define{"factorial", &Function{"factorial", []string{"n"}, "",
			&NamedCall{"if", []Expr{
				&NamedCall{"=", []Expr{&Atom{"n"}, &Number{0}}},
				&Number{1},
				&NamedCall{"*", []Expr{
					&Atom{"n"},
					&NamedCall{"factorial", []Expr{&NamedCall{"-", []Expr{&Atom{"n"}, &Number{1}}}}},
				}},
			}}}}},
	"define atom": {source: "(define nil '())", expr: &Define{"nil", &Quote{&Nil{}}}},
	// Edge cases for define
	"define lambda no name": {source: "(define f (lambda (x) x))", expr: &Define{"f", &Function{"f", []string{"x"}, "", &Atom{"x"}}}},
	"define complex expr":   {source: "(define result (+ (* 2 3) 4))", expr: &Define{"result", &NamedCall{"+", []Expr{&NamedCall{"*", []Expr{&Number{2}, &Number{3}}}, &Number{4}}}}},
}

var lambdaTests = map[string]parserTest{
	"simple lambda":     {source: "(lambda (x) x)", expr: &Function{"", []string{"x"}, "", &Atom{"x"}}},
	"lambda no args":    {source: "(lambda () 42)", expr: &Function{"", []string{}, "", &Number{42}}},
	"lambda multi args": {source: "(lambda (x y z) (+ x y z))", expr: &Function{"", []string{"x", "y", "z"}, "", &NamedCall{"+", []Expr{&Atom{"x"}, &Atom{"y"}, &Atom{"z"}}}}},
	"lambda rest param": {source: "(lambda (x . xs) (cons x xs))", expr: &Function{"", []string{"x"}, "xs", &NamedCall{"cons", []Expr{&Atom{"x"}, &Atom{"xs"}}}}},
	"lambda only rest":  {source: "(lambda xs xs)", expr: &Function{"", []string{}, "xs", &Atom{"xs"}}},
	// Edge cases for lambda
	"lambda complex body":     {source: "(lambda (x) (if (> x 0) x (- x)))", expr: &Function{"", []string{"x"}, "", &NamedCall{"if", []Expr{&NamedCall{">", []Expr{&Atom{"x"}, &Number{0}}}, &Atom{"x"}, &NamedCall{"-", []Expr{&Atom{"x"}}}}}}},
	"lambda nested":           {source: "(lambda (f) (lambda (x) (f x)))", expr: &Function{"", []string{"f"}, "", &Function{"", []string{"x"}, "", &NamedCall{"f", []Expr{&Atom{"x"}}}}}},
	"lambda with many params": {source: "(lambda (a b c d e f g) (+ a b c d e f g))", expr: &Function{"", []string{"a", "b", "c", "d", "e", "f", "g"}, "", &NamedCall{"+", []Expr{&Atom{"a"}, &Atom{"b"}, &Atom{"c"}, &Atom{"d"}, &Atom{"e"}, &Atom{"f"}, &Atom{"g"}}}}},
	"lambda multiple rest":    {source: "(lambda (x y . rest) rest)", expr: &Function{"", []string{"x", "y"}, "rest", &Atom{"rest"}}},
}

var letTests = map[string]parserTest{
	"simple let": {source: "(let ((x 1)) x)", expr: &Let{[]Binding{{"x", &Number{1}}}, &Atom{"x"}}},
	"let multiple bindings": {source: "(let ((x 1) (y 2)) (+ x y))",
		expr: &Let{[]Binding{{"x", &Number{1}}, {"y", &Number{2}}}, &NamedCall{"+", []Expr{&Atom{"x"}, &Atom{"y"}}}}},
	"let no bindings": {source: "(let () 42)", expr: &Let{[]Binding{}, &Number{42}}},
	"let nested": {source: "(let ((x 1)) (let ((y 2)) (+ x y)))",
		expr: &Let{[]Binding{{"x", &Number{1}}}, &Let{[]Binding{{"y", &Number{2}}}, &NamedCall{"+", []Expr{&Atom{"x"}, &Atom{"y"}}}}}},
	// Edge cases for let
	"let complex bindings": {source: "(let ((f (lambda (x) (* x x))) (y 5)) (f y))",
		expr: &Let{[]Binding{{"f", &Function{"", []string{"x"}, "", &NamedCall{"*", []Expr{&Atom{"x"}, &Atom{"x"}}}}}, {"y", &Number{5}}}, &NamedCall{"f", []Expr{&Atom{"y"}}}}},
	"let recursive binding": {source: "(let ((x (+ y 1)) (y 5)) x)",
		expr: &Let{[]Binding{{"x", &NamedCall{"+", []Expr{&Atom{"y"}, &Number{1}}}}, {"y", &Number{5}}}, &Atom{"x"}}},
	"let shadowing": {source: "(let ((x 1)) (let ((x 2)) x))",
		expr: &Let{[]Binding{{"x", &Number{1}}}, &Let{[]Binding{{"x", &Number{2}}}, &Atom{"x"}}}},
}

var complexTests = map[string]parserTest{
	"map function": {
		source: "(define map (lambda (f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs))))))",
		expr: &Define{"map", &Function{"map", []string{"f", "xs"}, "",
			&NamedCall{"if", []Expr{
				&NamedCall{"null?", []Expr{&Atom{"xs"}}},
				&Quote{&Nil{}},
				&NamedCall{"cons", []Expr{
					&NamedCall{"f", []Expr{&NamedCall{"car", []Expr{&Atom{"xs"}}}}},
					&NamedCall{"map", []Expr{&Atom{"f"}, &NamedCall{"cdr", []Expr{&Atom{"xs"}}}}},
				}},
			}}}},
	},
	"nested structures": {
		source: "(let ((f (lambda (x) (* x x)))) (f (+ 2 3)))",
		expr: &Let{[]Binding{{"f", &Function{"", []string{"x"}, "", &NamedCall{"*", []Expr{&Atom{"x"}, &Atom{"x"}}}}}},
			&NamedCall{"f", []Expr{&NamedCall{"+", []Expr{&Number{2}, &Number{3}}}}}},
	},
	// More complex edge cases
	"y combinator": {
		source: "(lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))",
		expr:   &Function{"", []string{"f"}, "", &Call{&Function{"", []string{"x"}, "", &NamedCall{"f", []Expr{&NamedCall{"x", []Expr{&Atom{"x"}}}}}}, []Expr{&Function{"", []string{"x"}, "", &NamedCall{"f", []Expr{&NamedCall{"x", []Expr{&Atom{"x"}}}}}}}}},
	},
	"deeply nested let": {
		source: "(let ((a 1)) (let ((b 2)) (let ((c 3)) (+ a b c))))",
		expr:   &Let{[]Binding{{"a", &Number{1}}}, &Let{[]Binding{{"b", &Number{2}}}, &Let{[]Binding{{"c", &Number{3}}}, &NamedCall{"+", []Expr{&Atom{"a"}, &Atom{"b"}, &Atom{"c"}}}}}},
	},
}

var errorTests = map[string]struct {
	source      string
	expectedMsg string
	expectedPos tokenizer.Position
}{
	"unmatched paren": {
		source:      "(+ 1 2",
		expectedMsg: "Unexpected token: expected expression, got TokenEOF",
		expectedPos: tokenizer.Position{Line: 1, Column: 7},
	},
	"incomplete define": {
		source:      "(define x)",
		expectedMsg: "Unexpected token: expected expression, got TokenRightParen",
		expectedPos: tokenizer.Position{Line: 1, Column: 10},
	},
	"incomplete lambda": {
		source:      "(lambda (x))",
		expectedMsg: "Unexpected token: expected expression, got TokenRightParen",
		expectedPos: tokenizer.Position{Line: 1, Column: 12},
	},
	"incomplete let": {
		source:      "(let ((x 1)))",
		expectedMsg: "Unexpected token: expected expression, got TokenRightParen",
		expectedPos: tokenizer.Position{Line: 1, Column: 13},
	},
	"malformed let": {
		source:      "(let (x 1) x)",
		expectedMsg: "Unexpected token: expected TokenLeftParen, got TokenAtom",
		expectedPos: tokenizer.Position{Line: 1, Column: 7},
	},
	"malformed binding": {
		source:      "(let ((x)) x)",
		expectedMsg: "Unexpected token: expected expression, got TokenRightParen",
		expectedPos: tokenizer.Position{Line: 1, Column: 9},
	},
	"incomplete quote": {
		source:      "'",
		expectedMsg: "Unexpected token: expected expression, got TokenEOF",
		expectedPos: tokenizer.Position{Line: 1, Column: 2},
	},
	"bad lambda params": {
		source:      "(lambda 123 x)",
		expectedMsg: "Unexpected lambda parameter: TokenNumber",
		expectedPos: tokenizer.Position{Line: 1, Column: 9},
	},
	"unterminated quote": {
		source:      "(quote",
		expectedMsg: "Unexpected token: expected expression, got TokenEOF",
		expectedPos: tokenizer.Position{Line: 1, Column: 7},
	},
	"empty define": {
		source:      "(define)",
		expectedMsg: "Unexpected token: expected TokenAtom, got TokenRightParen",
		expectedPos: tokenizer.Position{Line: 1, Column: 8},
	},
	"define with number": {
		source:      "(define 123 x)",
		expectedMsg: "Unexpected token: expected TokenAtom, got TokenNumber",
		expectedPos: tokenizer.Position{Line: 1, Column: 9},
	},
	"let missing bindings": {
		source:      "(let)",
		expectedMsg: "Unexpected token: expected TokenLeftParen, got TokenRightParen",
		expectedPos: tokenizer.Position{Line: 1, Column: 5},
	},
	"let unclosed bindings": {
		source:      "(let ((x 1)",
		expectedMsg: "Unexpected token: expected TokenLeftParen, got TokenEOF",
		expectedPos: tokenizer.Position{Line: 1, Column: 12},
	},
	"lambda missing params": {
		source:      "(lambda)",
		expectedMsg: "Unexpected lambda parameter: TokenRightParen",
		expectedPos: tokenizer.Position{Line: 1, Column: 8},
	},
	"dot without rest param": {
		source:      "(lambda (x .) body)",
		expectedMsg: "Unexpected token: expected TokenAtom, got TokenRightParen",
		expectedPos: tokenizer.Position{Line: 1, Column: 13},
	},
	"multiple dots": {
		source:      "(lambda (x . y . z) body)",
		expectedMsg: "Unexpected token: expected TokenRightParen, got TokenDot",
		expectedPos: tokenizer.Position{Line: 1, Column: 16},
	},
	"invalid token": {
		source:      "(",
		expectedMsg: "Unexpected token: expected expression, got TokenEOF",
		expectedPos: tokenizer.Position{Line: 1, Column: 2},
	},
	"nested unmatched": {
		source:      "(+ (- 3",
		expectedMsg: "Unexpected token: expected expression, got TokenEOF",
		expectedPos: tokenizer.Position{Line: 1, Column: 8},
	},
	"quote missing expr": {
		source:      "(quote)",
		expectedMsg: "Unexpected token: expected expression, got TokenRightParen",
		expectedPos: tokenizer.Position{Line: 1, Column: 7},
	},
	"number overflow": {
		source:      "999999999999999999999999999999999999999999999999999999999999999999",
		expectedMsg: "Integer overflow",
		expectedPos: tokenizer.Position{Line: 1, Column: 1},
	},
	"invalid number": {
		source:      "0xGHI",
		expectedMsg: "Atom cannot start with a number",
		expectedPos: tokenizer.Position{Line: 1, Column: 1},
	},
}

var programTests = map[string]struct {
	source string
	exprs  []Expr
}{
	"single expr":    {source: "42", exprs: []Expr{&Number{42}}},
	"multiple exprs": {source: "1 2 'hello", exprs: []Expr{&Number{1}, &Number{2}, &Quote{&Atom{"hello"}}}},
	"mixed exprs": {source: "(define x 1) (+ x 2)", exprs: []Expr{
		&Define{"x", &Number{1}},
		&NamedCall{"+", []Expr{&Atom{"x"}, &Number{2}}},
	}},
	"empty program":       {source: "", exprs: []Expr{}},
	"whitespace only":     {source: "   \n\t  ", exprs: []Expr{}},
	"comments only":       {source: "; This is a comment\n; Another comment", exprs: []Expr{}},
	"mixed with comments": {source: "; Start\n42 ; middle\n'hello ; end", exprs: []Expr{&Number{42}, &Quote{&Atom{"hello"}}}},
}

func TestParseAtoms(t *testing.T) {
	for name, tc := range atomTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.expr, expr) {
				t.Errorf("expected %#v, got %#v", tc.expr, expr)
			}
		})
	}
}

func TestParseNumbers(t *testing.T) {
	for name, tc := range numberTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.expr, expr) {
				t.Errorf("expected %#v, got %#v", tc.expr, expr)
			}
		})
	}
}

func TestParseNil(t *testing.T) {
	for name, tc := range nilTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.expr, expr) {
				t.Errorf("expected %#v, got %#v", tc.expr, expr)
			}
		})
	}
}

func TestParseQuotes(t *testing.T) {
	for name, tc := range quoteTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.expr, expr) {
				t.Errorf("expected %#v, got %#v", tc.expr, expr)
			}
		})
	}
}

func TestParseCalls(t *testing.T) {
	for name, tc := range callTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.expr, expr) {
				t.Errorf("expected %#v, got %#v", tc.expr, expr)
			}
			if _, ok := expr.(*Call); !ok {
				t.Errorf("expected Call, got %T", expr)
			}
		})
	}
}

func TestParseNamedCalls(t *testing.T) {
	for name, tc := range namedCallTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.expr, expr) {
				t.Errorf("expected %#v, got %#v", tc.expr, expr)
			}
			if _, ok := expr.(*NamedCall); !ok {
				t.Errorf("expected NamedCall, got %T", expr)
			}
		})
	}
}

func TestParseDefines(t *testing.T) {
	for name, tc := range defineTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.expr, expr) {
				t.Errorf("expected %#v, got %#v", tc.expr, expr)
			}
		})
	}
}

func TestParseLambdas(t *testing.T) {
	for name, tc := range lambdaTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.expr, expr) {
				t.Errorf("expected %#v, got %#v", tc.expr, expr)
			}
		})
	}
}

func TestParseLets(t *testing.T) {
	for name, tc := range letTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.expr, expr) {
				t.Errorf("expected %#v, got %#v", tc.expr, expr)
			}
		})
	}
}

func TestParseComplex(t *testing.T) {
	for name, tc := range complexTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.expr, expr) {
				t.Errorf("expected %#v, got %#v", tc.expr, expr)
			}
		})
	}
}

func TestParseErrors(t *testing.T) {
	for name, tc := range errorTests {
		t.Run(name, func(t *testing.T) {
			_, err := Parse(name, tc.source)

			if err == nil {
				t.Errorf("expected error, got nil")
				return
			}

			var parseErr *ParseError
			if !errors.As(err, &parseErr) {
				t.Errorf("expected ParseError, got %T: %v", err, err)
				return
			}

			if tc.expectedMsg != "" && parseErr.Message != tc.expectedMsg {
				t.Errorf("expected error message %q, got %q", tc.expectedMsg, parseErr.Message)
			}

			if tc.expectedPos.Line != 0 || tc.expectedPos.Column != 0 {
				if parseErr.Position.Line != tc.expectedPos.Line || parseErr.Position.Column != tc.expectedPos.Column {
					t.Errorf("expected error at %d:%d, got %d:%d",
						tc.expectedPos.Line, tc.expectedPos.Column,
						parseErr.Position.Line, parseErr.Position.Column)
				}
			}
		})
	}
}

func TestParsePrograms(t *testing.T) {
	for name, tc := range programTests {
		t.Run(name, func(t *testing.T) {
			prog, err := Parse(name, tc.source)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
				return
			}
			if len(prog.Exprs) != len(tc.exprs) {
				t.Errorf("expected %d expressions, got %d", len(tc.exprs), len(prog.Exprs))
				return
			}
			for i, expected := range tc.exprs {
				if !exprsEqual(expected, prog.Exprs[i]) {
					t.Errorf("expr %d: expected %#v, got %#v", i, expected, prog.Exprs[i])
				}
			}
		})
	}
}

func TestParseList(t *testing.T) {
	parser := newTestParser("test", "a b c)")
	args := parser.parseList()
	expected := []Expr{&Atom{"a"}, &Atom{"b"}, &Atom{"c"}}
	if len(args) != len(expected) {
		t.Errorf("expected %d args, got %d", len(expected), len(args))
	}
	for i, arg := range args {
		if !exprsEqual(expected[i], arg) {
			t.Errorf("arg %d: expected %#v, got %#v", i, expected[i], arg)
		}
	}
}

func TestParseStringList(t *testing.T) {
	parser := newTestParser("test", "x y z)")
	args, dotArg := parser.parseStringList()
	expected := []string{"x", "y", "z"}
	if !stringSlicesEqual(args, expected) {
		t.Errorf("expected %v, got %v", expected, args)
	}
	if dotArg != "" {
		t.Errorf("expected empty dot arg, got %s", dotArg)
	}
}

func TestParseStringListWithDot(t *testing.T) {
	parser := newTestParser("test", "x y . rest)")
	args, dotArg := parser.parseStringList()
	expected := []string{"x", "y"}
	if !stringSlicesEqual(args, expected) {
		t.Errorf("expected %v, got %v", expected, args)
	}
	if dotArg != "rest" {
		t.Errorf("expected dot arg 'rest', got %s", dotArg)
	}
}

func TestParseEdgeCases(t *testing.T) {
	tests := map[string]struct {
		source   string
		expected Expr
	}{
		"signed atom": {
			source:   "+symbol",
			expected: &Atom{"+symbol"},
		},
		"negative atom": {
			source:   "-symbol",
			expected: &Atom{"-symbol"},
		},
		"complex nested quote": {
			source:   "'''x",
			expected: &Quote{&Quote{&Quote{&Atom{"x"}}}},
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.expected, expr) {
				t.Errorf("expected %#v, got %#v", tc.expected, expr)
			}
		})
	}
}

func TestCallVsNamedCallDistinction(t *testing.T) {
	// Test that atom function calls create NamedCall
	parser1 := newTestParser("test1", "(f x)")
	expr1 := parser1.parseExpr()
	if _, ok := expr1.(*NamedCall); !ok {
		t.Errorf("expected NamedCall for atom function, got %T", expr1)
	}

	// Test that lambda function calls create Call
	parser2 := newTestParser("test2", "((lambda (x) x) y)")
	expr2 := parser2.parseExpr()
	if _, ok := expr2.(*Call); !ok {
		t.Errorf("expected Call for lambda function, got %T", expr2)
	}

	// Test that they have different string representations but we check structurally
	namedCall := &NamedCall{"f", []Expr{&Atom{"x"}}}
	call := &Call{&Atom{"f"}, []Expr{&Atom{"x"}}}

	// These should have the same string representation
	if namedCall.String() != call.String() {
		t.Errorf("string representations differ: %s vs %s", namedCall.String(), call.String())
	}

	// But they should be structurally different
	if namedCall.Equal(call) {
		t.Errorf("Call and NamedCall should be structurally different")
	}
}

func TestWhitespaceAndComments(t *testing.T) {
	tests := map[string]struct {
		source   string
		expected []Expr
	}{
		"leading whitespace": {
			source:   "   42",
			expected: []Expr{&Number{42}},
		},
		"trailing whitespace": {
			source:   "42   ",
			expected: []Expr{&Number{42}},
		},
		"mixed whitespace": {
			source:   "\t\n  42  \n\t",
			expected: []Expr{&Number{42}},
		},
		"comment at end": {
			source:   "42 ; this is a comment",
			expected: []Expr{&Number{42}},
		},
		"comment at start": {
			source:   "; comment\n42",
			expected: []Expr{&Number{42}},
		},
		"multiple comments": {
			source:   "; first\n42 ; second\n; third",
			expected: []Expr{&Number{42}},
		},
		"comment with special chars": {
			source:   "; comment with () ' \" special chars\n42",
			expected: []Expr{&Number{42}},
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			prog, err := Parse(name, tc.source)
			if err != nil {
				t.Errorf("unexpected error: %v", err)
				return
			}

			if len(prog.Exprs) != len(tc.expected) {
				t.Errorf("expected %d expressions, got %d", len(tc.expected), len(prog.Exprs))
				return
			}

			for i, expected := range tc.expected {
				if !exprsEqual(expected, prog.Exprs[i]) {
					t.Errorf("expr %d: expected %#v, got %#v", i, expected, prog.Exprs[i])
				}
			}
		})
	}
}

func TestPositions(t *testing.T) {
	tests := map[string]struct {
		source      string
		expectedErr *ParseError
	}{
		"error on line 2": {
			source: "42\n(",
			expectedErr: &ParseError{
				Position: tokenizer.Position{Line: 2, Column: 2},
				Message:  "Unexpected token: expected expression, got TokenEOF",
			},
		},
		"error on line 3 column 5": {
			source: "42\n(+ 1\n    (",
			expectedErr: &ParseError{
				Position: tokenizer.Position{Line: 3, Column: 6},
				Message:  "Unexpected token: expected expression, got TokenEOF",
			},
		},
	}

	for name, tc := range tests {
		t.Run(name, func(t *testing.T) {
			_, err := Parse(name, tc.source)

			if err == nil {
				t.Errorf("expected error, got nil")
				return
			}

			var parseErr *ParseError
			if !errors.As(err, &parseErr) {
				t.Errorf("expected ParseError, got %T", err)
				return
			}

			if parseErr.Position.Line != tc.expectedErr.Position.Line ||
				parseErr.Position.Column != tc.expectedErr.Position.Column {
				t.Errorf("expected position %d:%d, got %d:%d",
					tc.expectedErr.Position.Line, tc.expectedErr.Position.Column,
					parseErr.Position.Line, parseErr.Position.Column)
			}

			if parseErr.Message != tc.expectedErr.Message {
				t.Errorf("expected message %q, got %q", tc.expectedErr.Message, parseErr.Message)
			}
		})
	}
}
