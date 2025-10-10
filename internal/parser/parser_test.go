package parser

import (
	"errors"
	"testing"

	"github.com/google/go-cmp/cmp"
	"tomaskala.com/glisp/internal/ast"
	"tomaskala.com/glisp/internal/tokenizer"
)

type parserTest struct {
	source string
	Node   ast.Node
}

func newTestParser(name, source string) *Parser {
	tokenizer := tokenizer.NewTokenizer(source)
	parser := &Parser{name: name, tokenizer: tokenizer}
	parser.next() // Initialize the first token.
	return parser
}

func exprsEqual(expected, actual ast.Node) bool {
	skipTokens := cmp.FilterPath(func(p cmp.Path) bool {
		sf, ok := p.Last().(cmp.StructField)
		return ok && sf.Name() == "Tok"
	}, cmp.Ignore())
	return cmp.Equal(expected, actual, skipTokens)
}

func makeNil() *ast.Nil {
	return &ast.Nil{Tok: tokenizer.Token{}}
}

func makeAtom(name string) *ast.Atom {
	return &ast.Atom{Name: name, Tok: tokenizer.Token{}}
}

func makeNumber(value float64) *ast.Number {
	return &ast.Number{Value: value, Tok: tokenizer.Token{}}
}

func makeQuote(value ast.Node) *ast.Quote {
	return &ast.Quote{Value: value, Tok: tokenizer.Token{}}
}

func makeCall(function ast.Node, args []ast.Node) *ast.Call {
	return &ast.Call{Func: function, Args: args, Tok: tokenizer.Token{}}
}

func makeFunction(name string, params []string, restParam string, body ast.Node) *ast.Function {
	return &ast.Function{Name: name, Params: params, RestParam: restParam, Body: body, Tok: tokenizer.Token{}}
}

func makeDefine(name string, value ast.Node) *ast.Define {
	return &ast.Define{Name: name, Value: value, Tok: tokenizer.Token{}}
}

func makeIf(cond ast.Node, thenBranch ast.Node, elseBranch ast.Node) *ast.If {
	return &ast.If{Cond: cond, Then: thenBranch, Else: elseBranch, Tok: tokenizer.Token{}}
}

func makeLet(kind ast.LetKind, bindings []ast.Binding, body ast.Node) *ast.Let {
	return &ast.Let{Kind: kind, Bindings: bindings, Body: body, Tok: tokenizer.Token{}}
}

func makeCond(clauses []ast.CondClause) *ast.Cond {
	return &ast.Cond{Clauses: clauses, Tok: tokenizer.Token{}}
}

func makeCondClause(cond ast.Node, value ast.Node) ast.CondClause {
	return ast.CondClause{Cond: cond, Value: value}
}

func makeAnd(exprs []ast.Node) *ast.And {
	return &ast.And{Exprs: exprs, Tok: tokenizer.Token{}}
}

func makeOr(exprs []ast.Node) *ast.Or {
	return &ast.Or{Exprs: exprs, Tok: tokenizer.Token{}}
}

func makeSet(variable string, value ast.Node) *ast.Set {
	return &ast.Set{Variable: variable, Value: value, Tok: tokenizer.Token{}}
}

func makeBegin(exprs []ast.Node, tail ast.Node) *ast.Begin {
	return &ast.Begin{Exprs: exprs, Tail: tail, Tok: tokenizer.Token{}}
}

var atomTests = map[string]parserTest{
	"simple atom":    {source: "map", Node: makeAtom("map")},
	"atom with dash": {source: "list-ref", Node: makeAtom("list-ref")},
	"atom plus":      {source: "+", Node: makeAtom("+")},
	"atom minus":     {source: "-", Node: makeAtom("-")},
	"atom multiply":  {source: "*", Node: makeAtom("*")},
	"atom divide":    {source: "/", Node: makeAtom("/")},
	"atom question":  {source: "null?", Node: makeAtom("null?")},
	"atom with nums": {source: "val2num", Node: makeAtom("val2num")},
	"atom special":   {source: "odd@val-lisp", Node: makeAtom("odd@val-lisp")},
	"atom long":      {source: "very-long-atom@with%some+signs", Node: makeAtom("very-long-atom@with%some+signs")},
	"atom operator":  {source: "<*>", Node: makeAtom("<*>")},
	"atom boolean":   {source: "#t", Node: makeAtom("#t")},
	// Edge cases for atoms
	"atom underscore":    {source: "_", Node: makeAtom("_")},
	"atom with numbers":  {source: "var123", Node: makeAtom("var123")},
	"atom with unicode":  {source: "λ", Node: makeAtom("λ")},
	"atom mixed unicode": {source: "test-λ-func", Node: makeAtom("test-λ-func")},
	"atom exclamation":   {source: "not!", Node: makeAtom("not!")},
	"atom colon":         {source: "keyword:", Node: makeAtom("keyword:")},
	"atom pipe":          {source: "|special|", Node: makeAtom("|special|")},
	"atom backslash":     {source: "\\", Node: makeAtom("\\")},
	"atom caret":         {source: "^", Node: makeAtom("^")},
	"atom tilde":         {source: "~", Node: makeAtom("~")},
	"atom ampersand":     {source: "&", Node: makeAtom("&")},
}

var numberTests = map[string]parserTest{
	"integer":         {source: "42", Node: makeNumber(42)},
	"negative int":    {source: "-17", Node: makeNumber(-17)},
	"positive int":    {source: "+123", Node: makeNumber(123)},
	"zero":            {source: "0", Node: makeNumber(0)},
	"float":           {source: "3.14159", Node: makeNumber(3.14159)},
	"negative float":  {source: "-2.718", Node: makeNumber(-2.718)},
	"positive float":  {source: "+1.414", Node: makeNumber(1.414)},
	"hex":             {source: "0xFF", Node: makeNumber(255)},
	"hex negative":    {source: "-0xABC", Node: makeNumber(-2748)},
	"octal":           {source: "0o755", Node: makeNumber(493)},
	"octal negative":  {source: "-0o644", Node: makeNumber(-420)},
	"binary":          {source: "0b1101", Node: makeNumber(13)},
	"binary negative": {source: "-0b1010", Node: makeNumber(-10)},
	"scientific":      {source: "1.5e3", Node: makeNumber(1500)},
	"scientific neg":  {source: "-2.5E-2", Node: makeNumber(-0.025)},
	"hex float":       {source: "0x1.8p3", Node: makeNumber(12)},
	// Edge cases for numbers
	"hex lowercase":            {source: "0xabc", Node: makeNumber(2748)},
	"hex uppercase":            {source: "0XDEF", Node: makeNumber(3567)},
	"octal lowercase":          {source: "0o123", Node: makeNumber(83)},
	"octal uppercase":          {source: "0O456", Node: makeNumber(302)},
	"binary lowercase":         {source: "0b101", Node: makeNumber(5)},
	"binary uppercase":         {source: "0B110", Node: makeNumber(6)},
	"zero float":               {source: "0.0", Node: makeNumber(0.0)},
	"negative zero":            {source: "-0", Node: makeNumber(0)},
	"positive zero":            {source: "+0", Node: makeNumber(0)},
	"scientific uppercase":     {source: "1E10", Node: makeNumber(1e10)},
	"hex scientific":           {source: "0x1P4", Node: makeNumber(16)},
	"hex float negative":       {source: "-0x1.5p-1", Node: makeNumber(-0.65625)},
	"large integer":            {source: "9223372036854775807", Node: makeNumber(9.223372036854776e+18)},
	"very small float":         {source: "1e-100", Node: makeNumber(1e-100)},
	"numbers with underscores": {source: "1_000_000", Node: makeNumber(1000000)},
	"hex with underscores":     {source: "0xFF_FF", Node: makeNumber(65535)},
	"float with underscores":   {source: "3.141_592", Node: makeNumber(3.141592)},
}

var nilTests = map[string]parserTest{
	"empty list": {source: "()", Node: makeNil()},
}

var quoteTests = map[string]parserTest{
	"quote atom":   {source: "'x", Node: makeQuote(makeAtom("x"))},
	"quote number": {source: "'42", Node: makeQuote(makeNumber(42))},
	"quote nil":    {source: "'()", Node: makeQuote(makeNil())},
	"quote list": {
		source: "'(a b c)",
		Node:   makeQuote(makeCall(makeAtom("a"), []ast.Node{makeAtom("b"), makeAtom("c")})),
	},
	"long quote atom": {source: "(quote x)", Node: makeQuote(makeAtom("x"))},
	"long quote list": {source: "(quote (a b))", Node: makeQuote(makeCall(makeAtom("a"), []ast.Node{makeAtom("b")}))},
	// Edge cases for quotes
	"nested quotes": {source: "''x", Node: makeQuote(makeQuote(makeAtom("x")))},
	"quote complex expr": {
		source: "'(+ 1 (* 2 3))",
		Node: makeQuote(
			makeCall(
				makeAtom("+"),
				[]ast.Node{makeNumber(1), makeCall(makeAtom("*"), []ast.Node{makeNumber(2), makeNumber(3)})},
			),
		),
	},
	"quote lambda": {
		source: "'(lambda (x) x)",
		Node:   makeQuote(makeFunction("", []string{"x"}, "", makeAtom("x"))),
	},
	"long quote nested": {source: "(quote (quote x))", Node: makeQuote(makeQuote(makeAtom("x")))},
}

var callTests = map[string]parserTest{
	"lambda call": {
		source: "((lambda (x) x) 42)",
		Node:   makeCall(makeFunction("", []string{"x"}, "", makeAtom("x")), []ast.Node{makeNumber(42)}),
	},
	"nested lambda call": {
		source: "((lambda (f x) (f x)) + 5)",
		Node: makeCall(
			makeFunction("", []string{"f", "x"}, "", makeCall(makeAtom("f"), []ast.Node{makeAtom("x")})),
			[]ast.Node{makeAtom("+"), makeNumber(5)},
		),
	},
	"quoted lambda call": {
		source: "('(lambda (x) x) 'test)",
		Node: makeCall(
			makeQuote(makeFunction("", []string{"x"}, "", makeAtom("x"))),
			[]ast.Node{makeQuote(makeAtom("test"))},
		),
	},
	"let expression call": {
		source: "((let ((x 5)) (lambda (y) (+ x y))) 3)",
		Node: makeCall(
			makeLet(
				ast.LetPlain,
				[]ast.Binding{{Name: "x", Value: makeNumber(5)}},
				makeFunction("", []string{"y"}, "", makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeAtom("y")})),
			),
			[]ast.Node{makeNumber(3)},
		),
	},
	"let* expression call": {
		source: "((let* ((x 5)) (lambda (y) (+ x y))) 3)",
		Node: makeCall(
			makeLet(
				ast.LetStar,
				[]ast.Binding{{Name: "x", Value: makeNumber(5)}},
				makeFunction("", []string{"y"}, "", makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeAtom("y")})),
			),
			[]ast.Node{makeNumber(3)},
		),
	},
	"letrec expression call": {
		source: "((letrec ((x 5)) (lambda (y) (+ x y))) 3)",
		Node: makeCall(
			makeLet(
				ast.LetRec,
				[]ast.Binding{{Name: "x", Value: makeNumber(5)}},
				makeFunction("", []string{"y"}, "", makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeAtom("y")})),
			),
			[]ast.Node{makeNumber(3)},
		),
	},
	"call with no args": {
		source: "((lambda () 42))",
		Node:   makeCall(makeFunction("", nil, "", makeNumber(42)), nil),
	},
	"call with many args": {
		source: "((lambda (a b c d) (+ a b c d)) 1 2 3 4)",
		Node: makeCall(
			makeFunction(
				"",
				[]string{"a", "b", "c", "d"},
				"",
				makeCall(makeAtom("+"), []ast.Node{makeAtom("a"), makeAtom("b"), makeAtom("c"), makeAtom("d")}),
			),
			[]ast.Node{makeNumber(1), makeNumber(2), makeNumber(3), makeNumber(4)},
		),
	},
	"nested call": {
		source: "(((lambda (x) (lambda (y) (+ x y))) 5) 3)",
		Node: makeCall(
			makeCall(
				makeFunction(
					"",
					[]string{"x"},
					"",
					makeFunction(
						"",
						[]string{"y"},
						"",
						makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeAtom("y")}),
					),
				),
				[]ast.Node{makeNumber(5)},
			),
			[]ast.Node{makeNumber(3)},
		),
	},
}

var namedCallTests = map[string]parserTest{
	"simple call": {source: "(+ 1 2)", Node: makeCall(makeAtom("+"), []ast.Node{makeNumber(1), makeNumber(2)})},
	"nested call": {
		source: "(+ (* 2 3) 4)",
		Node: makeCall(
			makeAtom("+"),
			[]ast.Node{makeCall(makeAtom("*"), []ast.Node{makeNumber(2), makeNumber(3)}), makeNumber(4)},
		),
	},
	"no args": {source: "(list)", Node: makeCall(makeAtom("list"), nil)},
	"one arg": {source: "(car lst)", Node: makeCall(makeAtom("car"), []ast.Node{makeAtom("lst")})},
	"many args": {
		source: "(+ 1 2 3 4 5)",
		Node: makeCall(
			makeAtom("+"),
			[]ast.Node{makeNumber(1), makeNumber(2), makeNumber(3), makeNumber(4), makeNumber(5)},
		),
	},
	"call with quote": {
		source: "(cons 'a '())",
		Node:   makeCall(makeAtom("cons"), []ast.Node{makeQuote(makeAtom("a")), makeQuote(makeNil())}),
	},
	// Edge cases for named calls
	"deeply nested": {
		source: "(f (g (h (i j))))",
		Node: makeCall(
			makeAtom("f"),
			[]ast.Node{
				makeCall(
					makeAtom("g"),
					[]ast.Node{makeCall(makeAtom("h"), []ast.Node{makeCall(makeAtom("i"), []ast.Node{makeAtom("j")})})},
				),
			},
		),
	},
	"mixed expressions": {
		source: "(f 'a 42 (g x) '())",
		Node: makeCall(
			makeAtom("f"),
			[]ast.Node{
				makeQuote(makeAtom("a")),
				makeNumber(42),
				makeCall(makeAtom("g"), []ast.Node{makeAtom("x")}),
				makeQuote(makeNil()),
			},
		),
	},
	"special characters": {
		source: "(+ -1 +2)",
		Node:   makeCall(makeAtom("+"), []ast.Node{makeNumber(-1), makeNumber(2)}),
	},
}

var defineTests = map[string]parserTest{
	"define var": {source: "(define x 42)", Node: makeDefine("x", makeNumber(42))},
	"define func": {
		source: "(define factorial (lambda (n) (if (= n 0) 1 (* n (factorial (- n 1))))))",
		Node: makeDefine("factorial", makeFunction("factorial", []string{"n"}, "",
			makeIf(makeCall(makeAtom("="), []ast.Node{makeAtom("n"), makeNumber(0)}),
				makeNumber(1),
				makeCall(makeAtom("*"), []ast.Node{
					makeAtom("n"),
					makeCall(
						makeAtom("factorial"),
						[]ast.Node{makeCall(makeAtom("-"), []ast.Node{makeAtom("n"), makeNumber(1)})},
					),
				}),
			))),
	},
	"define atom": {source: "(define nil '())", Node: makeDefine("nil", makeQuote(makeNil()))},
	// Edge cases for define
	"define lambda no name": {
		source: "(define f (lambda (x) x))",
		Node:   makeDefine("f", makeFunction("f", []string{"x"}, "", makeAtom("x"))),
	},
	"define complex expr": {
		source: "(define result (+ (* 2 3) 4))",
		Node: makeDefine(
			"result",
			makeCall(
				makeAtom("+"),
				[]ast.Node{makeCall(makeAtom("*"), []ast.Node{makeNumber(2), makeNumber(3)}), makeNumber(4)},
			),
		),
	},
}

var lambdaTests = map[string]parserTest{
	"simple lambda":  {source: "(lambda (x) x)", Node: makeFunction("", []string{"x"}, "", makeAtom("x"))},
	"lambda no args": {source: "(lambda () 42)", Node: makeFunction("", nil, "", makeNumber(42))},
	"lambda multi args": {
		source: "(lambda (x y z) (+ x y z))",
		Node: makeFunction(
			"",
			[]string{"x", "y", "z"},
			"",
			makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeAtom("y"), makeAtom("z")}),
		),
	},
	"lambda rest param": {
		source: "(lambda (x . xs) (cons x xs))",
		Node: makeFunction(
			"",
			[]string{"x"},
			"xs",
			makeCall(makeAtom("cons"), []ast.Node{makeAtom("x"), makeAtom("xs")}),
		),
	},
	"lambda only rest": {source: "(lambda xs xs)", Node: makeFunction("", nil, "xs", makeAtom("xs"))},
	// Edge cases for lambda
	"lambda complex body": {
		source: "(lambda (x) (if (> x 0) x (- x)))",
		Node: makeFunction(
			"",
			[]string{"x"},
			"",
			makeIf(
				makeCall(makeAtom(">"), []ast.Node{makeAtom("x"), makeNumber(0)}),
				makeAtom("x"),
				makeCall(makeAtom("-"), []ast.Node{makeAtom("x")}),
			),
		),
	},
	"lambda nested": {
		source: "(lambda (f) (lambda (x) (f x)))",
		Node: makeFunction(
			"",
			[]string{"f"},
			"",
			makeFunction("", []string{"x"}, "", makeCall(makeAtom("f"), []ast.Node{makeAtom("x")})),
		),
	},
	"lambda with many params": {
		source: "(lambda (a b c d e f g) (+ a b c d e f g))",
		Node: makeFunction(
			"",
			[]string{"a", "b", "c", "d", "e", "f", "g"},
			"",
			makeCall(
				makeAtom("+"),
				[]ast.Node{
					makeAtom("a"),
					makeAtom("b"),
					makeAtom("c"),
					makeAtom("d"),
					makeAtom("e"),
					makeAtom("f"),
					makeAtom("g"),
				},
			),
		),
	},
	"lambda multiple rest": {
		source: "(lambda (x y . rest) rest)",
		Node:   makeFunction("", []string{"x", "y"}, "rest", makeAtom("rest")),
	},
}

var letTests = map[string]parserTest{
	"simple let": {
		source: "(let ((x 1)) x)",
		Node:   makeLet(ast.LetPlain, []ast.Binding{{Name: "x", Value: makeNumber(1)}}, makeAtom("x")),
	},
	"simple let*": {
		source: "(let* ((x 1)) x)",
		Node:   makeLet(ast.LetStar, []ast.Binding{{Name: "x", Value: makeNumber(1)}}, makeAtom("x")),
	},
	"simple letrec": {
		source: "(letrec ((x 1)) x)",
		Node:   makeLet(ast.LetRec, []ast.Binding{{Name: "x", Value: makeNumber(1)}}, makeAtom("x")),
	},
	"let multiple bindings": {
		source: "(let ((x 1) (y 2)) (+ x y))",
		Node: makeLet(
			ast.LetPlain,
			[]ast.Binding{{Name: "x", Value: makeNumber(1)}, {Name: "y", Value: makeNumber(2)}},
			makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeAtom("y")}),
		),
	},
	"let* multiple bindings": {
		source: "(let* ((x 1) (y 2)) (+ x y))",
		Node: makeLet(
			ast.LetStar,
			[]ast.Binding{{Name: "x", Value: makeNumber(1)}, {Name: "y", Value: makeNumber(2)}},
			makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeAtom("y")}),
		),
	},
	"letrec multiple bindings": {
		source: "(letrec ((x 1) (y 2)) (+ x y))",
		Node: makeLet(
			ast.LetRec,
			[]ast.Binding{{Name: "x", Value: makeNumber(1)}, {Name: "y", Value: makeNumber(2)}},
			makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeAtom("y")}),
		),
	},
	"let no bindings":    {source: "(let () 42)", Node: makeLet(ast.LetPlain, nil, makeNumber(42))},
	"let* no bindings":   {source: "(let* () 42)", Node: makeLet(ast.LetStar, nil, makeNumber(42))},
	"letrec no bindings": {source: "(letrec () 42)", Node: makeLet(ast.LetRec, nil, makeNumber(42))},
	"let nested": {
		source: "(let ((x 1)) (let ((y 2)) (+ x y)))",
		Node: makeLet(
			ast.LetPlain,
			[]ast.Binding{{Name: "x", Value: makeNumber(1)}},
			makeLet(
				ast.LetPlain,
				[]ast.Binding{{Name: "y", Value: makeNumber(2)}},
				makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeAtom("y")}),
			),
		),
	},
	"let* nested": {
		source: "(let* ((x 1)) (let* ((y 2)) (+ x y)))",
		Node: makeLet(
			ast.LetStar,
			[]ast.Binding{{Name: "x", Value: makeNumber(1)}},
			makeLet(
				ast.LetStar,
				[]ast.Binding{{Name: "y", Value: makeNumber(2)}},
				makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeAtom("y")}),
			),
		),
	},
	"letrec nested": {
		source: "(letrec ((x 1)) (letrec ((y 2)) (+ x y)))",
		Node: makeLet(
			ast.LetRec,
			[]ast.Binding{{Name: "x", Value: makeNumber(1)}},
			makeLet(
				ast.LetRec,
				[]ast.Binding{{Name: "y", Value: makeNumber(2)}},
				makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeAtom("y")}),
			),
		),
	},
	// Edge cases for let
	"let complex bindings": {
		source: "(let ((f (lambda (x) (* x x))) (y 5)) (f y))",
		Node: makeLet(
			ast.LetPlain,
			[]ast.Binding{
				{
					Name: "f",
					Value: makeFunction(
						"",
						[]string{"x"},
						"",
						makeCall(makeAtom("*"), []ast.Node{makeAtom("x"), makeAtom("x")}),
					),
				},
				{Name: "y", Value: makeNumber(5)},
			},
			makeCall(makeAtom("f"), []ast.Node{makeAtom("y")}),
		),
	},
	"let* complex bindings": {
		source: "(let* ((f (lambda (x) (* x x))) (y 5)) (f y))",
		Node: makeLet(
			ast.LetStar,
			[]ast.Binding{
				{
					Name: "f",
					Value: makeFunction(
						"",
						[]string{"x"},
						"",
						makeCall(makeAtom("*"), []ast.Node{makeAtom("x"), makeAtom("x")}),
					),
				},
				{Name: "y", Value: makeNumber(5)},
			},
			makeCall(makeAtom("f"), []ast.Node{makeAtom("y")}),
		),
	},
	"letrec complex bindings": {
		source: "(letrec ((f (lambda (x) (* x x))) (y 5)) (f y))",
		Node: makeLet(
			ast.LetRec,
			[]ast.Binding{
				{
					Name: "f",
					Value: makeFunction(
						"",
						[]string{"x"},
						"",
						makeCall(makeAtom("*"), []ast.Node{makeAtom("x"), makeAtom("x")}),
					),
				},
				{Name: "y", Value: makeNumber(5)},
			},
			makeCall(makeAtom("f"), []ast.Node{makeAtom("y")}),
		),
	},
	"let recursive binding": {
		source: "(let ((x (+ y 1)) (y 5)) x)",
		Node: makeLet(
			ast.LetPlain,
			[]ast.Binding{
				{Name: "x", Value: makeCall(makeAtom("+"), []ast.Node{makeAtom("y"), makeNumber(1)})},
				{Name: "y", Value: makeNumber(5)},
			},
			makeAtom("x"),
		),
	},
	"let* recursive binding": {
		source: "(let* ((x (+ y 1)) (y 5)) x)",
		Node: makeLet(
			ast.LetStar,
			[]ast.Binding{
				{Name: "x", Value: makeCall(makeAtom("+"), []ast.Node{makeAtom("y"), makeNumber(1)})},
				{Name: "y", Value: makeNumber(5)},
			},
			makeAtom("x"),
		),
	},
	"letrec recursive binding": {
		source: "(letrec ((x (+ y 1)) (y 5)) x)",
		Node: makeLet(
			ast.LetRec,
			[]ast.Binding{
				{Name: "x", Value: makeCall(makeAtom("+"), []ast.Node{makeAtom("y"), makeNumber(1)})},
				{Name: "y", Value: makeNumber(5)},
			},
			makeAtom("x"),
		),
	},
	"let shadowing": {
		source: "(let ((x 1)) (let ((x 2)) x))",
		Node: makeLet(
			ast.LetPlain,
			[]ast.Binding{{Name: "x", Value: makeNumber(1)}},
			makeLet(ast.LetPlain, []ast.Binding{{Name: "x", Value: makeNumber(2)}}, makeAtom("x")),
		),
	},
	"let* shadowing": {
		source: "(let* ((x 1)) (let* ((x 2)) x))",
		Node: makeLet(
			ast.LetStar,
			[]ast.Binding{{Name: "x", Value: makeNumber(1)}},
			makeLet(ast.LetStar, []ast.Binding{{Name: "x", Value: makeNumber(2)}}, makeAtom("x")),
		),
	},
	"letrec shadowing": {
		source: "(letrec ((x 1)) (letrec ((x 2)) x))",
		Node: makeLet(
			ast.LetRec,
			[]ast.Binding{{Name: "x", Value: makeNumber(1)}},
			makeLet(ast.LetRec, []ast.Binding{{Name: "x", Value: makeNumber(2)}}, makeAtom("x")),
		),
	},
}

var ifTests = map[string]parserTest{
	"simple if": {source: "(if #t 1 2)", Node: makeIf(makeAtom("#t"), makeNumber(1), makeNumber(2))},
	"if with call": {
		source: "(if (= x 0) 'zero 'nonzero)",
		Node: makeIf(
			makeCall(makeAtom("="), []ast.Node{makeAtom("x"), makeNumber(0)}),
			makeQuote(makeAtom("zero")),
			makeQuote(makeAtom("nonzero"))),
	},
	"nested if": {
		source: "(if a (if b c d) e)",
		Node: makeIf(makeAtom("a"),
			makeIf(makeAtom("b"), makeAtom("c"), makeAtom("d")),
			makeAtom("e")),
	},
	"if with complex expressions": {
		source: "(if (> x y) (+ x 1) (* y 2))",
		Node: makeIf(
			makeCall(makeAtom(">"), []ast.Node{makeAtom("x"), makeAtom("y")}),
			makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeNumber(1)}),
			makeCall(makeAtom("*"), []ast.Node{makeAtom("y"), makeNumber(2)})),
	},
	"if with lambda": {
		source: "(if test (lambda (x) x) (lambda (y) y))",
		Node: makeIf(makeAtom("test"),
			makeFunction("", []string{"x"}, "", makeAtom("x")),
			makeFunction("", []string{"y"}, "", makeAtom("y"))),
	},
}

var condTests = map[string]parserTest{
	"empty cond": {source: "(cond)", Node: makeCond(nil)},
	"single clause": {
		source: "(cond (#t 42))",
		Node:   makeCond([]ast.CondClause{makeCondClause(makeAtom("#t"), makeNumber(42))}),
	},
	"multiple clauses": {
		source: "(cond ((= x 0) 'zero) ((= x 1) 'one) (#t 'other))",
		Node: makeCond([]ast.CondClause{
			makeCondClause(
				makeCall(makeAtom("="), []ast.Node{makeAtom("x"), makeNumber(0)}),
				makeQuote(makeAtom("zero")),
			),
			makeCondClause(
				makeCall(makeAtom("="), []ast.Node{makeAtom("x"), makeNumber(1)}),
				makeQuote(makeAtom("one")),
			),
			makeCondClause(makeAtom("#t"), makeQuote(makeAtom("other"))),
		}),
	},
	"cond with complex expressions": {
		source: "(cond ((null? lst) '()) (#t (cons (car lst) (process (cdr lst)))))",
		Node: makeCond([]ast.CondClause{
			makeCondClause(makeCall(makeAtom("null?"), []ast.Node{makeAtom("lst")}), makeQuote(makeNil())),
			makeCondClause(makeAtom("#t"), makeCall(makeAtom("cons"), []ast.Node{
				makeCall(makeAtom("car"), []ast.Node{makeAtom("lst")}),
				makeCall(makeAtom("process"), []ast.Node{makeCall(makeAtom("cdr"), []ast.Node{makeAtom("lst")})}),
			})),
		}),
	},
	"nested cond": {
		source: "(cond (a (cond (b c) (#t d))) (#t e))",
		Node: makeCond([]ast.CondClause{
			makeCondClause(makeAtom("a"), makeCond([]ast.CondClause{
				makeCondClause(makeAtom("b"), makeAtom("c")),
				makeCondClause(makeAtom("#t"), makeAtom("d")),
			})),
			makeCondClause(makeAtom("#t"), makeAtom("e")),
		}),
	},
}

var andTests = map[string]parserTest{
	"empty and":    {source: "(and)", Node: makeAnd(nil)},
	"single and":   {source: "(and #t)", Node: makeAnd([]ast.Node{makeAtom("#t")})},
	"multiple and": {source: "(and a b c)", Node: makeAnd([]ast.Node{makeAtom("a"), makeAtom("b"), makeAtom("c")})},
	"and with calls": {
		source: "(and (> x 0) (< x 10))",
		Node: makeAnd([]ast.Node{
			makeCall(makeAtom(">"), []ast.Node{makeAtom("x"), makeNumber(0)}),
			makeCall(makeAtom("<"), []ast.Node{makeAtom("x"), makeNumber(10)}),
		}),
	},
	"nested and": {
		source: "(and a (and b c) d)",
		Node: makeAnd([]ast.Node{
			makeAtom("a"),
			makeAnd([]ast.Node{makeAtom("b"), makeAtom("c")}),
			makeAtom("d"),
		}),
	},
	"and with mixed types": {
		source: "(and 42 'symbol (list 1 2) #t)",
		Node: makeAnd([]ast.Node{
			makeNumber(42),
			makeQuote(makeAtom("symbol")),
			makeCall(makeAtom("list"), []ast.Node{makeNumber(1), makeNumber(2)}),
			makeAtom("#t"),
		}),
	},
}

var orTests = map[string]parserTest{
	"empty or":    {source: "(or)", Node: makeOr(nil)},
	"single or":   {source: "(or #t)", Node: makeOr([]ast.Node{makeAtom("#t")})},
	"multiple or": {source: "(or a b c)", Node: makeOr([]ast.Node{makeAtom("a"), makeAtom("b"), makeAtom("c")})},
	"or with calls": {
		source: "(or (null? lst) (empty? lst))",
		Node: makeOr([]ast.Node{
			makeCall(makeAtom("null?"), []ast.Node{makeAtom("lst")}),
			makeCall(makeAtom("empty?"), []ast.Node{makeAtom("lst")}),
		}),
	},
	"nested or": {
		source: "(or a (or b c) d)",
		Node: makeOr([]ast.Node{
			makeAtom("a"),
			makeOr([]ast.Node{makeAtom("b"), makeAtom("c")}),
			makeAtom("d"),
		}),
	},
	"or with mixed types": {
		source: "(or '() 0 \"\" #f)",
		Node: makeOr([]ast.Node{
			makeQuote(makeNil()),
			makeNumber(0),
			makeAtom("\"\""),
			makeAtom("#f"),
		}),
	},
}

var setTests = map[string]parserTest{
	"simple set": {source: "(set! x 42)", Node: makeSet("x", makeNumber(42))},
	"set with expression": {
		source: "(set! counter (+ counter 1))",
		Node:   makeSet("counter", makeCall(makeAtom("+"), []ast.Node{makeAtom("counter"), makeNumber(1)})),
	},
	"set with lambda": {
		source: "(set! handler (lambda (x) (* x x)))",
		Node: makeSet(
			"handler",
			makeFunction("", []string{"x"}, "", makeCall(makeAtom("*"), []ast.Node{makeAtom("x"), makeAtom("x")})),
		),
	},
	"set with call": {
		source: "(set! result (compute-value arg))",
		Node:   makeSet("result", makeCall(makeAtom("compute-value"), []ast.Node{makeAtom("arg")})),
	},
	"set with quote": {
		source: "(set! data '(1 2 3))",
		Node:   makeSet("data", makeQuote(makeCall(makeNumber(1), []ast.Node{makeNumber(2), makeNumber(3)}))),
	},
}

var beginTests = map[string]parserTest{
	"single expression": {
		source: "(begin 42)",
		Node:   makeBegin([]ast.Node{}, makeNumber(42)),
	},
	"two expressions": {
		source: "(begin 1 2)",
		Node:   makeBegin([]ast.Node{makeNumber(1)}, makeNumber(2)),
	},
	"multiple expressions": {
		source: "(begin 1 2 3 4)",
		Node:   makeBegin([]ast.Node{makeNumber(1), makeNumber(2), makeNumber(3)}, makeNumber(4)),
	},
	"begin with side effects": {
		source: "(begin (set! x 1) (set! y 2) (+ x y))",
		Node: makeBegin(
			[]ast.Node{
				makeSet("x", makeNumber(1)),
				makeSet("y", makeNumber(2)),
			},
			makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeAtom("y")}),
		),
	},
	"begin with function calls": {
		source: "(begin (print 'hello) (print 'world) 42)",
		Node: makeBegin(
			[]ast.Node{
				makeCall(makeAtom("print"), []ast.Node{makeQuote(makeAtom("hello"))}),
				makeCall(makeAtom("print"), []ast.Node{makeQuote(makeAtom("world"))}),
			},
			makeNumber(42),
		),
	},
	"nested begin": {
		source: "(begin (begin 1 2) 3)",
		Node: makeBegin(
			[]ast.Node{makeBegin([]ast.Node{makeNumber(1)}, makeNumber(2))},
			makeNumber(3),
		),
	},
	"begin with complex expressions": {
		source: "(begin (define f (lambda (x) x)) (f 42))",
		Node: makeBegin(
			[]ast.Node{makeDefine("f", makeFunction("f", []string{"x"}, "", makeAtom("x")))},
			makeCall(makeAtom("f"), []ast.Node{makeNumber(42)}),
		),
	},
	"begin with let": {
		source: "(begin (let ((x 1)) x) 'done)",
		Node: makeBegin(
			[]ast.Node{makeLet(ast.LetPlain, []ast.Binding{{Name: "x", Value: makeNumber(1)}}, makeAtom("x"))},
			makeQuote(makeAtom("done")),
		),
	},
	"begin with conditionals": {
		source: "(begin (if test (set! x 1) (set! x 2)) x)",
		Node: makeBegin(
			[]ast.Node{makeIf(makeAtom("test"), makeSet("x", makeNumber(1)), makeSet("x", makeNumber(2)))},
			makeAtom("x"),
		),
	},
	"long begin sequence": {
		source: "(begin 'a 'b 'c 'd 'e 'f)",
		Node: makeBegin(
			[]ast.Node{
				makeQuote(makeAtom("a")),
				makeQuote(makeAtom("b")),
				makeQuote(makeAtom("c")),
				makeQuote(makeAtom("d")),
				makeQuote(makeAtom("e")),
			},
			makeQuote(makeAtom("f")),
		),
	},
}

var complexTests = map[string]parserTest{
	"map function": {
		source: "(define map (lambda (f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs))))))",
		Node: makeDefine("map", makeFunction("map", []string{"f", "xs"}, "",
			makeIf(
				makeCall(makeAtom("null?"), []ast.Node{makeAtom("xs")}),
				makeQuote(makeNil()),
				makeCall(makeAtom("cons"), []ast.Node{
					makeCall(makeAtom("f"), []ast.Node{makeCall(makeAtom("car"), []ast.Node{makeAtom("xs")})}),
					makeCall(
						makeAtom("map"),
						[]ast.Node{makeAtom("f"), makeCall(makeAtom("cdr"), []ast.Node{makeAtom("xs")})},
					),
				}),
			))),
	},
	"nested structures let": {
		source: "(let ((f (lambda (x) (* x x)))) (f (+ 2 3)))",
		Node: makeLet(
			ast.LetPlain,
			[]ast.Binding{
				{
					Name: "f",
					Value: makeFunction(
						"",
						[]string{"x"},
						"",
						makeCall(makeAtom("*"), []ast.Node{makeAtom("x"), makeAtom("x")}),
					),
				},
			},
			makeCall(makeAtom("f"), []ast.Node{makeCall(makeAtom("+"), []ast.Node{makeNumber(2), makeNumber(3)})}),
		),
	},
	"nested structures let*": {
		source: "(let* ((f (lambda (x) (* x x)))) (f (+ 2 3)))",
		Node: makeLet(
			ast.LetStar,
			[]ast.Binding{
				{
					Name: "f",
					Value: makeFunction(
						"",
						[]string{"x"},
						"",
						makeCall(makeAtom("*"), []ast.Node{makeAtom("x"), makeAtom("x")}),
					),
				},
			},
			makeCall(makeAtom("f"), []ast.Node{makeCall(makeAtom("+"), []ast.Node{makeNumber(2), makeNumber(3)})}),
		),
	},
	"nested structures letrec": {
		source: "(letrec ((f (lambda (x) (* x x)))) (f (+ 2 3)))",
		Node: makeLet(
			ast.LetRec,
			[]ast.Binding{
				{
					Name: "f",
					Value: makeFunction(
						"",
						[]string{"x"},
						"",
						makeCall(makeAtom("*"), []ast.Node{makeAtom("x"), makeAtom("x")}),
					),
				},
			},
			makeCall(makeAtom("f"), []ast.Node{makeCall(makeAtom("+"), []ast.Node{makeNumber(2), makeNumber(3)})}),
		),
	},
	// More complex edge cases
	"y combinator": {
		source: "(lambda (f) ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))",
		Node: makeFunction(
			"",
			[]string{"f"},
			"",
			makeCall(
				makeFunction(
					"",
					[]string{"x"},
					"",
					makeCall(makeAtom("f"), []ast.Node{makeCall(makeAtom("x"), []ast.Node{makeAtom("x")})}),
				),
				[]ast.Node{
					makeFunction(
						"",
						[]string{"x"},
						"",
						makeCall(makeAtom("f"), []ast.Node{makeCall(makeAtom("x"), []ast.Node{makeAtom("x")})}),
					),
				},
			),
		),
	},
	"deeply nested let": {
		source: "(let ((a 1)) (let ((b 2)) (let ((c 3)) (+ a b c))))",
		Node: makeLet(
			ast.LetPlain,
			[]ast.Binding{{Name: "a", Value: makeNumber(1)}},
			makeLet(
				ast.LetPlain,
				[]ast.Binding{{Name: "b", Value: makeNumber(2)}},
				makeLet(
					ast.LetPlain,
					[]ast.Binding{{Name: "c", Value: makeNumber(3)}},
					makeCall(makeAtom("+"), []ast.Node{makeAtom("a"), makeAtom("b"), makeAtom("c")}),
				),
			),
		),
	},
	"deeply nested let*": {
		source: "(let* ((a 1)) (let* ((b 2)) (let* ((c 3)) (+ a b c))))",
		Node: makeLet(
			ast.LetStar,
			[]ast.Binding{{Name: "a", Value: makeNumber(1)}},
			makeLet(
				ast.LetStar,
				[]ast.Binding{{Name: "b", Value: makeNumber(2)}},
				makeLet(
					ast.LetStar,
					[]ast.Binding{{Name: "c", Value: makeNumber(3)}},
					makeCall(makeAtom("+"), []ast.Node{makeAtom("a"), makeAtom("b"), makeAtom("c")}),
				),
			),
		),
	},
	"deeply nested letrec": {
		source: "(letrec ((a 1)) (letrec ((b 2)) (letrec ((c 3)) (+ a b c))))",
		Node: makeLet(
			ast.LetRec,
			[]ast.Binding{{Name: "a", Value: makeNumber(1)}},
			makeLet(
				ast.LetRec,
				[]ast.Binding{{Name: "b", Value: makeNumber(2)}},
				makeLet(
					ast.LetRec,
					[]ast.Binding{{Name: "c", Value: makeNumber(3)}},
					makeCall(makeAtom("+"), []ast.Node{makeAtom("a"), makeAtom("b"), makeAtom("c")}),
				),
			),
		),
	},
}

var errorTests = map[string]struct {
	source       string
	expectedMsg  string
	expectedLine int
}{
	"unmatched paren": {
		source:       "(+ 1 2",
		expectedMsg:  "unexpected token: expected expression, got TokenEOF",
		expectedLine: 1,
	},
	"incomplete define": {
		source:       "(define x)",
		expectedMsg:  "unexpected token: expected expression, got TokenRightParen",
		expectedLine: 1,
	},
	"incomplete lambda": {
		source:       "(lambda (x))",
		expectedMsg:  "unexpected token: expected expression, got TokenRightParen",
		expectedLine: 1,
	},
	"incomplete let": {
		source:       "(let ((x 1)))",
		expectedMsg:  "unexpected token: expected expression, got TokenRightParen",
		expectedLine: 1,
	},
	"malformed let": {
		source:       "(let (x 1) x)",
		expectedMsg:  "unexpected token: expected TokenLeftParen, got TokenAtom",
		expectedLine: 1,
	},
	"malformed binding": {
		source:       "(let ((x)) x)",
		expectedMsg:  "unexpected token: expected expression, got TokenRightParen",
		expectedLine: 1,
	},
	"incomplete quote": {
		source:       "'",
		expectedMsg:  "unexpected token: expected expression, got TokenEOF",
		expectedLine: 1,
	},
	"bad lambda params": {
		source:       "(lambda 123 x)",
		expectedMsg:  "unexpected lambda parameter: TokenNumber",
		expectedLine: 1,
	},
	"unterminated quote": {
		source:       "(quote",
		expectedMsg:  "unexpected token: expected expression, got TokenEOF",
		expectedLine: 1,
	},
	"empty define": {
		source:       "(define)",
		expectedMsg:  "unexpected token: expected TokenAtom, got TokenRightParen",
		expectedLine: 1,
	},
	"define with number": {
		source:       "(define 123 x)",
		expectedMsg:  "unexpected token: expected TokenAtom, got TokenNumber",
		expectedLine: 1,
	},
	"let missing bindings": {
		source:       "(let)",
		expectedMsg:  "unexpected token: expected TokenLeftParen, got TokenRightParen",
		expectedLine: 1,
	},
	"let unclosed bindings": {
		source:       "(let ((x 1)",
		expectedMsg:  "unexpected token: expected TokenLeftParen, got TokenEOF",
		expectedLine: 1,
	},
	"lambda missing params": {
		source:       "(lambda)",
		expectedMsg:  "unexpected lambda parameter: TokenRightParen",
		expectedLine: 1,
	},
	"dot without rest param": {
		source:       "(lambda (x .) body)",
		expectedMsg:  "unexpected token: expected TokenAtom, got TokenRightParen",
		expectedLine: 1,
	},
	"multiple dots": {
		source:       "(lambda (x . y . z) body)",
		expectedMsg:  "unexpected token: expected TokenRightParen, got TokenDot",
		expectedLine: 1,
	},
	"invalid token": {
		source:       "(",
		expectedMsg:  "unexpected token: expected expression, got TokenEOF",
		expectedLine: 1,
	},
	"nested unmatched": {
		source:       "(+ (- 3",
		expectedMsg:  "unexpected token: expected expression, got TokenEOF",
		expectedLine: 1,
	},
	"quote missing expr": {
		source:       "(quote)",
		expectedMsg:  "unexpected token: expected expression, got TokenRightParen",
		expectedLine: 1,
	},
	"invalid number": {
		source:       "0xGHI",
		expectedMsg:  "Atom cannot start with a number",
		expectedLine: 1,
	},
	"incomplete if": {
		source:       "(if #t)",
		expectedMsg:  "unexpected token: expected expression, got TokenRightParen",
		expectedLine: 1,
	},
	"if missing else": {
		source:       "(if #t 1)",
		expectedMsg:  "unexpected token: expected expression, got TokenRightParen",
		expectedLine: 1,
	},
	"malformed cond clause": {
		source:       "(cond (a))",
		expectedMsg:  "unexpected token: expected expression, got TokenRightParen",
		expectedLine: 1,
	},
	"incomplete set": {
		source:       "(set! x)",
		expectedMsg:  "unexpected token: expected expression, got TokenRightParen",
		expectedLine: 1,
	},
	"set with number": {
		source:       "(set! 123 value)",
		expectedMsg:  "unexpected token: expected TokenAtom, got TokenNumber",
		expectedLine: 1,
	},
	"empty begin": {
		source:       "(begin)",
		expectedMsg:  "begin requires at least one expression",
		expectedLine: 1,
	},
}

var programTests = map[string]struct {
	source string
	exprs  []ast.Node
}{
	"single expr": {source: "42", exprs: []ast.Node{makeNumber(42)}},
	"multiple exprs": {
		source: "1 2 'hello",
		exprs:  []ast.Node{makeNumber(1), makeNumber(2), makeQuote(makeAtom("hello"))},
	},
	"mixed exprs": {source: "(define x 1) (+ x 2)", exprs: []ast.Node{
		makeDefine("x", makeNumber(1)),
		makeCall(makeAtom("+"), []ast.Node{makeAtom("x"), makeNumber(2)}),
	}},
	"empty program":   {source: "", exprs: []ast.Node{}},
	"whitespace only": {source: "   \n\t  ", exprs: []ast.Node{}},
	"comments only":   {source: "; This is a comment\n; Another comment", exprs: []ast.Node{}},
	"mixed with comments": {
		source: "; Start\n42 ; middle\n'hello ; end",
		exprs:  []ast.Node{makeNumber(42), makeQuote(makeAtom("hello"))},
	},
}

func TestParseAtoms(t *testing.T) {
	for name, tc := range atomTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseNumbers(t *testing.T) {
	for name, tc := range numberTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseNil(t *testing.T) {
	for name, tc := range nilTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseQuotes(t *testing.T) {
	for name, tc := range quoteTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseCalls(t *testing.T) {
	for name, tc := range callTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
			if _, ok := expr.(*ast.Call); !ok {
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
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
			if _, ok := expr.(*ast.Call); !ok {
				t.Errorf("expected Call, got %T", expr)
			}
		})
	}
}

func TestParseDefines(t *testing.T) {
	for name, tc := range defineTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseLambdas(t *testing.T) {
	for name, tc := range lambdaTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseLets(t *testing.T) {
	for name, tc := range letTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseIfs(t *testing.T) {
	for name, tc := range ifTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseConds(t *testing.T) {
	for name, tc := range condTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseAnds(t *testing.T) {
	for name, tc := range andTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseOrs(t *testing.T) {
	for name, tc := range orTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseSets(t *testing.T) {
	for name, tc := range setTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseBegins(t *testing.T) {
	for name, tc := range beginTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
			}
		})
	}
}

func TestParseComplex(t *testing.T) {
	for name, tc := range complexTests {
		t.Run(name, func(t *testing.T) {
			parser := newTestParser(name, tc.source)
			expr := parser.parseExpr()
			if !exprsEqual(tc.Node, expr) {
				t.Errorf("expected %#v, got %#v", tc.Node, expr)
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

			if tc.expectedLine != 0 {
				if parseErr.Line != tc.expectedLine {
					t.Errorf("expected error at %d, got %d", tc.expectedLine, parseErr.Line)
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
	expected := []ast.Node{makeAtom("a"), makeAtom("b"), makeAtom("c")}
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
	if !cmp.Equal(args, expected) {
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
	if !cmp.Equal(args, expected) {
		t.Errorf("expected %v, got %v", expected, args)
	}
	if dotArg != "rest" {
		t.Errorf("expected dot arg 'rest', got %s", dotArg)
	}
}

func TestParseEdgeCases(t *testing.T) {
	tests := map[string]struct {
		source   string
		expected ast.Node
	}{
		"signed atom": {
			source:   "+symbol",
			expected: makeAtom("+symbol"),
		},
		"negative atom": {
			source:   "-symbol",
			expected: makeAtom("-symbol"),
		},
		"complex nested quote": {
			source:   "'''x",
			expected: makeQuote(makeQuote(makeQuote(makeAtom("x")))),
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

func TestWhitespaceAndComments(t *testing.T) {
	tests := map[string]struct {
		source   string
		expected []ast.Node
	}{
		"leading whitespace": {
			source:   "   42",
			expected: []ast.Node{makeNumber(42)},
		},
		"trailing whitespace": {
			source:   "42   ",
			expected: []ast.Node{makeNumber(42)},
		},
		"mixed whitespace": {
			source:   "\t\n  42  \n\t",
			expected: []ast.Node{makeNumber(42)},
		},
		"comment at end": {
			source:   "42 ; this is a comment",
			expected: []ast.Node{makeNumber(42)},
		},
		"comment at start": {
			source:   "; comment\n42",
			expected: []ast.Node{makeNumber(42)},
		},
		"multiple comments": {
			source:   "; first\n42 ; second\n; third",
			expected: []ast.Node{makeNumber(42)},
		},
		"comment with special chars": {
			source:   "; comment with () ' \" special chars\n42",
			expected: []ast.Node{makeNumber(42)},
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

func TestLineNumbers(t *testing.T) {
	tests := map[string]struct {
		source      string
		expectedErr *ParseError
	}{
		"error on line 2": {
			source: "42\n(",
			expectedErr: &ParseError{
				Line:    2,
				Message: "unexpected token: expected expression, got TokenEOF",
			},
		},
		"error on line 3 column 5": {
			source: "42\n(+ 1\n    (",
			expectedErr: &ParseError{
				Line:    3,
				Message: "unexpected token: expected expression, got TokenEOF",
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

			if parseErr.Line != tc.expectedErr.Line {
				t.Errorf("expected position %d, got %d", tc.expectedErr.Line, parseErr.Line)
			}

			if parseErr.Message != tc.expectedErr.Message {
				t.Errorf("expected message %q, got %q", tc.expectedErr.Message, parseErr.Message)
			}
		})
	}
}
