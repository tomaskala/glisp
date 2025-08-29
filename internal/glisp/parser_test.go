package glisp

import (
	"errors"
	"testing"
)

type parserTest struct {
	source string
	expr   Expr
	err    error
}

var simpleInputs = map[string]parserTest{
	"nil":            {source: "()", expr: Nil},
	"single element": {source: "(1)", expr: &Cons{&Number{1}, Nil}},
	"list":           {source: "(1 2 3)", expr: &Cons{&Number{1}, &Cons{&Number{2}, &Cons{&Number{3}, Nil}}}},
	"nested list": {
		source: "((1 2 3) (4 5 6) ((7 8) 9))",
		expr: &Cons{
			&Cons{&Number{1}, &Cons{&Number{2}, &Cons{&Number{3}, Nil}}},
			&Cons{
				&Cons{&Number{4}, &Cons{&Number{5}, &Cons{&Number{6}, Nil}}},
				&Cons{
					&Cons{
						&Cons{&Number{7}, &Cons{&Number{8}, Nil}},
						&Cons{&Number{9}, Nil},
					},
					Nil,
				},
			},
		},
	},
	"dotted list":     {source: "(1 2 3 . 4)", expr: &Cons{&Number{1}, &Cons{&Number{2}, &Cons{&Number{3}, &Number{4}}}}},
	"dotted pair":     {source: "(1 . 2)", expr: &Cons{&Number{1}, &Number{2}}},
	"odd dotted pair": {source: "(. 1)", expr: &Number{1}},
	"quoted atom":     {source: "'map", expr: &Cons{&Atom{"quote"}, &Cons{&Atom{"map"}, Nil}}},
	"quoted nil":      {source: "'()", expr: &Cons{&Atom{"quote"}, &Cons{Nil, Nil}}},
	"quoted list":     {source: "'(1 2 3)", expr: &Cons{&Atom{"quote"}, &Cons{&Cons{&Number{1}, &Cons{&Number{2}, &Cons{&Number{3}, Nil}}}, Nil}}},
	"quoted dot":      {source: "'(fun x y . z)", expr: &Cons{&Atom{"quote"}, &Cons{&Cons{&Atom{"fun"}, &Cons{&Atom{"x"}, &Cons{&Atom{"y"}, &Atom{"z"}}}}, Nil}}},
	"number dec":      {source: "44234", expr: &Number{44234}},
	"number hex":      {source: "0xabc123", expr: &Number{0xabc123}},
	"number oct":      {source: "0o755", expr: &Number{0o755}},
	"number bin":      {source: "0b001101", expr: &Number{0b001101}},
	"number float":    {source: "0.125", expr: &Number{0.125}},
	"number exp":      {source: "1e5", expr: &Number{1e5}},
	"number plus":     {source: "+111", expr: &Number{+111}},
	"number minus":    {source: "-0.75", expr: &Number{-0.75}},
	"atom filter":     {source: "filter", expr: &Atom{"filter"}},
	"atom plus":       {source: "+", expr: &Atom{"+"}},
	"atom long":       {source: "name-with-dashes", expr: &Atom{"name-with-dashes"}},
	"atom longer":     {source: "very-long-atom@with%some+signs", expr: &Atom{"very-long-atom@with%some+signs"}},
	"atom operator":   {source: "<*>", expr: &Atom{"<*>"}},
}

var complexInputs = map[string]parserTest{
	"flip": {
		source: "(lambda (x y) (list y x))",
		expr: &Cons{
			&Atom{"lambda"},
			&Cons{
				&Cons{
					&Atom{"x"},
					&Cons{
						&Atom{"y"},
						Nil,
					},
				},
				&Cons{
					&Cons{
						&Atom{"list"},
						&Cons{
							&Atom{"y"},
							&Cons{
								&Atom{"x"},
								Nil,
							},
						},
					},
					Nil,
				},
			},
		},
	},
	"map": {
		source: `; Map function implementation.
		(define map (lambda (f xs)
		  ; Base case.
		  (if (null? xs) '()
		      ; Recursive case.
		      (cons (f (car xs)) (map f (cdr xs))))))
		`,
		expr: &Cons{
			&Atom{"define"},
			&Cons{
				&Atom{"map"},
				&Cons{
					&Cons{
						&Atom{"lambda"},
						&Cons{
							&Cons{
								&Atom{"f"},
								&Cons{
									&Atom{"xs"},
									Nil,
								},
							},
							&Cons{
								&Cons{
									&Atom{"if"},
									&Cons{
										&Cons{
											&Atom{"null?"},
											&Cons{
												&Atom{"xs"},
												Nil,
											},
										},
										&Cons{
											&Cons{
												&Atom{"quote"},
												&Cons{
													Nil,
													Nil,
												},
											},
											&Cons{
												&Cons{
													&Atom{"cons"},
													&Cons{
														&Cons{
															&Atom{"f"},
															&Cons{
																&Cons{
																	&Atom{"car"},
																	&Cons{
																		&Atom{"xs"},
																		Nil,
																	},
																},
																Nil,
															},
														},
														&Cons{
															&Cons{
																&Atom{"map"},
																&Cons{
																	&Atom{"f"},
																	&Cons{
																		&Cons{
																			&Atom{"cdr"},
																			&Cons{
																				&Atom{"xs"},
																				Nil,
																			},
																		},
																		Nil,
																	},
																},
															},
															Nil,
														},
													},
												},
												Nil,
											},
										},
									},
								},
								Nil,
							},
						},
					},
					Nil,
				},
			},
		},
	},
}

var invalidInputs = map[string]parserTest{
	// This is valid, but is here so that we can easily check the err field.
	"empty":              {source: "", err: NewEOFError("empty", token{tokenEOF, 0, 1, "EOF"})},
	"unbalanced parens1": {source: "(+ 1 2 3", err: NewParseError("Unexpected end of file", "unbalanced parens1", token{tokenEOF, 8, 1, "EOF"})},
	"unbalanced parens2": {source: "(* (+ 1 2 3) (+ 4 5 6", err: NewParseError("Unexpected end of file", "unbalanced parens2", token{tokenEOF, 21, 1, "EOF"})},
	"unbalanced parens3": {source: "(((", err: NewParseError("Unexpected end of file", "unbalanced parens3", token{tokenEOF, 3, 1, "EOF"})},
	"unbalanced parens4": {source: ")(", err: NewParseError("Unexpected token", "unbalanced parens4", token{tokenRightParen, 1, 1, ")"})},
	"unterminated list":  {source: "(a b c d e f g h", err: NewParseError("Unexpected end of file", "unterminated list", token{tokenRightParen, 16, 1, "EOF"})},
	"unterminated dot":   {source: "(fun . x", err: NewParseError("Unexpected token, expected tokenRightParen", "unterminated dot", token{tokenEOF, 8, 1, "EOF"})},
}

func TestParser(t *testing.T) {
	t.Run("simple cases", func(t *testing.T) {
		for name, tc := range simpleInputs {
			parser := NewParser(name, tc.source)
			expr, err := parser.NextExpr()
			if err != nil {
				t.Errorf("%s: expected nil error, got %v", name, err)
			}
			if !expr.Equal(tc.expr) {
				t.Errorf("%s: expected %v, got %v", name, tc.expr, expr)
			}
			_, err = parser.NextExpr()
			var lispError LispError
			if errors.As(err, &lispError) && lispError.Type != ErrorEOF {
				t.Errorf("%s: expected EOF error, got %v", name, err)
			}
		}
	})
	t.Run("complex cases", func(t *testing.T) {
		for name, tc := range complexInputs {
			parser := NewParser(name, tc.source)
			expr, err := parser.NextExpr()
			if err != nil {
				t.Errorf("%s: expected nil error, got %v", name, err)
			}
			if !expr.Equal(tc.expr) {
				t.Errorf("%s: expected %v, got %v", name, tc.expr, expr)
			}
			_, err = parser.NextExpr()
			var lispError LispError
			if errors.As(err, &lispError) && lispError.Type != ErrorEOF {
				t.Errorf("%s: expected EOF error, got %v", name, err)
			}
		}
	})
	t.Run("invalid cases", func(t *testing.T) {
		for name, tc := range invalidInputs {
			parser := NewParser(name, tc.source)
			_, err := parser.NextExpr()
			if !errors.Is(err, tc.err) {
				t.Errorf("%s: expected error %v, got %v", name, tc.err, err)
			}
		}
	})
}
