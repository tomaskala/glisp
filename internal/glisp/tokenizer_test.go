package glisp

import "testing"

type simpleTest struct {
	source string
	token  token
}

type complexTest struct {
	source string
	tokens []token
}

var simpleCases = map[string]simpleTest{
	"leftParen":         {"(", token{tokenLeftParen, 1, 1, "("}},
	"rightParen":        {")", token{tokenRightParen, 1, 1, ")"}},
	"dot":               {".", token{tokenDot, 1, 1, "."}},
	"quote":             {"'", token{tokenQuote, 1, 1, "'"}},
	"number dec":        {"134", token{tokenNumber, 3, 1, "134"}},
	"number dec+":       {"+49405", token{tokenNumber, 6, 1, "+49405"}},
	"number dec-":       {"-13", token{tokenNumber, 3, 1, "-13"}},
	"number hex":        {"0xdead1cafe2", token{tokenNumber, 12, 1, "0xdead1cafe2"}},
	"number hex+":       {"+0x10dead20cafe", token{tokenNumber, 15, 1, "+0x10dead20cafe"}},
	"number hex-":       {"-0X123ABCDEFabcdef", token{tokenNumber, 18, 1, "-0X123ABCDEFabcdef"}},
	"number oct":        {"0o751", token{tokenNumber, 5, 1, "0o751"}},
	"number oct+":       {"+0O755", token{tokenNumber, 6, 1, "+0O755"}},
	"number oct-":       {"-0o123", token{tokenNumber, 6, 1, "-0o123"}},
	"number bin":        {"0b0101", token{tokenNumber, 6, 1, "0b0101"}},
	"number bin+":       {"+0B111", token{tokenNumber, 6, 1, "+0B111"}},
	"number bin-":       {"-0b001", token{tokenNumber, 6, 1, "-0b001"}},
	"number dec float":  {"85943.45", token{tokenNumber, 8, 1, "85943.45"}},
	"number dec float+": {"+39.332442234", token{tokenNumber, 13, 1, "+39.332442234"}},
	"number dec float-": {"-111.111111", token{tokenNumber, 11, 1, "-111.111111"}},
	"number hex float":  {"0x1p-2", token{tokenNumber, 6, 1, "0x1p-2"}},
	"number hex float+": {"+0x1.Fp+0", token{tokenNumber, 9, 1, "+0x1.Fp+0"}},
	"number hex float-": {"-0x1DEFp-2", token{tokenNumber, 10, 1, "-0x1DEFp-2"}},
	"number dec exp":    {"6.67428e-11", token{tokenNumber, 11, 1, "6.67428e-11"}},
	"number dec exp+":   {"+1E6", token{tokenNumber, 4, 1, "+1E6"}},
	"number dec exp-":   {"-0.12345E+5", token{tokenNumber, 11, 1, "-0.12345E+5"}},
	"atom alpha":        {"filter", token{tokenAtom, 6, 1, "filter"}},
	"atom alphanum":     {"val2num", token{tokenAtom, 7, 1, "val2num"}},
	"atom specials":     {"odd@val-lisp", token{tokenAtom, 12, 1, "odd@val-lisp"}},
	"atom long":         {"long-dash-separated+but@with%specials#", token{tokenAtom, 38, 1, "long-dash-separated+but@with%specials#"}},
	"atom japanese":     {"こんにちは", token{tokenAtom, 15, 1, "こんにちは"}},
	"atom +":            {"+", token{tokenAtom, 1, 1, "+"}},
	"atom -":            {"-", token{tokenAtom, 1, 1, "-"}},
	"atom *":            {"*", token{tokenAtom, 1, 1, "*"}},
	"atom /":            {"/", token{tokenAtom, 1, 1, "/"}},
	"atom #t":           {"#t", token{tokenAtom, 2, 1, "#t"}},
	"atom <>":           {"<>", token{tokenAtom, 2, 1, "<>"}},
	"atom <|>":          {"<|>", token{tokenAtom, 3, 1, "<|>"}},
	"atom <*>":          {"<*>", token{tokenAtom, 3, 1, "<*>"}},
}

var complexCases = map[string]complexTest{
	"flip": {
		"(lambda (x y) (list y x))",
		[]token{
			{tokenLeftParen, 1, 1, "("},
			{tokenAtom, 7, 1, "lambda"},
			{tokenLeftParen, 9, 1, "("},
			{tokenAtom, 10, 1, "x"},
			{tokenAtom, 12, 1, "y"},
			{tokenRightParen, 13, 1, ")"},
			{tokenLeftParen, 15, 1, "("},
			{tokenAtom, 19, 1, "list"},
			{tokenAtom, 21, 1, "y"},
			{tokenAtom, 23, 1, "x"},
			{tokenRightParen, 24, 1, ")"},
			{tokenRightParen, 25, 1, ")"},
		},
	},
	"multiline": {
		`; Map function implementation.
		(define map (lambda (f xs)
		  ; Base case.
		  (if (null? xs) '()
		      ; Recursive case.
		      (cons (f (car xs)) (map f (cdr xs))))))
		`,
		[]token{
			{tokenLeftParen, 34, 2, "("},
			{tokenAtom, 40, 2, "define"},
			{tokenAtom, 44, 2, "map"},
			{tokenLeftParen, 46, 2, "("},
			{tokenAtom, 52, 2, "lambda"},
			{tokenLeftParen, 54, 2, "("},
			{tokenAtom, 55, 2, "f"},
			{tokenAtom, 58, 2, "xs"},
			{tokenRightParen, 59, 2, ")"},
			{tokenLeftParen, 82, 4, "("},
			{tokenAtom, 84, 4, "if"},
			{tokenLeftParen, 86, 4, "("},
			{tokenAtom, 91, 4, "null?"},
			{tokenAtom, 94, 4, "xs"},
			{tokenRightParen, 95, 4, ")"},
			{tokenQuote, 97, 4, "'"},
			{tokenLeftParen, 98, 4, "("},
			{tokenRightParen, 99, 4, ")"},
			{tokenLeftParen, 135, 6, "("},
			{tokenAtom, 139, 6, "cons"},
			{tokenLeftParen, 141, 6, "("},
			{tokenAtom, 142, 6, "f"},
			{tokenLeftParen, 144, 6, "("},
			{tokenAtom, 147, 6, "car"},
			{tokenAtom, 150, 6, "xs"},
			{tokenRightParen, 151, 6, ")"},
			{tokenRightParen, 152, 6, ")"},
			{tokenLeftParen, 154, 6, "("},
			{tokenAtom, 157, 6, "map"},
			{tokenAtom, 159, 6, "f"},
			{tokenLeftParen, 161, 6, "("},
			{tokenAtom, 164, 6, "cdr"},
			{tokenAtom, 167, 6, "xs"},
			{tokenRightParen, 168, 6, ")"},
			{tokenRightParen, 169, 6, ")"},
			{tokenRightParen, 170, 6, ")"},
			{tokenRightParen, 171, 6, ")"},
			{tokenRightParen, 172, 6, ")"},
			{tokenRightParen, 173, 6, ")"},
		},
	},
	"number and atom": {
		"77atom",
		[]token{
			{tokenNumber, 2, 1, "77"},
			{tokenAtom, 6, 1, "atom"},
		},
	},
}

func TestTokenizer(t *testing.T) {
	t.Run("simple cases", func(t *testing.T) {
		for name, tc := range simpleCases {
			tokenizer := newTokenizer(name, tc.source)
			token := tokenizer.nextToken()
			if token != tc.token {
				t.Errorf("%s: expected %#v, got %#v", name, tc.token, token)
			}
			token = tokenizer.nextToken()
			if token.typ != tokenEOF {
				t.Errorf("%s: expected EOF at the end, got %#v", name, token)
			}
		}
	})
	t.Run("complex cases", func(t *testing.T) {
		for name, tc := range complexCases {
			tokenizer := newTokenizer(name, tc.source)
			for _, expected := range tc.tokens {
				token := tokenizer.nextToken()
				if token != expected {
					t.Errorf("%s: expected %#v, got %#v", name, expected, token)
				}
			}
			token := tokenizer.nextToken()
			if token.typ != tokenEOF {
				t.Errorf("%s: expected EOF at the end, got %#v", name, token)
			}
		}
	})
}
