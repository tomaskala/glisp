package tokenizer

import "testing"

type simpleTest struct {
	source string
	token  Token
}

type complexTest struct {
	source string
	tokens []Token
}

var simpleCases = map[string]simpleTest{
	"leftParen":         {"(", Token{TokenLeftParen, Position{1, 1}, "("}},
	"rightParen":        {")", Token{TokenRightParen, Position{1, 1}, ")"}},
	"dot":               {".", Token{TokenDot, Position{1, 1}, "."}},
	"quote":             {"'", Token{TokenQuote, Position{1, 1}, "'"}},
	"number dec":        {"134", Token{TokenNumber, Position{1, 1}, "134"}},
	"number dec+":       {"+49405", Token{TokenNumber, Position{1, 1}, "+49405"}},
	"number dec-":       {"-13", Token{TokenNumber, Position{1, 1}, "-13"}},
	"number hex":        {"0xdead1cafe2", Token{TokenNumber, Position{1, 1}, "0xdead1cafe2"}},
	"number hex+":       {"+0x10dead20cafe", Token{TokenNumber, Position{1, 1}, "+0x10dead20cafe"}},
	"number hex-":       {"-0X123ABCDEFabcdef", Token{TokenNumber, Position{1, 1}, "-0X123ABCDEFabcdef"}},
	"number oct":        {"0o751", Token{TokenNumber, Position{1, 1}, "0o751"}},
	"number oct+":       {"+0O755", Token{TokenNumber, Position{1, 1}, "+0O755"}},
	"number oct-":       {"-0o123", Token{TokenNumber, Position{1, 1}, "-0o123"}},
	"number bin":        {"0b0101", Token{TokenNumber, Position{1, 1}, "0b0101"}},
	"number bin+":       {"+0B111", Token{TokenNumber, Position{1, 1}, "+0B111"}},
	"number bin-":       {"-0b001", Token{TokenNumber, Position{1, 1}, "-0b001"}},
	"number dec float":  {"85943.45", Token{TokenNumber, Position{1, 1}, "85943.45"}},
	"number dec float+": {"+39.332442234", Token{TokenNumber, Position{1, 1}, "+39.332442234"}},
	"number dec float-": {"-111.111111", Token{TokenNumber, Position{1, 1}, "-111.111111"}},
	"number hex float":  {"0x1p-2", Token{TokenNumber, Position{1, 1}, "0x1p-2"}},
	"number hex float+": {"+0x1.Fp+0", Token{TokenNumber, Position{1, 1}, "+0x1.Fp+0"}},
	"number hex float-": {"-0x1DEFp-2", Token{TokenNumber, Position{1, 1}, "-0x1DEFp-2"}},
	"number dec exp":    {"6.67428e-11", Token{TokenNumber, Position{1, 1}, "6.67428e-11"}},
	"number dec exp+":   {"+1E6", Token{TokenNumber, Position{1, 1}, "+1E6"}},
	"number dec exp-":   {"-0.12345E+5", Token{TokenNumber, Position{1, 1}, "-0.12345E+5"}},
	"atom alpha":        {"filter", Token{TokenAtom, Position{1, 1}, "filter"}},
	"atom alphanum":     {"val2num", Token{TokenAtom, Position{1, 1}, "val2num"}},
	"atom specials":     {"odd@val-lisp", Token{TokenAtom, Position{1, 1}, "odd@val-lisp"}},
	"atom long":         {"long-dash-separated+but@with%specials#", Token{TokenAtom, Position{1, 1}, "long-dash-separated+but@with%specials#"}},
	"atom japanese":     {"こんにちは", Token{TokenAtom, Position{1, 1}, "こんにちは"}},
	"atom +":            {"+", Token{TokenAtom, Position{1, 1}, "+"}},
	"atom -":            {"-", Token{TokenAtom, Position{1, 1}, "-"}},
	"atom *":            {"*", Token{TokenAtom, Position{1, 1}, "*"}},
	"atom /":            {"/", Token{TokenAtom, Position{1, 1}, "/"}},
	"atom #t":           {"#t", Token{TokenAtom, Position{1, 1}, "#t"}},
	"atom <>":           {"<>", Token{TokenAtom, Position{1, 1}, "<>"}},
	"atom <|>":          {"<|>", Token{TokenAtom, Position{1, 1}, "<|>"}},
	"atom <*>":          {"<*>", Token{TokenAtom, Position{1, 1}, "<*>"}},
}

var complexCases = map[string]complexTest{
	"flip": {
		"(lambda (x y) (list y x))",
		[]Token{
			{TokenLeftParen, Position{1, 1}, "("},
			{TokenAtom, Position{1, 2}, "lambda"},
			{TokenLeftParen, Position{1, 9}, "("},
			{TokenAtom, Position{1, 10}, "x"},
			{TokenAtom, Position{1, 12}, "y"},
			{TokenRightParen, Position{1, 13}, ")"},
			{TokenLeftParen, Position{1, 15}, "("},
			{TokenAtom, Position{1, 16}, "list"},
			{TokenAtom, Position{1, 21}, "y"},
			{TokenAtom, Position{1, 23}, "x"},
			{TokenRightParen, Position{1, 24}, ")"},
			{TokenRightParen, Position{1, 25}, ")"},
		},
	},
	"multiline": {
		`; Map function implementation.
		(define     map		(lambda 
	(f xs)
		  ; Base case.
		  (		 if (	null? xs) '()
				; Recursive case.
					(cons (f (car xs)) (map f (cdr xs))))))
		`,
		[]Token{
			{TokenLeftParen, Position{2, 3}, "("},
			{TokenAtom, Position{2, 4}, "define"},
			{TokenAtom, Position{2, 15}, "map"},
			{TokenLeftParen, Position{2, 20}, "("},
			{TokenAtom, Position{2, 21}, "lambda"},
			{TokenLeftParen, Position{3, 2}, "("},
			{TokenAtom, Position{3, 3}, "f"},
			{TokenAtom, Position{3, 5}, "xs"},
			{TokenRightParen, Position{3, 7}, ")"},
			{TokenLeftParen, Position{5, 5}, "("},
			{TokenAtom, Position{5, 9}, "if"},
			{TokenLeftParen, Position{5, 12}, "("},
			{TokenAtom, Position{5, 14}, "null?"},
			{TokenAtom, Position{5, 20}, "xs"},
			{TokenRightParen, Position{5, 22}, ")"},
			{TokenQuote, Position{5, 24}, "'"},
			{TokenLeftParen, Position{5, 25}, "("},
			{TokenRightParen, Position{5, 26}, ")"},
			{TokenLeftParen, Position{7, 6}, "("},
			{TokenAtom, Position{7, 7}, "cons"},
			{TokenLeftParen, Position{7, 12}, "("},
			{TokenAtom, Position{7, 13}, "f"},
			{TokenLeftParen, Position{7, 15}, "("},
			{TokenAtom, Position{7, 16}, "car"},
			{TokenAtom, Position{7, 20}, "xs"},
			{TokenRightParen, Position{7, 22}, ")"},
			{TokenRightParen, Position{7, 23}, ")"},
			{TokenLeftParen, Position{7, 25}, "("},
			{TokenAtom, Position{7, 26}, "map"},
			{TokenAtom, Position{7, 30}, "f"},
			{TokenLeftParen, Position{7, 32}, "("},
			{TokenAtom, Position{7, 33}, "cdr"},
			{TokenAtom, Position{7, 37}, "xs"},
			{TokenRightParen, Position{7, 39}, ")"},
			{TokenRightParen, Position{7, 40}, ")"},
			{TokenRightParen, Position{7, 41}, ")"},
			{TokenRightParen, Position{7, 42}, ")"},
			{TokenRightParen, Position{7, 43}, ")"},
			{TokenRightParen, Position{7, 44}, ")"},
		},
	},
	"number and atom": {
		"77atom",
		[]Token{
			{TokenNumber, Position{1, 1}, "77"},
			{TokenAtom, Position{1, 3}, "atom"},
		},
	},
}

func TestTokenizer(t *testing.T) {
	t.Run("simple cases", func(t *testing.T) {
		for name, tc := range simpleCases {
			tokenizer := NewTokenizer(tc.source)
			token := tokenizer.NextToken()
			if token != tc.token {
				t.Errorf("%s: expected %#v, got %#v", name, tc.token, token)
			}
			token = tokenizer.NextToken()
			if token.Type != TokenEOF {
				t.Errorf("%s: expected EOF at the end, got %#v", name, token)
			}
		}
	})
	t.Run("complex cases", func(t *testing.T) {
		for name, tc := range complexCases {
			tokenizer := NewTokenizer(tc.source)
			for _, expected := range tc.tokens {
				token := tokenizer.NextToken()
				if token != expected {
					t.Errorf("%s: expected %#v, got %#v", name, expected, token)
				}
			}
			token := tokenizer.NextToken()
			if token.Type != TokenEOF {
				t.Errorf("%s: expected EOF at the end, got %#v", name, token)
			}
		}
	})
}
