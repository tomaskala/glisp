package test

import (
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
	"tomaskala.com/glisp/internal/tokenizer"
)

// Helper function to tokenize entire input and return all tokens.
func tokenizeAll(source string) []tokenizer.Token {
	tok := tokenizer.NewTokenizer(source)
	var tokens []tokenizer.Token
	for {
		token := tok.NextToken()
		tokens = append(tokens, token)
		if token.Type == tokenizer.TokenEOF {
			break
		}
	}
	return tokens
}

// Basic token tests.
var basicTokenTests = map[string]struct {
	source string
	token  tokenizer.Token
}{
	// Parentheses
	"left paren":  {"(", tokenizer.Token{Type: tokenizer.TokenLeftParen, Line: 1, Val: "("}},
	"right paren": {")", tokenizer.Token{Type: tokenizer.TokenRightParen, Line: 1, Val: ")"}},

	// Special characters
	"dot":       {".", tokenizer.Token{Type: tokenizer.TokenDot, Line: 1, Val: "."}},
	"quote":     {"'", tokenizer.Token{Type: tokenizer.TokenQuote, Line: 1, Val: "'"}},
	"backquote": {"`", tokenizer.Token{Type: tokenizer.TokenBackquote, Line: 1, Val: "`"}},
	"comma":     {",", tokenizer.Token{Type: tokenizer.TokenComma, Line: 1, Val: ","}},

	// Basic atoms
	"simple atom":        {"hello", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "hello"}},
	"atom with dash":     {"hello-world", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "hello-world"}},
	"atom with question": {"null?", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "null?"}},
	"atom with numbers":  {"var123", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "var123"}},
	"single char atom":   {"x", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "x"}},

	// Mathematical operators
	"plus":     {"+", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "+"}},
	"minus":    {"-", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "-"}},
	"multiply": {"*", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "*"}},
	"divide":   {"/", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "/"}},

	// Basic numbers
	"zero":           {"0", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0"}},
	"positive int":   {"42", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "42"}},
	"negative int":   {"-123", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "-123"}},
	"explicit pos":   {"+456", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "+456"}},
	"simple float":   {"3.14", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "3.14"}},
	"negative float": {"-2.718", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "-2.718"}},
}

// Number format tests - comprehensive coverage of all number formats.
var numberFormatTests = map[string]struct {
	source string
	token  tokenizer.Token
}{
	// Decimal integers
	"single digit":     {"7", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "7"}},
	"multi digit":      {"1234567890", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "1234567890"}},
	"with underscores": {"1_000_000", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "1_000_000"}},
	"leading zeros":    {"00123", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "00123"}},

	// Hexadecimal
	"hex lowercase":    {"0xff", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0xff"}},
	"hex uppercase":    {"0XFF", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0XFF"}},
	"hex mixed case":   {"0xDeAdBeEf", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0xDeAdBeEf"}},
	"hex with under":   {"0xFF_00_AA", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0xFF_00_AA"}},
	"hex signed pos":   {"+0x123", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "+0x123"}},
	"hex signed neg":   {"-0xABC", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "-0xABC"}},
	"hex single digit": {"0x1", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0x1"}},
	"hex all digits": {
		"0x0123456789abcdefABCDEF",
		tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0x0123456789abcdefABCDEF"},
	},

	// Octal
	"octal lowercase":  {"0o755", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0o755"}},
	"octal uppercase":  {"0O644", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0O644"}},
	"octal with under": {"0o777_444", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0o777_444"}},
	"octal signed pos": {"+0o123", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "+0o123"}},
	"octal signed neg": {"-0o567", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "-0o567"}},
	"octal single":     {"0o1", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0o1"}},
	"octal all digits": {"0o01234567", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0o01234567"}},

	// Binary
	"binary lowercase":  {"0b101", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0b101"}},
	"binary uppercase":  {"0B110", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0B110"}},
	"binary with under": {"0b1010_1111", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0b1010_1111"}},
	"binary signed pos": {"+0b11", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "+0b11"}},
	"binary signed neg": {"-0b01", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "-0b01"}},
	"binary single":     {"0b1", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0b1"}},
	"binary long": {
		"0b11110000111100001111",
		tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0b11110000111100001111"},
	},

	// Floating point decimal
	"float simple":      {"1.5", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "1.5"}},
	"float zero":        {"0.0", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0.0"}},
	"float no leading":  {".5", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: ".5"}},
	"float no trailing": {"5.", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "5."}},
	"float with under":  {"3.141_592", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "3.141_592"}},
	"float signed pos":  {"+2.5", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "+2.5"}},
	"float signed neg":  {"-9.75", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "-9.75"}},

	// Scientific notation (decimal)
	"sci simple":       {"1e5", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "1e5"}},
	"sci uppercase":    {"1E5", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "1E5"}},
	"sci negative exp": {"1e-5", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "1e-5"}},
	"sci positive exp": {"1e+5", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "1e+5"}},
	"sci float base":   {"3.14e2", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "3.14e2"}},
	"sci with under":   {"1_000e3", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "1_000e3"}},
	"sci signed":       {"-2.5E-10", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "-2.5E-10"}},
	"sci zero exp":     {"5e0", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "5e0"}},

	// Hexadecimal floating point
	"hex float simple":   {"0x1p0", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0x1p0"}},
	"hex float with dot": {"0x1.5p2", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0x1.5p2"}},
	"hex float neg exp":  {"0x1p-2", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0x1p-2"}},
	"hex float pos exp":  {"0x1p+3", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0x1p+3"}},
	"hex float upper":    {"0X1.FP4", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0X1.FP4"}},
	"hex float signed":   {"-0x1.ABCp-5", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "-0x1.ABCp-5"}},

	// Edge cases
	"negative zero": {"-0", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "-0"}},
	"positive zero": {"+0", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "+0"}},
	"zero float":    {"0.00000", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0.00000"}},
	"large number": {
		"999999999999999999",
		tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "999999999999999999"},
	},
	"tiny float": {"0.000000001", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "0.000000001"}},
}

// Atom edge case tests.
var atomEdgeCaseTests = map[string]struct {
	source string
	token  tokenizer.Token
}{
	// Special characters in atoms
	"at symbol":    {"@", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "@"}},
	"hash":         {"#", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "#"}},
	"percent":      {"%", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "%"}},
	"caret":        {"^", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "^"}},
	"ampersand":    {"&", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "&"}},
	"exclamation":  {"!", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "!"}},
	"tilde":        {"~", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "~"}},
	"backslash":    {"\\", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "\\"}},
	"pipe":         {"|", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "|"}},
	"colon":        {":", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: ":"}},
	"less than":    {"<", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "<"}},
	"greater than": {">", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: ">"}},
	"equals":       {"=", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "="}},
	"dollar":       {"$", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "$"}},
	"underscore":   {"_", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "_"}},

	// Complex atoms
	"mixed special": {
		"hello-world@test",
		tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "hello-world@test"},
	},
	"operator like":   {"<=", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "<="}},
	"operator like 2": {">=", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: ">="}},
	"operator like 3": {"<>", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "<>"}},
	"lisp style":      {"car-cdr-cons", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "car-cdr-cons"}},
	"with numbers":    {"test123abc", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "test123abc"}},
	"ends with num":   {"var2", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "var2"}},
	"boolean true":    {"#t", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "#t"}},
	"boolean false":   {"#f", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "#f"}},
	"keyword style":   {":keyword", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: ":keyword"}},
	"namespace style": {"ns/func", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "ns/func"}},

	// Unicode atoms
	"unicode lambda": {"λ", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "λ"}},
	"unicode mixed":  {"test-λ-func", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "test-λ-func"}},
	"unicode emoji":  {"🚀", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "🚀"}},
	"japanese":       {"こんにちは", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "こんにちは"}},
	"greek":          {"αβγδε", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "αβγδε"}},

	// Very long atoms
	"long atom": {
		strings.Repeat("a", 100),
		tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: strings.Repeat("a", 100)},
	},
	"long mixed": {
		"very-long-atom-with-many-dashes-and-123-numbers-and-@-symbols",
		tokenizer.Token{
			Type: tokenizer.TokenAtom,
			Line: 1,
			Val:  "very-long-atom-with-many-dashes-and-123-numbers-and-@-symbols",
		},
	},
}

// Multi-token sequence tests.
var sequenceTests = map[string]struct {
	source string
	tokens []tokenizer.Token
}{
	"simple list": {
		source: "(+ 1 2)",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenLeftParen, Line: 1, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "+"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "1"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "2"},
			{Type: tokenizer.TokenRightParen, Line: 1, Val: ")"},
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"nested list": {
		source: "(f (g x) y)",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenLeftParen, Line: 1, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "f"},
			{Type: tokenizer.TokenLeftParen, Line: 1, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "g"},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "x"},
			{Type: tokenizer.TokenRightParen, Line: 1, Val: ")"},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "y"},
			{Type: tokenizer.TokenRightParen, Line: 1, Val: ")"},
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"quoted expression": {
		source: "'(a b c)",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenQuote, Line: 1, Val: "'"},
			{Type: tokenizer.TokenLeftParen, Line: 1, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "a"},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "b"},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "c"},
			{Type: tokenizer.TokenRightParen, Line: 1, Val: ")"},
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"mixed numbers": {
		source: "42 3.14 0xFF -123 +456",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenNumber, Line: 1, Val: "42"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "3.14"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "0xFF"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "-123"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "+456"},
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"dots and quotes": {
		source: ". ' . '",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenDot, Line: 1, Val: "."},
			{Type: tokenizer.TokenQuote, Line: 1, Val: "'"},
			{Type: tokenizer.TokenDot, Line: 1, Val: "."},
			{Type: tokenizer.TokenQuote, Line: 1, Val: "'"},
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"backquoted expression": {
		source: "`(a ,b (c ,d) (e f))",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenBackquote, Line: 1, Val: "`"},
			{Type: tokenizer.TokenLeftParen, Line: 1, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "a"},
			{Type: tokenizer.TokenComma, Line: 1, Val: ","},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "b"},
			{Type: tokenizer.TokenLeftParen, Line: 1, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "c"},
			{Type: tokenizer.TokenComma, Line: 1, Val: ","},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "d"},
			{Type: tokenizer.TokenRightParen, Line: 1, Val: ")"},
			{Type: tokenizer.TokenLeftParen, Line: 1, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "e"},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "f"},
			{Type: tokenizer.TokenRightParen, Line: 1, Val: ")"},
			{Type: tokenizer.TokenRightParen, Line: 1, Val: ")"},
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
}

// Whitespace and formatting tests.
var whitespaceTests = map[string]struct {
	source string
	tokens []tokenizer.Token
}{
	"spaces": {
		source: "  a   b  ",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 1, Val: "a"},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "b"},
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"tabs": {
		source: "\ta\t\tb\t",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 1, Val: "a"},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "b"},
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"newlines": {
		source: "a\nb\n",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 1, Val: "a"},
			{Type: tokenizer.TokenAtom, Line: 2, Val: "b"},
			{Type: tokenizer.TokenEOF, Line: 3, Val: "EOF"},
		},
	},
	"mixed whitespace": {
		source: " \t\n a \t\n b \t\n ",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 2, Val: "a"},
			{Type: tokenizer.TokenAtom, Line: 3, Val: "b"},
			{Type: tokenizer.TokenEOF, Line: 4, Val: "EOF"},
		},
	},
	"carriage returns": {
		source: "a\r\nb\r\n",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 1, Val: "a"},
			{Type: tokenizer.TokenAtom, Line: 2, Val: "b"},
			{Type: tokenizer.TokenEOF, Line: 3, Val: "EOF"},
		},
	},
}

// Comment tests.
var commentTests = map[string]struct {
	source string
	tokens []tokenizer.Token
}{
	"line comment": {
		source: "; this is a comment\na",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 2, Val: "a"},
			{Type: tokenizer.TokenEOF, Line: 2, Val: "EOF"},
		},
	},
	"comment at end": {
		source: "a ; comment",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 1, Val: "a"},
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"multiple comments": {
		source: "; first\na ; second\n; third\nb",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 2, Val: "a"},
			{Type: tokenizer.TokenAtom, Line: 4, Val: "b"},
			{Type: tokenizer.TokenEOF, Line: 4, Val: "EOF"},
		},
	},
	"comment with specials": {
		source: "; comment with ()'\". special chars\na",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 2, Val: "a"},
			{Type: tokenizer.TokenEOF, Line: 2, Val: "EOF"},
		},
	},
	"empty comment": {
		source: ";\na",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 2, Val: "a"},
			{Type: tokenizer.TokenEOF, Line: 2, Val: "EOF"},
		},
	},
	"comment only": {
		source: "; just a comment",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"comment no newline": {
		source: "a ; comment at end",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 1, Val: "a"},
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
}

// Line number tracking tests.
var lineNumberTests = map[string]struct {
	source string
	tokens []tokenizer.Token
}{
	"multiline positions": {
		source: "a\n  b\n    c",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 1, Val: "a"},
			{Type: tokenizer.TokenAtom, Line: 2, Val: "b"},
			{Type: tokenizer.TokenAtom, Line: 3, Val: "c"},
			{Type: tokenizer.TokenEOF, Line: 3, Val: "EOF"},
		},
	},
	"complex multiline": {
		source: "(define factorial\n  (lambda (n)\n    (if (= n 0)\n        1\n        (* n (factorial (- n 1))))))",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenLeftParen, Line: 1, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "define"},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "factorial"},
			{Type: tokenizer.TokenLeftParen, Line: 2, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 2, Val: "lambda"},
			{Type: tokenizer.TokenLeftParen, Line: 2, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 2, Val: "n"},
			{Type: tokenizer.TokenRightParen, Line: 2, Val: ")"},
			{Type: tokenizer.TokenLeftParen, Line: 3, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 3, Val: "if"},
			{Type: tokenizer.TokenLeftParen, Line: 3, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 3, Val: "="},
			{Type: tokenizer.TokenAtom, Line: 3, Val: "n"},
			{Type: tokenizer.TokenNumber, Line: 3, Val: "0"},
			{Type: tokenizer.TokenRightParen, Line: 3, Val: ")"},
			{Type: tokenizer.TokenNumber, Line: 4, Val: "1"},
			{Type: tokenizer.TokenLeftParen, Line: 5, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 5, Val: "*"},
			{Type: tokenizer.TokenAtom, Line: 5, Val: "n"},
			{Type: tokenizer.TokenLeftParen, Line: 5, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 5, Val: "factorial"},
			{Type: tokenizer.TokenLeftParen, Line: 5, Val: "("},
			{Type: tokenizer.TokenAtom, Line: 5, Val: "-"},
			{Type: tokenizer.TokenAtom, Line: 5, Val: "n"},
			{Type: tokenizer.TokenNumber, Line: 5, Val: "1"},
			{Type: tokenizer.TokenRightParen, Line: 5, Val: ")"},
			{Type: tokenizer.TokenRightParen, Line: 5, Val: ")"},
			{Type: tokenizer.TokenRightParen, Line: 5, Val: ")"},
			{Type: tokenizer.TokenRightParen, Line: 5, Val: ")"},
			{Type: tokenizer.TokenRightParen, Line: 5, Val: ")"},
			{Type: tokenizer.TokenRightParen, Line: 5, Val: ")"},
			{Type: tokenizer.TokenEOF, Line: 5, Val: "EOF"},
		},
	},
}

// Edge cases and boundary conditions.
var edgeCaseTests = map[string]struct {
	source string
	tokens []tokenizer.Token
}{
	"empty input": {
		source: "",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"only whitespace": {
		source: "   \n\t  \n ",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenEOF, Line: 3, Val: "EOF"},
		},
	},
	"plus minus edge": {
		source: "+ - +1 -2 +abc -def",
		tokens: []tokenizer.Token{
			{Type: tokenizer.TokenAtom, Line: 1, Val: "+"},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "-"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "+1"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "-2"},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "+abc"},
			{Type: tokenizer.TokenAtom, Line: 1, Val: "-def"},
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"zero prefixes": {
		source: "0 0x 0o 0b 0xff 0o755 0b101",
		tokens: []tokenizer.Token{
			// The tokenizer will miss some invalid numbers which will be later rejected by the parser.
			{Type: tokenizer.TokenNumber, Line: 1, Val: "0"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "0x"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "0o"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "0b"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "0xff"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "0o755"},
			{Type: tokenizer.TokenNumber, Line: 1, Val: "0b101"},
			{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
		},
	},
}

// Error condition tests.
var errorTests = map[string]struct {
	source      string
	expectError bool
	errorMsg    string
}{
	"unrecognized char": {
		source:      "abc\ufffd",
		expectError: true,
		errorMsg:    "unrecognized character",
	},
	"invalid hex1": {
		source:      "0xGHI",
		expectError: true,
		errorMsg:    "Atom cannot start with a number",
	},
	"invalid hex2": {
		source:      "+0xGHI",
		expectError: true,
		errorMsg:    "Atom cannot start with a number",
	},
	"invalid hex3": {
		source:      "-0xGHI",
		expectError: true,
		errorMsg:    "Atom cannot start with a number",
	},
	"invalid oct1": {
		source:      "0oABC",
		expectError: true,
		errorMsg:    "Atom cannot start with a number",
	},
	"invalid oct2": {
		source:      "+0oABC",
		expectError: true,
		errorMsg:    "Atom cannot start with a number",
	},
	"invalid oct3": {
		source:      "-0oABC",
		expectError: true,
		errorMsg:    "Atom cannot start with a number",
	},
	"invalid token1": {
		source:      "123abc",
		expectError: true,
		errorMsg:    "Atom cannot start with a number",
	},
	"invalid token2": {
		source:      "+123abc",
		expectError: true,
		errorMsg:    "Atom cannot start with a number",
	},
	"invalid token3": {
		source:      "-123abc",
		expectError: true,
		errorMsg:    "Atom cannot start with a number",
	},
	"numbers next to each other": {
		// () gets parsed correctly, +1- fails because it would become an atom starting with a number.
		source:      "()+1-2'abc",
		expectError: true,
		errorMsg:    "Atom cannot start with a number",
	},
	"isolated control char": {
		source:      "abc\x00def",
		expectError: true,
		errorMsg:    "unrecognized character",
	},
}

// Main test functions.
func TestBasicTokens(t *testing.T) {
	for name, tc := range basicTokenTests {
		t.Run(name, func(t *testing.T) {
			tok := tokenizer.NewTokenizer(tc.source)
			token := tok.NextToken()
			if !cmp.Equal(token, tc.token) {
				t.Errorf("expected %#v, got %#v", tc.token, token)
			}
			// Check that next token is EOF
			eofToken := tok.NextToken()
			if eofToken.Type != tokenizer.TokenEOF {
				t.Errorf("expected EOF after token, got %#v", eofToken)
			}
		})
	}
}

func TestNumberFormats(t *testing.T) {
	for name, tc := range numberFormatTests {
		t.Run(name, func(t *testing.T) {
			tok := tokenizer.NewTokenizer(tc.source)
			token := tok.NextToken()
			if !cmp.Equal(token, tc.token) {
				t.Errorf("expected %#v, got %#v", tc.token, token)
			}
		})
	}
}

func TestAtomEdgeCases(t *testing.T) {
	for name, tc := range atomEdgeCaseTests {
		t.Run(name, func(t *testing.T) {
			tok := tokenizer.NewTokenizer(tc.source)
			token := tok.NextToken()
			if !cmp.Equal(token, tc.token) {
				t.Errorf("expected %#v, got %#v", tc.token, token)
			}
		})
	}
}

func TestSequences(t *testing.T) {
	for name, tc := range sequenceTests {
		t.Run(name, func(t *testing.T) {
			tokens := tokenizeAll(tc.source)
			if !cmp.Equal(tokens, tc.tokens) {
				t.Errorf("tokenizer.Token mismatch for %s", name)
				t.Errorf("Expected: %#v", tc.tokens)
				t.Errorf("Got:      %#v", tokens)
				for i := 0; i < len(tokens) && i < len(tc.tokens); i++ {
					if !cmp.Equal(tokens[i], tc.tokens[i]) {
						t.Errorf("First difference at index %d: expected %#v, got %#v", i, tc.tokens[i], tokens[i])
						break
					}
				}
			}
		})
	}
}

func TestWhitespace(t *testing.T) {
	for name, tc := range whitespaceTests {
		t.Run(name, func(t *testing.T) {
			tokens := tokenizeAll(tc.source)
			if !cmp.Equal(tokens, tc.tokens) {
				t.Errorf("tokenizer.Token mismatch for %s", name)
				t.Errorf("Expected: %#v", tc.tokens)
				t.Errorf("Got:      %#v", tokens)
			}
		})
	}
}

func TestComments(t *testing.T) {
	for name, tc := range commentTests {
		t.Run(name, func(t *testing.T) {
			tokens := tokenizeAll(tc.source)
			if !cmp.Equal(tokens, tc.tokens) {
				t.Errorf("tokenizer.Token mismatch for %s", name)
				t.Errorf("Expected: %#v", tc.tokens)
				t.Errorf("Got:      %#v", tokens)
			}
		})
	}
}

func TestLineNumbers(t *testing.T) {
	for name, tc := range lineNumberTests {
		t.Run(name, func(t *testing.T) {
			tokens := tokenizeAll(tc.source)
			if !cmp.Equal(tokens, tc.tokens) {
				t.Errorf("tokenizer.Token mismatch for %s", name)
				t.Errorf("Expected: %#v", tc.tokens)
				t.Errorf("Got:      %#v", tokens)
			}
		})
	}
}

func TestTokenizerEdgeCases(t *testing.T) {
	for name, tc := range edgeCaseTests {
		t.Run(name, func(t *testing.T) {
			tokens := tokenizeAll(tc.source)
			if !cmp.Equal(tokens, tc.tokens) {
				t.Errorf("tokenizer.Token mismatch for %s", name)
				t.Errorf("Expected: %#v", tc.tokens)
				t.Errorf("Got:      %#v", tokens)
			}
		})
	}
}

func TestErrorConditions(t *testing.T) {
	for name, tc := range errorTests {
		t.Run(name, func(t *testing.T) {
			tok := tokenizer.NewTokenizer(tc.source)
			token := tok.NextToken()

			if tc.expectError {
				for token.Type != tokenizer.TokenErr && token.Type != tokenizer.TokenEOF {
					token = tok.NextToken()
				}
				if token.Type != tokenizer.TokenErr {
					t.Errorf("expected error token, got %#v", token)
				} else if !strings.Contains(token.Val, tc.errorMsg) {
					t.Errorf("expected error message to contain %q, got %q", tc.errorMsg, token.Val)
				}
			} else if token.Type == tokenizer.TokenErr {
				t.Errorf("unexpected error: %s", token.Val)
			}
		})
	}
}

// Performance and stress tests.
func TestLargeInput(t *testing.T) {
	// Test with a large input to ensure no performance issues
	largeAtom := strings.Repeat("a", 10000)
	tok := tokenizer.NewTokenizer(largeAtom)
	token := tok.NextToken()

	if token.Type != tokenizer.TokenAtom || token.Val != largeAtom {
		t.Errorf("failed to tokenize large atom correctly")
	}
}

func TestManyTokens(t *testing.T) {
	// Test with many small tokens
	source := strings.Repeat("a ", 1000)
	tokens := tokenizeAll(source)

	if len(tokens) != 1001 { // 1000 atoms + 1 EOF
		t.Errorf("expected 1001 tokens, got %d", len(tokens))
	}

	for i := range 1000 {
		if tokens[i].Type != tokenizer.TokenAtom || tokens[i].Val != "a" {
			t.Errorf("token %d: expected atom 'a', got %#v", i, tokens[i])
			break
		}
	}
}

func TestDeepNesting(t *testing.T) {
	// Test with deeply nested parentheses
	source := strings.Repeat("(", 1000) + "x" + strings.Repeat(")", 1000)
	tokens := tokenizeAll(source)

	expected := 1000 + 1 + 1000 + 1 // left parens + atom + right parens + EOF
	if len(tokens) != expected {
		t.Errorf("expected %d tokens, got %d", expected, len(tokens))
	}
}

func TestSignedNumbersVsAtoms(t *testing.T) {
	// Test the distinction between signed numbers and atoms with +/- prefix
	tests := []struct {
		source   string
		expected tokenizer.Token
	}{
		{"+", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "+"}},
		{"-", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "-"}},
		{"+123", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "+123"}},
		{"-123", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "-123"}},
		{"+abc", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "+abc"}},
		{"-abc", tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "-abc"}},
		{"+0xFF", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "+0xFF"}},
		{"-0xFF", tokenizer.Token{Type: tokenizer.TokenNumber, Line: 1, Val: "-0xFF"}},
	}

	for _, tc := range tests {
		t.Run(tc.source, func(t *testing.T) {
			tok := tokenizer.NewTokenizer(tc.source)
			token := tok.NextToken()
			if !cmp.Equal(token, tc.expected) {
				t.Errorf("for source %q: expected %#v, got %#v", tc.source, tc.expected, token)
			}
		})
	}
}

func TestCommentEdgeCases(t *testing.T) {
	// Test comment handling edge cases
	tests := []struct {
		name   string
		source string
		tokens []tokenizer.Token
	}{
		{
			name:   "comment with no newline at EOF",
			source: "a; comment",
			tokens: []tokenizer.Token{
				{Type: tokenizer.TokenAtom, Line: 1, Val: "a"},
				{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"},
			},
		},
		{
			name:   "semicolon in atom",
			source: "test;atom",
			tokens: []tokenizer.Token{
				{Type: tokenizer.TokenAtom, Line: 1, Val: "test"},
				{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"}, // ';' starts comment, rest is ignored
			},
		},
		{
			name:   "multiple semicolons in comment",
			source: "; ;;; comment ;;;\na",
			tokens: []tokenizer.Token{
				{Type: tokenizer.TokenAtom, Line: 2, Val: "a"},
				{Type: tokenizer.TokenEOF, Line: 2, Val: "EOF"},
			},
		},
		{
			name:   "semicolon alone",
			source: ";",
			tokens: []tokenizer.Token{{Type: tokenizer.TokenEOF, Line: 1, Val: "EOF"}},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			tokens := tokenizeAll(tc.source)
			if !cmp.Equal(tokens, tc.tokens) {
				t.Errorf("Expected: %#v", tc.tokens)
				t.Errorf("Got:      %#v", tokens)
			}
		})
	}
}

func TestUnicodeHandling(t *testing.T) {
	// Test various unicode scenarios
	tests := []struct {
		name   string
		source string
		token  tokenizer.Token
	}{
		{
			name:   "basic unicode",
			source: "λ",
			token:  tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "λ"},
		},
		{
			name:   "emoji",
			source: "🚀",
			token:  tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "🚀"},
		},
		{
			name:   "mixed ascii unicode",
			source: "test-λ-func",
			token:  tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "test-λ-func"},
		},
		{
			name:   "japanese",
			source: "こんにちは",
			token:  tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "こんにちは"},
		},
		{
			name:   "arabic",
			source: "مرحبا",
			token:  tokenizer.Token{Type: tokenizer.TokenAtom, Line: 1, Val: "مرحبا"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			tok := tokenizer.NewTokenizer(tc.source)
			token := tok.NextToken()
			if !cmp.Equal(token, tc.token) {
				t.Errorf("expected %#v, got %#v", tc.token, token)
			}
		})
	}
}
