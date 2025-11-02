package tokenizer

import (
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

// Helper function to tokenize entire input and return all tokens.
func tokenizeAll(source string) []Token {
	tok := NewTokenizer(source)
	var tokens []Token
	for {
		token := tok.NextToken()
		tokens = append(tokens, token)
		if token.Type == TokenEOF {
			break
		}
	}
	return tokens
}

// Basic token tests.
var basicTokenTests = map[string]struct {
	source string
	token  Token
}{
	// Parentheses
	"left paren":  {"(", Token{Type: TokenLeftParen, Line: 1, Val: "("}},
	"right paren": {")", Token{Type: TokenRightParen, Line: 1, Val: ")"}},

	// Special characters
	"dot":       {".", Token{Type: TokenDot, Line: 1, Val: "."}},
	"quote":     {"'", Token{Type: TokenQuote, Line: 1, Val: "'"}},
	"backquote": {"`", Token{Type: TokenBackquote, Line: 1, Val: "`"}},
	"comma":     {",", Token{Type: TokenComma, Line: 1, Val: ","}},
	"comma-at":  {",@", Token{Type: TokenCommaAt, Line: 1, Val: ",@"}},

	// Basic atoms
	"simple atom":        {"hello", Token{Type: TokenAtom, Line: 1, Val: "hello"}},
	"atom with dash":     {"hello-world", Token{Type: TokenAtom, Line: 1, Val: "hello-world"}},
	"atom with question": {"null?", Token{Type: TokenAtom, Line: 1, Val: "null?"}},
	"atom with numbers":  {"var123", Token{Type: TokenAtom, Line: 1, Val: "var123"}},
	"single char atom":   {"x", Token{Type: TokenAtom, Line: 1, Val: "x"}},

	// Mathematical operators
	"plus":     {"+", Token{Type: TokenAtom, Line: 1, Val: "+"}},
	"minus":    {"-", Token{Type: TokenAtom, Line: 1, Val: "-"}},
	"multiply": {"*", Token{Type: TokenAtom, Line: 1, Val: "*"}},
	"divide":   {"/", Token{Type: TokenAtom, Line: 1, Val: "/"}},

	// Basic numbers
	"zero":           {"0", Token{Type: TokenNumber, Line: 1, Val: "0"}},
	"positive int":   {"42", Token{Type: TokenNumber, Line: 1, Val: "42"}},
	"negative int":   {"-123", Token{Type: TokenNumber, Line: 1, Val: "-123"}},
	"explicit pos":   {"+456", Token{Type: TokenNumber, Line: 1, Val: "+456"}},
	"simple float":   {"3.14", Token{Type: TokenNumber, Line: 1, Val: "3.14"}},
	"negative float": {"-2.718", Token{Type: TokenNumber, Line: 1, Val: "-2.718"}},
}

// Number format tests - comprehensive coverage of all number formats.
var numberFormatTests = map[string]struct {
	source string
	token  Token
}{
	// Decimal integers
	"single digit":     {"7", Token{Type: TokenNumber, Line: 1, Val: "7"}},
	"multi digit":      {"1234567890", Token{Type: TokenNumber, Line: 1, Val: "1234567890"}},
	"with underscores": {"1_000_000", Token{Type: TokenNumber, Line: 1, Val: "1_000_000"}},
	"leading zeros":    {"00123", Token{Type: TokenNumber, Line: 1, Val: "00123"}},

	// Hexadecimal
	"hex lowercase":    {"0xff", Token{Type: TokenNumber, Line: 1, Val: "0xff"}},
	"hex uppercase":    {"0XFF", Token{Type: TokenNumber, Line: 1, Val: "0XFF"}},
	"hex mixed case":   {"0xDeAdBeEf", Token{Type: TokenNumber, Line: 1, Val: "0xDeAdBeEf"}},
	"hex with under":   {"0xFF_00_AA", Token{Type: TokenNumber, Line: 1, Val: "0xFF_00_AA"}},
	"hex signed pos":   {"+0x123", Token{Type: TokenNumber, Line: 1, Val: "+0x123"}},
	"hex signed neg":   {"-0xABC", Token{Type: TokenNumber, Line: 1, Val: "-0xABC"}},
	"hex single digit": {"0x1", Token{Type: TokenNumber, Line: 1, Val: "0x1"}},
	"hex all digits": {
		"0x0123456789abcdefABCDEF",
		Token{Type: TokenNumber, Line: 1, Val: "0x0123456789abcdefABCDEF"},
	},

	// Octal
	"octal lowercase":  {"0o755", Token{Type: TokenNumber, Line: 1, Val: "0o755"}},
	"octal uppercase":  {"0O644", Token{Type: TokenNumber, Line: 1, Val: "0O644"}},
	"octal with under": {"0o777_444", Token{Type: TokenNumber, Line: 1, Val: "0o777_444"}},
	"octal signed pos": {"+0o123", Token{Type: TokenNumber, Line: 1, Val: "+0o123"}},
	"octal signed neg": {"-0o567", Token{Type: TokenNumber, Line: 1, Val: "-0o567"}},
	"octal single":     {"0o1", Token{Type: TokenNumber, Line: 1, Val: "0o1"}},
	"octal all digits": {"0o01234567", Token{Type: TokenNumber, Line: 1, Val: "0o01234567"}},

	// Binary
	"binary lowercase":  {"0b101", Token{Type: TokenNumber, Line: 1, Val: "0b101"}},
	"binary uppercase":  {"0B110", Token{Type: TokenNumber, Line: 1, Val: "0B110"}},
	"binary with under": {"0b1010_1111", Token{Type: TokenNumber, Line: 1, Val: "0b1010_1111"}},
	"binary signed pos": {"+0b11", Token{Type: TokenNumber, Line: 1, Val: "+0b11"}},
	"binary signed neg": {"-0b01", Token{Type: TokenNumber, Line: 1, Val: "-0b01"}},
	"binary single":     {"0b1", Token{Type: TokenNumber, Line: 1, Val: "0b1"}},
	"binary long": {
		"0b11110000111100001111",
		Token{Type: TokenNumber, Line: 1, Val: "0b11110000111100001111"},
	},

	// Floating point decimal
	"float simple":      {"1.5", Token{Type: TokenNumber, Line: 1, Val: "1.5"}},
	"float zero":        {"0.0", Token{Type: TokenNumber, Line: 1, Val: "0.0"}},
	"float no leading":  {".5", Token{Type: TokenNumber, Line: 1, Val: ".5"}},
	"float no trailing": {"5.", Token{Type: TokenNumber, Line: 1, Val: "5."}},
	"float with under":  {"3.141_592", Token{Type: TokenNumber, Line: 1, Val: "3.141_592"}},
	"float signed pos":  {"+2.5", Token{Type: TokenNumber, Line: 1, Val: "+2.5"}},
	"float signed neg":  {"-9.75", Token{Type: TokenNumber, Line: 1, Val: "-9.75"}},

	// Scientific notation (decimal)
	"sci simple":       {"1e5", Token{Type: TokenNumber, Line: 1, Val: "1e5"}},
	"sci uppercase":    {"1E5", Token{Type: TokenNumber, Line: 1, Val: "1E5"}},
	"sci negative exp": {"1e-5", Token{Type: TokenNumber, Line: 1, Val: "1e-5"}},
	"sci positive exp": {"1e+5", Token{Type: TokenNumber, Line: 1, Val: "1e+5"}},
	"sci float base":   {"3.14e2", Token{Type: TokenNumber, Line: 1, Val: "3.14e2"}},
	"sci with under":   {"1_000e3", Token{Type: TokenNumber, Line: 1, Val: "1_000e3"}},
	"sci signed":       {"-2.5E-10", Token{Type: TokenNumber, Line: 1, Val: "-2.5E-10"}},
	"sci zero exp":     {"5e0", Token{Type: TokenNumber, Line: 1, Val: "5e0"}},

	// Hexadecimal floating point
	"hex float simple":   {"0x1p0", Token{Type: TokenNumber, Line: 1, Val: "0x1p0"}},
	"hex float with dot": {"0x1.5p2", Token{Type: TokenNumber, Line: 1, Val: "0x1.5p2"}},
	"hex float neg exp":  {"0x1p-2", Token{Type: TokenNumber, Line: 1, Val: "0x1p-2"}},
	"hex float pos exp":  {"0x1p+3", Token{Type: TokenNumber, Line: 1, Val: "0x1p+3"}},
	"hex float upper":    {"0X1.FP4", Token{Type: TokenNumber, Line: 1, Val: "0X1.FP4"}},
	"hex float signed":   {"-0x1.ABCp-5", Token{Type: TokenNumber, Line: 1, Val: "-0x1.ABCp-5"}},

	// Edge cases
	"negative zero": {"-0", Token{Type: TokenNumber, Line: 1, Val: "-0"}},
	"positive zero": {"+0", Token{Type: TokenNumber, Line: 1, Val: "+0"}},
	"zero float":    {"0.00000", Token{Type: TokenNumber, Line: 1, Val: "0.00000"}},
	"large number": {
		"999999999999999999",
		Token{Type: TokenNumber, Line: 1, Val: "999999999999999999"},
	},
	"tiny float": {"0.000000001", Token{Type: TokenNumber, Line: 1, Val: "0.000000001"}},

	// Reserved values
	"nan1":       {"NaN", Token{Type: TokenNumber, Line: 1, Val: "NaN"}},
	"nan2":       {"nan", Token{Type: TokenNumber, Line: 1, Val: "nan"}},
	"nan3":       {"NAN", Token{Type: TokenNumber, Line: 1, Val: "NAN"}},
	"inf1":       {"inf", Token{Type: TokenNumber, Line: 1, Val: "inf"}},
	"inf2":       {"INF", Token{Type: TokenNumber, Line: 1, Val: "INF"}},
	"inf3":       {"iNf", Token{Type: TokenNumber, Line: 1, Val: "iNf"}},
	"infinity1":  {"infinity", Token{Type: TokenNumber, Line: 1, Val: "infinity"}},
	"infinity2":  {"INFINITY", Token{Type: TokenNumber, Line: 1, Val: "INFINITY"}},
	"infinity3":  {"iNfInItY", Token{Type: TokenNumber, Line: 1, Val: "iNfInItY"}},
	"-inf1":      {"-inf", Token{Type: TokenNumber, Line: 1, Val: "-inf"}},
	"-inf2":      {"-INF", Token{Type: TokenNumber, Line: 1, Val: "-INF"}},
	"-inf3":      {"-iNf", Token{Type: TokenNumber, Line: 1, Val: "-iNf"}},
	"-infinity1": {"-infinity", Token{Type: TokenNumber, Line: 1, Val: "-infinity"}},
	"-infinity2": {"-INFINITY", Token{Type: TokenNumber, Line: 1, Val: "-INFINITY"}},
	"-infinity3": {"-iNfInItY", Token{Type: TokenNumber, Line: 1, Val: "-iNfInItY"}},
}

// Atom edge case tests.
var atomEdgeCaseTests = map[string]struct {
	source string
	token  Token
}{
	// Special characters in atoms
	"hash":         {"#", Token{Type: TokenAtom, Line: 1, Val: "#"}},
	"percent":      {"%", Token{Type: TokenAtom, Line: 1, Val: "%"}},
	"caret":        {"^", Token{Type: TokenAtom, Line: 1, Val: "^"}},
	"ampersand":    {"&", Token{Type: TokenAtom, Line: 1, Val: "&"}},
	"exclamation":  {"!", Token{Type: TokenAtom, Line: 1, Val: "!"}},
	"tilde":        {"~", Token{Type: TokenAtom, Line: 1, Val: "~"}},
	"backslash":    {"\\", Token{Type: TokenAtom, Line: 1, Val: "\\"}},
	"pipe":         {"|", Token{Type: TokenAtom, Line: 1, Val: "|"}},
	"colon":        {":", Token{Type: TokenAtom, Line: 1, Val: ":"}},
	"less than":    {"<", Token{Type: TokenAtom, Line: 1, Val: "<"}},
	"greater than": {">", Token{Type: TokenAtom, Line: 1, Val: ">"}},
	"equals":       {"=", Token{Type: TokenAtom, Line: 1, Val: "="}},
	"dollar":       {"$", Token{Type: TokenAtom, Line: 1, Val: "$"}},
	"underscore":   {"_", Token{Type: TokenAtom, Line: 1, Val: "_"}},

	// Complex atoms
	"mixed special": {
		"hello-world#test",
		Token{Type: TokenAtom, Line: 1, Val: "hello-world#test"},
	},
	"operator like":   {"<=", Token{Type: TokenAtom, Line: 1, Val: "<="}},
	"operator like 2": {">=", Token{Type: TokenAtom, Line: 1, Val: ">="}},
	"operator like 3": {"<>", Token{Type: TokenAtom, Line: 1, Val: "<>"}},
	"lisp style":      {"car-cdr-cons", Token{Type: TokenAtom, Line: 1, Val: "car-cdr-cons"}},
	"with numbers":    {"test123abc", Token{Type: TokenAtom, Line: 1, Val: "test123abc"}},
	"ends with num":   {"var2", Token{Type: TokenAtom, Line: 1, Val: "var2"}},
	"boolean true":    {"#t", Token{Type: TokenAtom, Line: 1, Val: "#t"}},
	"boolean false":   {"#f", Token{Type: TokenAtom, Line: 1, Val: "#f"}},
	"keyword style":   {":keyword", Token{Type: TokenAtom, Line: 1, Val: ":keyword"}},
	"namespace style": {"ns/func", Token{Type: TokenAtom, Line: 1, Val: "ns/func"}},

	// Unicode atoms
	"unicode lambda": {"λ", Token{Type: TokenAtom, Line: 1, Val: "λ"}},
	"unicode mixed":  {"test-λ-func", Token{Type: TokenAtom, Line: 1, Val: "test-λ-func"}},
	"unicode emoji":  {"🚀", Token{Type: TokenAtom, Line: 1, Val: "🚀"}},
	"japanese":       {"こんにちは", Token{Type: TokenAtom, Line: 1, Val: "こんにちは"}},
	"greek":          {"αβγδε", Token{Type: TokenAtom, Line: 1, Val: "αβγδε"}},

	// Very long atoms
	"long atom": {
		strings.Repeat("a", 100),
		Token{Type: TokenAtom, Line: 1, Val: strings.Repeat("a", 100)},
	},
	"long mixed": {
		"very-long-atom-with-many-dashes-and-123-numbers-and-#-symbols",
		Token{
			Type: TokenAtom,
			Line: 1,
			Val:  "very-long-atom-with-many-dashes-and-123-numbers-and-#-symbols",
		},
	},
}

// Multi-token sequence tests.
var sequenceTests = map[string]struct {
	source string
	tokens []Token
}{
	"simple list": {
		source: "(+ 1 2)",
		tokens: []Token{
			{Type: TokenLeftParen, Line: 1, Val: "("},
			{Type: TokenAtom, Line: 1, Val: "+"},
			{Type: TokenNumber, Line: 1, Val: "1"},
			{Type: TokenNumber, Line: 1, Val: "2"},
			{Type: TokenRightParen, Line: 1, Val: ")"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"nested list": {
		source: "(f (g x) y)",
		tokens: []Token{
			{Type: TokenLeftParen, Line: 1, Val: "("},
			{Type: TokenAtom, Line: 1, Val: "f"},
			{Type: TokenLeftParen, Line: 1, Val: "("},
			{Type: TokenAtom, Line: 1, Val: "g"},
			{Type: TokenAtom, Line: 1, Val: "x"},
			{Type: TokenRightParen, Line: 1, Val: ")"},
			{Type: TokenAtom, Line: 1, Val: "y"},
			{Type: TokenRightParen, Line: 1, Val: ")"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"quoted expression": {
		source: "'(a b c)",
		tokens: []Token{
			{Type: TokenQuote, Line: 1, Val: "'"},
			{Type: TokenLeftParen, Line: 1, Val: "("},
			{Type: TokenAtom, Line: 1, Val: "a"},
			{Type: TokenAtom, Line: 1, Val: "b"},
			{Type: TokenAtom, Line: 1, Val: "c"},
			{Type: TokenRightParen, Line: 1, Val: ")"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"mixed numbers": {
		source: "42 3.14 0xFF -123 +456",
		tokens: []Token{
			{Type: TokenNumber, Line: 1, Val: "42"},
			{Type: TokenNumber, Line: 1, Val: "3.14"},
			{Type: TokenNumber, Line: 1, Val: "0xFF"},
			{Type: TokenNumber, Line: 1, Val: "-123"},
			{Type: TokenNumber, Line: 1, Val: "+456"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"dots and quotes": {
		source: ". ' . '",
		tokens: []Token{
			{Type: TokenDot, Line: 1, Val: "."},
			{Type: TokenQuote, Line: 1, Val: "'"},
			{Type: TokenDot, Line: 1, Val: "."},
			{Type: TokenQuote, Line: 1, Val: "'"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"backquoted expression": {
		source: "`(a ,b (c ,d) (e f))",
		tokens: []Token{
			{Type: TokenBackquote, Line: 1, Val: "`"},
			{Type: TokenLeftParen, Line: 1, Val: "("},
			{Type: TokenAtom, Line: 1, Val: "a"},
			{Type: TokenComma, Line: 1, Val: ","},
			{Type: TokenAtom, Line: 1, Val: "b"},
			{Type: TokenLeftParen, Line: 1, Val: "("},
			{Type: TokenAtom, Line: 1, Val: "c"},
			{Type: TokenComma, Line: 1, Val: ","},
			{Type: TokenAtom, Line: 1, Val: "d"},
			{Type: TokenRightParen, Line: 1, Val: ")"},
			{Type: TokenLeftParen, Line: 1, Val: "("},
			{Type: TokenAtom, Line: 1, Val: "e"},
			{Type: TokenAtom, Line: 1, Val: "f"},
			{Type: TokenRightParen, Line: 1, Val: ")"},
			{Type: TokenRightParen, Line: 1, Val: ")"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"comma-at expression": {
		source: "`(a ,@(list 1 2 3) b)",
		tokens: []Token{
			{Type: TokenBackquote, Line: 1, Val: "`"},
			{Type: TokenLeftParen, Line: 1, Val: "("},
			{Type: TokenAtom, Line: 1, Val: "a"},
			{Type: TokenCommaAt, Line: 1, Val: ",@"},
			{Type: TokenLeftParen, Line: 1, Val: "("},
			{Type: TokenAtom, Line: 1, Val: "list"},
			{Type: TokenNumber, Line: 1, Val: "1"},
			{Type: TokenNumber, Line: 1, Val: "2"},
			{Type: TokenNumber, Line: 1, Val: "3"},
			{Type: TokenRightParen, Line: 1, Val: ")"},
			{Type: TokenAtom, Line: 1, Val: "b"},
			{Type: TokenRightParen, Line: 1, Val: ")"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
}

// Whitespace and formatting tests.
var whitespaceTests = map[string]struct {
	source string
	tokens []Token
}{
	"spaces": {
		source: "  a   b  ",
		tokens: []Token{
			{Type: TokenAtom, Line: 1, Val: "a"},
			{Type: TokenAtom, Line: 1, Val: "b"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"tabs": {
		source: "\ta\t\tb\t",
		tokens: []Token{
			{Type: TokenAtom, Line: 1, Val: "a"},
			{Type: TokenAtom, Line: 1, Val: "b"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"newlines": {
		source: "a\nb\n",
		tokens: []Token{
			{Type: TokenAtom, Line: 1, Val: "a"},
			{Type: TokenAtom, Line: 2, Val: "b"},
			{Type: TokenEOF, Line: 3, Val: "EOF"},
		},
	},
	"mixed whitespace": {
		source: " \t\n a \t\n b \t\n ",
		tokens: []Token{
			{Type: TokenAtom, Line: 2, Val: "a"},
			{Type: TokenAtom, Line: 3, Val: "b"},
			{Type: TokenEOF, Line: 4, Val: "EOF"},
		},
	},
	"carriage returns": {
		source: "a\r\nb\r\n",
		tokens: []Token{
			{Type: TokenAtom, Line: 1, Val: "a"},
			{Type: TokenAtom, Line: 2, Val: "b"},
			{Type: TokenEOF, Line: 3, Val: "EOF"},
		},
	},
}

// Comment tests.
var commentTests = map[string]struct {
	source string
	tokens []Token
}{
	"line comment": {
		source: "; this is a comment\na",
		tokens: []Token{
			{Type: TokenAtom, Line: 2, Val: "a"},
			{Type: TokenEOF, Line: 2, Val: "EOF"},
		},
	},
	"comment at end": {
		source: "a ; comment",
		tokens: []Token{
			{Type: TokenAtom, Line: 1, Val: "a"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"multiple comments": {
		source: "; first\na ; second\n; third\nb",
		tokens: []Token{
			{Type: TokenAtom, Line: 2, Val: "a"},
			{Type: TokenAtom, Line: 4, Val: "b"},
			{Type: TokenEOF, Line: 4, Val: "EOF"},
		},
	},
	"comment with specials": {
		source: "; comment with ()'\". special chars\na",
		tokens: []Token{
			{Type: TokenAtom, Line: 2, Val: "a"},
			{Type: TokenEOF, Line: 2, Val: "EOF"},
		},
	},
	"empty comment": {
		source: ";\na",
		tokens: []Token{
			{Type: TokenAtom, Line: 2, Val: "a"},
			{Type: TokenEOF, Line: 2, Val: "EOF"},
		},
	},
	"comment only": {
		source: "; just a comment",
		tokens: []Token{
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"comment no newline": {
		source: "a ; comment at end",
		tokens: []Token{
			{Type: TokenAtom, Line: 1, Val: "a"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
}

// Line number tracking tests.
var lineNumberTests = map[string]struct {
	source string
	tokens []Token
}{
	"multiline positions": {
		source: "a\n  b\n    c",
		tokens: []Token{
			{Type: TokenAtom, Line: 1, Val: "a"},
			{Type: TokenAtom, Line: 2, Val: "b"},
			{Type: TokenAtom, Line: 3, Val: "c"},
			{Type: TokenEOF, Line: 3, Val: "EOF"},
		},
	},
	"complex multiline": {
		source: "(define factorial\n  (lambda (n)\n    (if (= n 0)\n        1\n        (* n (factorial (- n 1))))))",
		tokens: []Token{
			{Type: TokenLeftParen, Line: 1, Val: "("},
			{Type: TokenAtom, Line: 1, Val: "define"},
			{Type: TokenAtom, Line: 1, Val: "factorial"},
			{Type: TokenLeftParen, Line: 2, Val: "("},
			{Type: TokenAtom, Line: 2, Val: "lambda"},
			{Type: TokenLeftParen, Line: 2, Val: "("},
			{Type: TokenAtom, Line: 2, Val: "n"},
			{Type: TokenRightParen, Line: 2, Val: ")"},
			{Type: TokenLeftParen, Line: 3, Val: "("},
			{Type: TokenAtom, Line: 3, Val: "if"},
			{Type: TokenLeftParen, Line: 3, Val: "("},
			{Type: TokenAtom, Line: 3, Val: "="},
			{Type: TokenAtom, Line: 3, Val: "n"},
			{Type: TokenNumber, Line: 3, Val: "0"},
			{Type: TokenRightParen, Line: 3, Val: ")"},
			{Type: TokenNumber, Line: 4, Val: "1"},
			{Type: TokenLeftParen, Line: 5, Val: "("},
			{Type: TokenAtom, Line: 5, Val: "*"},
			{Type: TokenAtom, Line: 5, Val: "n"},
			{Type: TokenLeftParen, Line: 5, Val: "("},
			{Type: TokenAtom, Line: 5, Val: "factorial"},
			{Type: TokenLeftParen, Line: 5, Val: "("},
			{Type: TokenAtom, Line: 5, Val: "-"},
			{Type: TokenAtom, Line: 5, Val: "n"},
			{Type: TokenNumber, Line: 5, Val: "1"},
			{Type: TokenRightParen, Line: 5, Val: ")"},
			{Type: TokenRightParen, Line: 5, Val: ")"},
			{Type: TokenRightParen, Line: 5, Val: ")"},
			{Type: TokenRightParen, Line: 5, Val: ")"},
			{Type: TokenRightParen, Line: 5, Val: ")"},
			{Type: TokenRightParen, Line: 5, Val: ")"},
			{Type: TokenEOF, Line: 5, Val: "EOF"},
		},
	},
}

// Edge cases and boundary conditions.
var edgeCaseTests = map[string]struct {
	source string
	tokens []Token
}{
	"empty input": {
		source: "",
		tokens: []Token{
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"only whitespace": {
		source: "   \n\t  \n ",
		tokens: []Token{
			{Type: TokenEOF, Line: 3, Val: "EOF"},
		},
	},
	"plus minus edge": {
		source: "+ - +1 -2 +abc -def",
		tokens: []Token{
			{Type: TokenAtom, Line: 1, Val: "+"},
			{Type: TokenAtom, Line: 1, Val: "-"},
			{Type: TokenNumber, Line: 1, Val: "+1"},
			{Type: TokenNumber, Line: 1, Val: "-2"},
			{Type: TokenAtom, Line: 1, Val: "+abc"},
			{Type: TokenAtom, Line: 1, Val: "-def"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
		},
	},
	"zero prefixes": {
		source: "0 0x 0o 0b 0xff 0o755 0b101",
		tokens: []Token{
			// The tokenizer will miss some invalid numbers which will be later rejected by the parser.
			{Type: TokenNumber, Line: 1, Val: "0"},
			{Type: TokenNumber, Line: 1, Val: "0x"},
			{Type: TokenNumber, Line: 1, Val: "0o"},
			{Type: TokenNumber, Line: 1, Val: "0b"},
			{Type: TokenNumber, Line: 1, Val: "0xff"},
			{Type: TokenNumber, Line: 1, Val: "0o755"},
			{Type: TokenNumber, Line: 1, Val: "0b101"},
			{Type: TokenEOF, Line: 1, Val: "EOF"},
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
			tok := NewTokenizer(tc.source)
			token := tok.NextToken()
			if !cmp.Equal(token, tc.token) {
				t.Errorf("expected %#v, got %#v", tc.token, token)
			}
			// Check that next token is EOF
			eofToken := tok.NextToken()
			if eofToken.Type != TokenEOF {
				t.Errorf("expected EOF after token, got %#v", eofToken)
			}
		})
	}
}

func TestNumberFormats(t *testing.T) {
	for name, tc := range numberFormatTests {
		t.Run(name, func(t *testing.T) {
			tok := NewTokenizer(tc.source)
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
			tok := NewTokenizer(tc.source)
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
				t.Errorf("Token mismatch for %s", name)
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
				t.Errorf("Token mismatch for %s", name)
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
				t.Errorf("Token mismatch for %s", name)
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
				t.Errorf("Token mismatch for %s", name)
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
				t.Errorf("Token mismatch for %s", name)
				t.Errorf("Expected: %#v", tc.tokens)
				t.Errorf("Got:      %#v", tokens)
			}
		})
	}
}

func TestErrorConditions(t *testing.T) {
	for name, tc := range errorTests {
		t.Run(name, func(t *testing.T) {
			tok := NewTokenizer(tc.source)
			token := tok.NextToken()

			if tc.expectError {
				for token.Type != TokenErr && token.Type != TokenEOF {
					token = tok.NextToken()
				}
				if token.Type != TokenErr {
					t.Errorf("expected error token, got %#v", token)
				} else if !strings.Contains(token.Val, tc.errorMsg) {
					t.Errorf("expected error message to contain %q, got %q", tc.errorMsg, token.Val)
				}
			} else if token.Type == TokenErr {
				t.Errorf("unexpected error: %s", token.Val)
			}
		})
	}
}

// Performance and stress tests.
func TestLargeInput(t *testing.T) {
	// Test with a large input to ensure no performance issues
	largeAtom := strings.Repeat("a", 10000)
	tok := NewTokenizer(largeAtom)
	token := tok.NextToken()

	if token.Type != TokenAtom || token.Val != largeAtom {
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
		if tokens[i].Type != TokenAtom || tokens[i].Val != "a" {
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
		expected Token
	}{
		{"+", Token{Type: TokenAtom, Line: 1, Val: "+"}},
		{"-", Token{Type: TokenAtom, Line: 1, Val: "-"}},
		{"+123", Token{Type: TokenNumber, Line: 1, Val: "+123"}},
		{"-123", Token{Type: TokenNumber, Line: 1, Val: "-123"}},
		{"+abc", Token{Type: TokenAtom, Line: 1, Val: "+abc"}},
		{"-abc", Token{Type: TokenAtom, Line: 1, Val: "-abc"}},
		{"+0xFF", Token{Type: TokenNumber, Line: 1, Val: "+0xFF"}},
		{"-0xFF", Token{Type: TokenNumber, Line: 1, Val: "-0xFF"}},
	}

	for _, tc := range tests {
		t.Run(tc.source, func(t *testing.T) {
			tok := NewTokenizer(tc.source)
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
		tokens []Token
	}{
		{
			name:   "comment with no newline at EOF",
			source: "a; comment",
			tokens: []Token{
				{Type: TokenAtom, Line: 1, Val: "a"},
				{Type: TokenEOF, Line: 1, Val: "EOF"},
			},
		},
		{
			name:   "semicolon in atom",
			source: "test;atom",
			tokens: []Token{
				{Type: TokenAtom, Line: 1, Val: "test"},
				{Type: TokenEOF, Line: 1, Val: "EOF"}, // ';' starts comment, rest is ignored
			},
		},
		{
			name:   "multiple semicolons in comment",
			source: "; ;;; comment ;;;\na",
			tokens: []Token{
				{Type: TokenAtom, Line: 2, Val: "a"},
				{Type: TokenEOF, Line: 2, Val: "EOF"},
			},
		},
		{
			name:   "semicolon alone",
			source: ";",
			tokens: []Token{{Type: TokenEOF, Line: 1, Val: "EOF"}},
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
		token  Token
	}{
		{
			name:   "basic unicode",
			source: "λ",
			token:  Token{Type: TokenAtom, Line: 1, Val: "λ"},
		},
		{
			name:   "emoji",
			source: "🚀",
			token:  Token{Type: TokenAtom, Line: 1, Val: "🚀"},
		},
		{
			name:   "mixed ascii unicode",
			source: "test-λ-func",
			token:  Token{Type: TokenAtom, Line: 1, Val: "test-λ-func"},
		},
		{
			name:   "japanese",
			source: "こんにちは",
			token:  Token{Type: TokenAtom, Line: 1, Val: "こんにちは"},
		},
		{
			name:   "arabic",
			source: "مرحبا",
			token:  Token{Type: TokenAtom, Line: 1, Val: "مرحبا"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			tok := NewTokenizer(tc.source)
			token := tok.NextToken()
			if !cmp.Equal(token, tc.token) {
				t.Errorf("expected %#v, got %#v", tc.token, token)
			}
		})
	}
}
