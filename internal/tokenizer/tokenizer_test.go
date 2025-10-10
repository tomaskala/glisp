package tokenizer

import (
	"strings"
	"testing"

	"github.com/google/go-cmp/cmp"
)

// Helper function to tokenize entire input and return all tokens
func tokenizeAll(source string) []Token {
	tokenizer := NewTokenizer(source)
	var tokens []Token
	for {
		token := tokenizer.NextToken()
		tokens = append(tokens, token)
		if token.Type == TokenEOF {
			break
		}
	}
	return tokens
}

// Basic token tests
var basicTokenTests = map[string]struct {
	source string
	token  Token
}{
	// Parentheses
	"left paren":  {"(", Token{TokenLeftParen, 1, "("}},
	"right paren": {")", Token{TokenRightParen, 1, ")"}},

	// Special characters
	"dot":   {".", Token{TokenDot, 1, "."}},
	"quote": {"'", Token{TokenQuote, 1, "'"}},

	// Basic atoms
	"simple atom":        {"hello", Token{TokenAtom, 1, "hello"}},
	"atom with dash":     {"hello-world", Token{TokenAtom, 1, "hello-world"}},
	"atom with question": {"null?", Token{TokenAtom, 1, "null?"}},
	"atom with numbers":  {"var123", Token{TokenAtom, 1, "var123"}},
	"single char atom":   {"x", Token{TokenAtom, 1, "x"}},

	// Mathematical operators
	"plus":     {"+", Token{TokenAtom, 1, "+"}},
	"minus":    {"-", Token{TokenAtom, 1, "-"}},
	"multiply": {"*", Token{TokenAtom, 1, "*"}},
	"divide":   {"/", Token{TokenAtom, 1, "/"}},

	// Basic numbers
	"zero":           {"0", Token{TokenNumber, 1, "0"}},
	"positive int":   {"42", Token{TokenNumber, 1, "42"}},
	"negative int":   {"-123", Token{TokenNumber, 1, "-123"}},
	"explicit pos":   {"+456", Token{TokenNumber, 1, "+456"}},
	"simple float":   {"3.14", Token{TokenNumber, 1, "3.14"}},
	"negative float": {"-2.718", Token{TokenNumber, 1, "-2.718"}},
}

// Number format tests - comprehensive coverage of all number formats
var numberFormatTests = map[string]struct {
	source string
	token  Token
}{
	// Decimal integers
	"single digit":     {"7", Token{TokenNumber, 1, "7"}},
	"multi digit":      {"1234567890", Token{TokenNumber, 1, "1234567890"}},
	"with underscores": {"1_000_000", Token{TokenNumber, 1, "1_000_000"}},
	"leading zeros":    {"00123", Token{TokenNumber, 1, "00123"}},

	// Hexadecimal
	"hex lowercase":    {"0xff", Token{TokenNumber, 1, "0xff"}},
	"hex uppercase":    {"0XFF", Token{TokenNumber, 1, "0XFF"}},
	"hex mixed case":   {"0xDeAdBeEf", Token{TokenNumber, 1, "0xDeAdBeEf"}},
	"hex with under":   {"0xFF_00_AA", Token{TokenNumber, 1, "0xFF_00_AA"}},
	"hex signed pos":   {"+0x123", Token{TokenNumber, 1, "+0x123"}},
	"hex signed neg":   {"-0xABC", Token{TokenNumber, 1, "-0xABC"}},
	"hex single digit": {"0x1", Token{TokenNumber, 1, "0x1"}},
	"hex all digits":   {"0x0123456789abcdefABCDEF", Token{TokenNumber, 1, "0x0123456789abcdefABCDEF"}},

	// Octal
	"octal lowercase":  {"0o755", Token{TokenNumber, 1, "0o755"}},
	"octal uppercase":  {"0O644", Token{TokenNumber, 1, "0O644"}},
	"octal with under": {"0o777_444", Token{TokenNumber, 1, "0o777_444"}},
	"octal signed pos": {"+0o123", Token{TokenNumber, 1, "+0o123"}},
	"octal signed neg": {"-0o567", Token{TokenNumber, 1, "-0o567"}},
	"octal single":     {"0o1", Token{TokenNumber, 1, "0o1"}},
	"octal all digits": {"0o01234567", Token{TokenNumber, 1, "0o01234567"}},

	// Binary
	"binary lowercase":  {"0b101", Token{TokenNumber, 1, "0b101"}},
	"binary uppercase":  {"0B110", Token{TokenNumber, 1, "0B110"}},
	"binary with under": {"0b1010_1111", Token{TokenNumber, 1, "0b1010_1111"}},
	"binary signed pos": {"+0b11", Token{TokenNumber, 1, "+0b11"}},
	"binary signed neg": {"-0b01", Token{TokenNumber, 1, "-0b01"}},
	"binary single":     {"0b1", Token{TokenNumber, 1, "0b1"}},
	"binary long":       {"0b11110000111100001111", Token{TokenNumber, 1, "0b11110000111100001111"}},

	// Floating point decimal
	"float simple":      {"1.5", Token{TokenNumber, 1, "1.5"}},
	"float zero":        {"0.0", Token{TokenNumber, 1, "0.0"}},
	"float no leading":  {".5", Token{TokenNumber, 1, ".5"}},
	"float no trailing": {"5.", Token{TokenNumber, 1, "5."}},
	"float with under":  {"3.141_592", Token{TokenNumber, 1, "3.141_592"}},
	"float signed pos":  {"+2.5", Token{TokenNumber, 1, "+2.5"}},
	"float signed neg":  {"-9.75", Token{TokenNumber, 1, "-9.75"}},

	// Scientific notation (decimal)
	"sci simple":       {"1e5", Token{TokenNumber, 1, "1e5"}},
	"sci uppercase":    {"1E5", Token{TokenNumber, 1, "1E5"}},
	"sci negative exp": {"1e-5", Token{TokenNumber, 1, "1e-5"}},
	"sci positive exp": {"1e+5", Token{TokenNumber, 1, "1e+5"}},
	"sci float base":   {"3.14e2", Token{TokenNumber, 1, "3.14e2"}},
	"sci with under":   {"1_000e3", Token{TokenNumber, 1, "1_000e3"}},
	"sci signed":       {"-2.5E-10", Token{TokenNumber, 1, "-2.5E-10"}},
	"sci zero exp":     {"5e0", Token{TokenNumber, 1, "5e0"}},

	// Hexadecimal floating point
	"hex float simple":   {"0x1p0", Token{TokenNumber, 1, "0x1p0"}},
	"hex float with dot": {"0x1.5p2", Token{TokenNumber, 1, "0x1.5p2"}},
	"hex float neg exp":  {"0x1p-2", Token{TokenNumber, 1, "0x1p-2"}},
	"hex float pos exp":  {"0x1p+3", Token{TokenNumber, 1, "0x1p+3"}},
	"hex float upper":    {"0X1.FP4", Token{TokenNumber, 1, "0X1.FP4"}},
	"hex float signed":   {"-0x1.ABCp-5", Token{TokenNumber, 1, "-0x1.ABCp-5"}},

	// Edge cases
	"negative zero": {"-0", Token{TokenNumber, 1, "-0"}},
	"positive zero": {"+0", Token{TokenNumber, 1, "+0"}},
	"zero float":    {"0.00000", Token{TokenNumber, 1, "0.00000"}},
	"large number":  {"999999999999999999", Token{TokenNumber, 1, "999999999999999999"}},
	"tiny float":    {"0.000000001", Token{TokenNumber, 1, "0.000000001"}},
}

// Atom edge case tests
var atomEdgeCaseTests = map[string]struct {
	source string
	token  Token
}{
	// Special characters in atoms
	"at symbol":    {"@", Token{TokenAtom, 1, "@"}},
	"hash":         {"#", Token{TokenAtom, 1, "#"}},
	"percent":      {"%", Token{TokenAtom, 1, "%"}},
	"caret":        {"^", Token{TokenAtom, 1, "^"}},
	"ampersand":    {"&", Token{TokenAtom, 1, "&"}},
	"exclamation":  {"!", Token{TokenAtom, 1, "!"}},
	"tilde":        {"~", Token{TokenAtom, 1, "~"}},
	"backslash":    {"\\", Token{TokenAtom, 1, "\\"}},
	"pipe":         {"|", Token{TokenAtom, 1, "|"}},
	"colon":        {":", Token{TokenAtom, 1, ":"}},
	"less than":    {"<", Token{TokenAtom, 1, "<"}},
	"greater than": {">", Token{TokenAtom, 1, ">"}},
	"equals":       {"=", Token{TokenAtom, 1, "="}},
	"dollar":       {"$", Token{TokenAtom, 1, "$"}},
	"underscore":   {"_", Token{TokenAtom, 1, "_"}},

	// Complex atoms
	"mixed special":   {"hello-world@test", Token{TokenAtom, 1, "hello-world@test"}},
	"operator like":   {"<=", Token{TokenAtom, 1, "<="}},
	"operator like 2": {">=", Token{TokenAtom, 1, ">="}},
	"operator like 3": {"<>", Token{TokenAtom, 1, "<>"}},
	"lisp style":      {"car-cdr-cons", Token{TokenAtom, 1, "car-cdr-cons"}},
	"with numbers":    {"test123abc", Token{TokenAtom, 1, "test123abc"}},
	"ends with num":   {"var2", Token{TokenAtom, 1, "var2"}},
	"boolean true":    {"#t", Token{TokenAtom, 1, "#t"}},
	"boolean false":   {"#f", Token{TokenAtom, 1, "#f"}},
	"keyword style":   {":keyword", Token{TokenAtom, 1, ":keyword"}},
	"namespace style": {"ns/func", Token{TokenAtom, 1, "ns/func"}},

	// Unicode atoms
	"unicode lambda": {"λ", Token{TokenAtom, 1, "λ"}},
	"unicode mixed":  {"test-λ-func", Token{TokenAtom, 1, "test-λ-func"}},
	"unicode emoji":  {"🚀", Token{TokenAtom, 1, "🚀"}},
	"japanese":       {"こんにちは", Token{TokenAtom, 1, "こんにちは"}},
	"greek":          {"αβγδε", Token{TokenAtom, 1, "αβγδε"}},

	// Very long atoms
	"long atom":  {strings.Repeat("a", 100), Token{TokenAtom, 1, strings.Repeat("a", 100)}},
	"long mixed": {"very-long-atom-with-many-dashes-and-123-numbers-and-@-symbols", Token{TokenAtom, 1, "very-long-atom-with-many-dashes-and-123-numbers-and-@-symbols"}},
}

// Multi-token sequence tests
var sequenceTests = map[string]struct {
	source string
	tokens []Token
}{
	"simple list": {
		source: "(+ 1 2)",
		tokens: []Token{
			{TokenLeftParen, 1, "("},
			{TokenAtom, 1, "+"},
			{TokenNumber, 1, "1"},
			{TokenNumber, 1, "2"},
			{TokenRightParen, 1, ")"},
			{TokenEOF, 1, "EOF"},
		},
	},
	"nested list": {
		source: "(f (g x) y)",
		tokens: []Token{
			{TokenLeftParen, 1, "("},
			{TokenAtom, 1, "f"},
			{TokenLeftParen, 1, "("},
			{TokenAtom, 1, "g"},
			{TokenAtom, 1, "x"},
			{TokenRightParen, 1, ")"},
			{TokenAtom, 1, "y"},
			{TokenRightParen, 1, ")"},
			{TokenEOF, 1, "EOF"},
		},
	},
	"quoted expression": {
		source: "'(a b c)",
		tokens: []Token{
			{TokenQuote, 1, "'"},
			{TokenLeftParen, 1, "("},
			{TokenAtom, 1, "a"},
			{TokenAtom, 1, "b"},
			{TokenAtom, 1, "c"},
			{TokenRightParen, 1, ")"},
			{TokenEOF, 1, "EOF"},
		},
	},
	"mixed numbers": {
		source: "42 3.14 0xFF -123 +456",
		tokens: []Token{
			{TokenNumber, 1, "42"},
			{TokenNumber, 1, "3.14"},
			{TokenNumber, 1, "0xFF"},
			{TokenNumber, 1, "-123"},
			{TokenNumber, 1, "+456"},
			{TokenEOF, 1, "EOF"},
		},
	},
	"dots and quotes": {
		source: ". ' . '",
		tokens: []Token{
			{TokenDot, 1, "."},
			{TokenQuote, 1, "'"},
			{TokenDot, 1, "."},
			{TokenQuote, 1, "'"},
			{TokenEOF, 1, "EOF"},
		},
	},
}

// Whitespace and formatting tests
var whitespaceTests = map[string]struct {
	source string
	tokens []Token
}{
	"spaces": {
		source: "  a   b  ",
		tokens: []Token{
			{TokenAtom, 1, "a"},
			{TokenAtom, 1, "b"},
			{TokenEOF, 1, "EOF"},
		},
	},
	"tabs": {
		source: "\ta\t\tb\t",
		tokens: []Token{
			{TokenAtom, 1, "a"},
			{TokenAtom, 1, "b"},
			{TokenEOF, 1, "EOF"},
		},
	},
	"newlines": {
		source: "a\nb\n",
		tokens: []Token{
			{TokenAtom, 1, "a"},
			{TokenAtom, 2, "b"},
			{TokenEOF, 3, "EOF"},
		},
	},
	"mixed whitespace": {
		source: " \t\n a \t\n b \t\n ",
		tokens: []Token{
			{TokenAtom, 2, "a"},
			{TokenAtom, 3, "b"},
			{TokenEOF, 4, "EOF"},
		},
	},
	"carriage returns": {
		source: "a\r\nb\r\n",
		tokens: []Token{
			{TokenAtom, 1, "a"},
			{TokenAtom, 2, "b"},
			{TokenEOF, 3, "EOF"},
		},
	},
}

// Comment tests
var commentTests = map[string]struct {
	source string
	tokens []Token
}{
	"line comment": {
		source: "; this is a comment\na",
		tokens: []Token{
			{TokenAtom, 2, "a"},
			{TokenEOF, 2, "EOF"},
		},
	},
	"comment at end": {
		source: "a ; comment",
		tokens: []Token{
			{TokenAtom, 1, "a"},
			{TokenEOF, 1, "EOF"},
		},
	},
	"multiple comments": {
		source: "; first\na ; second\n; third\nb",
		tokens: []Token{
			{TokenAtom, 2, "a"},
			{TokenAtom, 4, "b"},
			{TokenEOF, 4, "EOF"},
		},
	},
	"comment with specials": {
		source: "; comment with ()'\". special chars\na",
		tokens: []Token{
			{TokenAtom, 2, "a"},
			{TokenEOF, 2, "EOF"},
		},
	},
	"empty comment": {
		source: ";\na",
		tokens: []Token{
			{TokenAtom, 2, "a"},
			{TokenEOF, 2, "EOF"},
		},
	},
	"comment only": {
		source: "; just a comment",
		tokens: []Token{
			{TokenEOF, 1, "EOF"},
		},
	},
	"comment no newline": {
		source: "a ; comment at end",
		tokens: []Token{
			{TokenAtom, 1, "a"},
			{TokenEOF, 1, "EOF"},
		},
	},
}

// Line number tracking tests
var lineNumberTests = map[string]struct {
	source string
	tokens []Token
}{
	"multiline positions": {
		source: "a\n  b\n    c",
		tokens: []Token{
			{TokenAtom, 1, "a"},
			{TokenAtom, 2, "b"},
			{TokenAtom, 3, "c"},
			{TokenEOF, 3, "EOF"},
		},
	},
	"complex multiline": {
		source: "(define factorial\n  (lambda (n)\n    (if (= n 0)\n        1\n        (* n (factorial (- n 1))))))",
		tokens: []Token{
			{TokenLeftParen, 1, "("},
			{TokenAtom, 1, "define"},
			{TokenAtom, 1, "factorial"},
			{TokenLeftParen, 2, "("},
			{TokenAtom, 2, "lambda"},
			{TokenLeftParen, 2, "("},
			{TokenAtom, 2, "n"},
			{TokenRightParen, 2, ")"},
			{TokenLeftParen, 3, "("},
			{TokenAtom, 3, "if"},
			{TokenLeftParen, 3, "("},
			{TokenAtom, 3, "="},
			{TokenAtom, 3, "n"},
			{TokenNumber, 3, "0"},
			{TokenRightParen, 3, ")"},
			{TokenNumber, 4, "1"},
			{TokenLeftParen, 5, "("},
			{TokenAtom, 5, "*"},
			{TokenAtom, 5, "n"},
			{TokenLeftParen, 5, "("},
			{TokenAtom, 5, "factorial"},
			{TokenLeftParen, 5, "("},
			{TokenAtom, 5, "-"},
			{TokenAtom, 5, "n"},
			{TokenNumber, 5, "1"},
			{TokenRightParen, 5, ")"},
			{TokenRightParen, 5, ")"},
			{TokenRightParen, 5, ")"},
			{TokenRightParen, 5, ")"},
			{TokenRightParen, 5, ")"},
			{TokenRightParen, 5, ")"},
			{TokenEOF, 5, "EOF"},
		},
	},
}

// Edge cases and boundary conditions
var edgeCaseTests = map[string]struct {
	source string
	tokens []Token
}{
	"empty input": {
		source: "",
		tokens: []Token{
			{TokenEOF, 1, "EOF"},
		},
	},
	"only whitespace": {
		source: "   \n\t  \n ",
		tokens: []Token{
			{TokenEOF, 3, "EOF"},
		},
	},
	"plus minus edge": {
		source: "+ - +1 -2 +abc -def",
		tokens: []Token{
			{TokenAtom, 1, "+"},
			{TokenAtom, 1, "-"},
			{TokenNumber, 1, "+1"},
			{TokenNumber, 1, "-2"},
			{TokenAtom, 1, "+abc"},
			{TokenAtom, 1, "-def"},
			{TokenEOF, 1, "EOF"},
		},
	},
	"zero prefixes": {
		source: "0 0x 0o 0b 0xff 0o755 0b101",
		tokens: []Token{
			// The tokenizer will miss some invalid numbers which will be later rejected by the parser.
			{TokenNumber, 1, "0"},
			{TokenNumber, 1, "0x"},
			{TokenNumber, 1, "0o"},
			{TokenNumber, 1, "0b"},
			{TokenNumber, 1, "0xff"},
			{TokenNumber, 1, "0o755"},
			{TokenNumber, 1, "0b101"},
			{TokenEOF, 1, "EOF"},
		},
	},
}

// Error condition tests
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

// Main test functions
func TestBasicTokens(t *testing.T) {
	for name, tc := range basicTokenTests {
		t.Run(name, func(t *testing.T) {
			tokenizer := NewTokenizer(tc.source)
			token := tokenizer.NextToken()
			if !cmp.Equal(token, tc.token) {
				t.Errorf("expected %#v, got %#v", tc.token, token)
			}
			// Check that next token is EOF
			eofToken := tokenizer.NextToken()
			if eofToken.Type != TokenEOF {
				t.Errorf("expected EOF after token, got %#v", eofToken)
			}
		})
	}
}

func TestNumberFormats(t *testing.T) {
	for name, tc := range numberFormatTests {
		t.Run(name, func(t *testing.T) {
			tokenizer := NewTokenizer(tc.source)
			token := tokenizer.NextToken()
			if !cmp.Equal(token, tc.token) {
				t.Errorf("expected %#v, got %#v", tc.token, token)
			}
		})
	}
}

func TestAtomEdgeCases(t *testing.T) {
	for name, tc := range atomEdgeCaseTests {
		t.Run(name, func(t *testing.T) {
			tokenizer := NewTokenizer(tc.source)
			token := tokenizer.NextToken()
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

func TestEdgeCases(t *testing.T) {
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
			tokenizer := NewTokenizer(tc.source)
			token := tokenizer.NextToken()

			if tc.expectError {
				for token.Type != TokenErr && token.Type != TokenEOF {
					token = tokenizer.NextToken()
				}
				if token.Type != TokenErr {
					t.Errorf("expected error token, got %#v", token)
				} else if !strings.Contains(token.Val, tc.errorMsg) {
					t.Errorf("expected error message to contain %q, got %q", tc.errorMsg, token.Val)
				}
			} else {
				if token.Type == TokenErr {
					t.Errorf("unexpected error: %s", token.Val)
				}
			}
		})
	}
}

// Performance and stress tests
func TestLargeInput(t *testing.T) {
	// Test with a large input to ensure no performance issues
	largeAtom := strings.Repeat("a", 10000)
	tokenizer := NewTokenizer(largeAtom)
	token := tokenizer.NextToken()

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
		{"+", Token{TokenAtom, 1, "+"}},
		{"-", Token{TokenAtom, 1, "-"}},
		{"+123", Token{TokenNumber, 1, "+123"}},
		{"-123", Token{TokenNumber, 1, "-123"}},
		{"+abc", Token{TokenAtom, 1, "+abc"}},
		{"-abc", Token{TokenAtom, 1, "-abc"}},
		{"+0xFF", Token{TokenNumber, 1, "+0xFF"}},
		{"-0xFF", Token{TokenNumber, 1, "-0xFF"}},
	}

	for _, tc := range tests {
		t.Run(tc.source, func(t *testing.T) {
			tokenizer := NewTokenizer(tc.source)
			token := tokenizer.NextToken()
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
				{TokenAtom, 1, "a"},
				{TokenEOF, 1, "EOF"},
			},
		},
		{
			name:   "semicolon in atom",
			source: "test;atom",
			tokens: []Token{
				{TokenAtom, 1, "test"},
				{TokenEOF, 1, "EOF"}, // ';' starts comment, rest is ignored
			},
		},
		{
			name:   "multiple semicolons in comment",
			source: "; ;;; comment ;;;\na",
			tokens: []Token{
				{TokenAtom, 2, "a"},
				{TokenEOF, 2, "EOF"},
			},
		},
		{
			name:   "semicolon alone",
			source: ";",
			tokens: []Token{{TokenEOF, 1, "EOF"}},
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
			token:  Token{TokenAtom, 1, "λ"},
		},
		{
			name:   "emoji",
			source: "🚀",
			token:  Token{TokenAtom, 1, "🚀"},
		},
		{
			name:   "mixed ascii unicode",
			source: "test-λ-func",
			token:  Token{TokenAtom, 1, "test-λ-func"},
		},
		{
			name:   "japanese",
			source: "こんにちは",
			token:  Token{TokenAtom, 1, "こんにちは"},
		},
		{
			name:   "arabic",
			source: "مرحبا",
			token:  Token{TokenAtom, 1, "مرحبا"},
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			tokenizer := NewTokenizer(tc.source)
			token := tokenizer.NextToken()
			if !cmp.Equal(token, tc.token) {
				t.Errorf("expected %#v, got %#v", tc.token, token)
			}
		})
	}
}
