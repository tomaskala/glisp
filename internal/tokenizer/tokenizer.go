package tokenizer

//go:generate stringer -output token_string.go -type TokenType

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

type Token struct {
	Type TokenType // The type of this token.
	Line int       // The line number of this token in the source text.
	Val  string    // The raw value of this token.
}

type TokenType int

const (
	TokenEOF TokenType = iota
	TokenErr
	TokenLeftParen
	TokenRightParen
	TokenDot
	TokenQuote
	TokenBackquote
	TokenComma
	TokenCommaAt
	TokenNumber
	TokenAtom
)

const (
	// eof denotes that we have reached the end of the input.
	eof = -1
	// forbiddenInAtom is a list of runes forbidden from appearing in an atom.
	forbiddenInAtom = "().';`,@"
)

type stateFn func(*Tokenizer) stateFn

type Tokenizer struct {
	source string // Source being tokenized.
	line   int    // Line number of the current token, 1-based.
	start  int    // Start position of the current token.
	pos    int    // Current position of the tokenizer.
	atEOF  bool   // Whether we have reached the end of the file.
	token  Token  // Token returned out to the parser.
}

func NewTokenizer(source string) *Tokenizer {
	return &Tokenizer{source: source, line: 1}
}

func (t *Tokenizer) accept(valid string) bool {
	if strings.ContainsRune(valid, t.next()) {
		return true
	}
	t.backup()
	return false
}

func (t *Tokenizer) acceptRun(valid string) {
	for strings.ContainsRune(valid, t.next()) {
	}
	t.backup()
}

func (t *Tokenizer) ignore() {
	t.start = t.pos
}

func (t *Tokenizer) createToken(typ TokenType, val string) Token {
	return Token{typ, t.line, val}
}

func (t *Tokenizer) emit(typ TokenType) stateFn {
	t.token = t.createToken(typ, t.source[t.start:t.pos])
	t.start = t.pos
	return nil
}

func (t *Tokenizer) errorf(format string, args ...any) stateFn {
	t.source = t.source[:0]
	t.line = 1
	t.start = 0
	t.pos = 0
	t.atEOF = true
	t.token = t.createToken(TokenErr, fmt.Sprintf(format, args...))
	return nil
}

func (t *Tokenizer) next() rune {
	if t.pos >= len(t.source) {
		t.atEOF = true
		return eof
	}
	r, w := utf8.DecodeRuneInString(t.source[t.pos:])
	t.pos += w
	if r == '\n' {
		t.line++
	}
	return r
}

func (t *Tokenizer) backup() {
	if t.atEOF || t.pos == 0 {
		return
	}
	r, w := utf8.DecodeLastRuneInString(t.source[:t.pos])
	t.pos -= w
	if r == '\n' {
		t.line--
	}
}

func (t *Tokenizer) peek() rune {
	r := t.next()
	t.backup()
	return r
}

func (t *Tokenizer) NextToken() Token {
	state := readExpr
	for state != nil {
		state = state(t)
	}
	return t.token
}

func readExpr(t *Tokenizer) stateFn {
	switch r := t.next(); {
	case r == eof:
		t.token = t.createToken(TokenEOF, "EOF")
		return nil
	case unicode.IsSpace(r):
		return readSpace(t)
	case r == '(':
		return t.emit(TokenLeftParen)
	case r == ')':
		return t.emit(TokenRightParen)
	case r == '.':
		// This can be a floating point number with the leading zero missing.
		next := t.peek()
		if '0' <= next && next <= '9' {
			t.backup() // Put the '.' back.
			return readNumber(t)
		}
		return t.emit(TokenDot)
	case r == '\'':
		return t.emit(TokenQuote)
	case r == '`':
		return t.emit(TokenBackquote)
	case r == ',':
		if t.accept("@") {
			return t.emit(TokenCommaAt)
		}
		return t.emit(TokenComma)
	case r == ';':
		return readComment(t)
	case r == '+' || r == '-':
		// This can be either a number (peek() is a digit) or an atom.
		next := t.peek()
		if '0' <= next && next <= '9' {
			t.backup() // Put the '+' or '-' back.
			return readNumber(t)
		}
		return readAtom(t)
	case '0' <= r && r <= '9':
		// We have to backup in case we consumed the leading 0 in a hexadecimal, octal or binary number.
		t.backup()
		return readNumber(t)
	case isAtomStart(r):
		return readAtom(t)
	default:
		return t.errorf("unrecognized character in action: %#U", r)
	}
}

func readSpace(t *Tokenizer) stateFn {
	for {
		r := t.next()
		if !unicode.IsSpace(r) {
			t.backup()
			break
		}
	}
	t.ignore()
	return readExpr(t)
}

func readComment(t *Tokenizer) stateFn {
	for {
		r := t.next()
		if r == '\n' || r == eof {
			break
		}
	}
	t.ignore()
	return readExpr(t)
}

func readNumber(t *Tokenizer) stateFn {
	t.accept("+-")
	// Decimal number by default.
	digits := "0123456789_"
	if t.accept("0") {
		switch {
		case t.accept("Xx"):
			// Try hexadecimal.
			digits = "0123456789abcdefABCDEF_"
		case t.accept("Oo"):
			// Try octal.
			digits = "01234567_"
		case t.accept("Bb"):
			// Try binary.
			digits = "01_"
		}
	}
	t.acceptRun(digits)
	if t.accept(".") {
		// Floating point.
		t.acceptRun(digits)
	}
	if len(digits) == 10+1 && t.accept("Ee") {
		// Decimal scientific notation.
		t.accept("+-")
		t.acceptRun("0123456789_")
	}
	if len(digits) == 16+6+1 && t.accept("pP") {
		// Hexadecimal scientific notation.
		t.accept("+-")
		t.acceptRun("0123456789_")
	}
	next := t.peek()
	if isAtom(next) {
		return t.errorf("Atom cannot start with a number")
	}
	return t.emit(TokenNumber)
}

func readAtom(t *Tokenizer) stateFn {
	for {
		r := t.next()
		if !isAtom(r) {
			t.backup()
			break
		}
	}
	val := t.source[t.start:t.pos]
	if strings.EqualFold(val, "NaN") ||
		strings.EqualFold(val, "Inf") ||
		strings.EqualFold(val, "-Inf") ||
		strings.EqualFold(val, "Infinity") ||
		strings.EqualFold(val, "-Infinity") {
		return t.emit(TokenNumber)
	}
	return t.emit(TokenAtom)
}

func isAtomStart(r rune) bool {
	return isAtom(r) && !unicode.IsDigit(r)
}

func isAtom(r rune) bool {
	return unicode.IsPrint(r) && !unicode.IsSpace(r) && r != unicode.ReplacementChar &&
		!strings.ContainsRune(forbiddenInAtom, r)
}
