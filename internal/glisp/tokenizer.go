package glisp

import (
	"fmt"
	"strings"
	"unicode"
	"unicode/utf8"
)

type token struct {
	typ  tokenType // The type of this token.
	pos  pos       // The starting position of this token in the input string, in bytes.
	line int       // The starting line of this token in the input string, 1-based.
	val  string    // The raw value of this token.
}

func (t token) String() string {
	switch t.typ {
	case tokenEOF:
		return "<EOF>"
	case tokenErr:
		return t.val
	default:
		return fmt.Sprintf("<%s>", t.val)
	}
}

type tokenType int

type pos int

const (
	tokenEOF tokenType = iota
	tokenErr
	tokenLeftParen
	tokenRightParen
	tokenDot
	tokenQuote
	tokenNumber
	tokenAtom
)

const (
	// eof denotes that we have reached the end of the input.
	eof = -1
	// forbiddenInAtom is a list of runes forbidden from appearing in an atom.
	forbiddenInAtom = "().'"
)

type stateFn func(*tokenizer) stateFn

type tokenizer struct {
	name   string // Name of the source being tokenized, used for error reporting.
	source string // Source being tokenized.
	start  pos    // Start position of the current token.
	pos    pos    // Current position of the tokenizer.
	line   int    // Current line of the tokenizer, 1-based.
	atEOF  bool   // Whether we have reached the end of the file.
	token  token  // Token returned out to the parser.
}

func newTokenizer(name, source string) *tokenizer {
	return &tokenizer{name: name, source: source, line: 1}
}

func (t *tokenizer) accept(valid string) bool {
	if strings.ContainsRune(valid, t.next()) {
		return true
	}
	t.backup()
	return false
}

func (t *tokenizer) acceptRun(valid string) {
	for strings.ContainsRune(valid, t.next()) {
	}
	t.backup()
}

func (t *tokenizer) ignore() {
	t.start = t.pos
}

func (t *tokenizer) emit(typ tokenType) stateFn {
	t.token = token{typ, t.pos, t.line, t.source[t.start:t.pos]}
	t.start = t.pos
	return nil
}

func (t *tokenizer) errorf(format string, args ...any) stateFn {
	t.token = token{tokenErr, t.pos, t.line, fmt.Sprintf(format, args...)}
	t.start = 0
	t.pos = pos(0)
	t.line = 1
	t.source = t.source[:0]
	return nil
}

func (t *tokenizer) next() rune {
	if int(t.pos) >= len(t.source) {
		t.atEOF = true
		return eof
	}
	r, w := utf8.DecodeRuneInString(t.source[t.pos:])
	t.pos += pos(w)
	if r == '\n' {
		t.line++
	}
	return r
}

func (t *tokenizer) backup() {
	if !t.atEOF && t.pos > 0 {
		r, w := utf8.DecodeLastRuneInString(t.source[:t.pos])
		t.pos -= pos(w)
		if r == '\n' {
			t.line--
		}
	}
}

func (t *tokenizer) peek() rune {
	r := t.next()
	t.backup()
	return r
}

func (t *tokenizer) nextToken() token {
	t.token = token{tokenEOF, t.pos, t.line, "EOF"}
	state := readExpr
	for {
		state = state(t)
		if state == nil {
			return t.token
		}
	}
}

func readExpr(t *tokenizer) stateFn {
	switch r := t.next(); {
	case r == eof:
		return nil
	case unicode.IsSpace(r):
		t.backup()
		return readSpace(t)
	case r == '(':
		return t.emit(tokenLeftParen)
	case r == ')':
		return t.emit(tokenRightParen)
	case r == '.':
		return t.emit(tokenDot)
	case r == '\'':
		return t.emit(tokenQuote)
	case r == ';':
		return readComment(t)
	case r == '+' || r == '-':
		// This can be either a number (peek() is a digit) or an atom.
		next := t.peek()
		t.backup()
		if '0' <= next && next <= '9' {
			return readNumber(t)
		}
		return readAtom(t)
	case '0' <= r && r <= '9':
		t.backup()
		return readNumber(t)
	case isAtomStart(r):
		return readAtom(t)
	default:
		return t.errorf("unrecognized character in action: %#U", r)
	}
}

func readSpace(t *tokenizer) stateFn {
	for {
		r := t.peek()
		if !unicode.IsSpace(r) {
			break
		}
		t.next()
	}
	t.ignore()
	return readExpr(t)
}

func readComment(t *tokenizer) stateFn {
	for {
		r := t.next()
		if r == '\n' || r == 0 {
			break
		}
	}
	t.ignore()
	return readExpr(t)
}

func readNumber(t *tokenizer) stateFn {
	t.accept("+-")
	// Decimal number by default.
	digits := "0123456789_"
	if t.accept("0") {
		if t.accept("Xx") {
			// Try hexadecimal.
			digits = "0123456789abcdefABCDEF_"
		} else if t.accept("Oo") {
			// Try octal.
			digits = "01234567_"
		} else if t.accept("Bb") {
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
	return t.emit(tokenNumber)
}

func readAtom(t *tokenizer) stateFn {
	for {
		r := t.peek()
		if !isAtom(r) {
			break
		}
		t.next()
	}
	return t.emit(tokenAtom)
}

func isAtomStart(r rune) bool {
	return isAtom(r) && !unicode.IsDigit(r)
}

func isAtom(r rune) bool {
	return unicode.IsPrint(r) && !unicode.IsSpace(r) && !strings.ContainsRune(forbiddenInAtom, r)
}
