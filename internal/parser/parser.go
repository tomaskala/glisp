package parser

import (
	"fmt"
	"strconv"

	"tomaskala.com/glisp/internal/runtime"
	"tomaskala.com/glisp/internal/tokenizer"
)

type ParseError struct {
	Line    int
	Message string
}

func (e *ParseError) Error() string {
	return fmt.Sprintf("parse error at %d: %s", e.Line, e.Message)
}

type Parser struct {
	name      string               // Name of the source being parsed, used for error reporting.
	previous  tokenizer.Token      // The last read token.
	current   tokenizer.Token      // The next token.
	tokenizer *tokenizer.Tokenizer // The underlying tokenizer.
}

func NewParser(name, source string) *Parser {
	p := &Parser{name: name, tokenizer: tokenizer.NewTokenizer(source)}
	p.advance() // Initialize the first token.
	return p
}

func (p *Parser) AtEOF() bool {
	return p.match(tokenizer.TokenEOF)
}

func (p *Parser) Expression() (result runtime.Value, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(*ParseError)
		}
	}()

	return p.expression(), nil
}

// TODO: We need to inject line number information to each runtime.Value.
func (p *Parser) expression() runtime.Value {
	switch {
	case p.match(tokenizer.TokenAtom):
		return runtime.MakeAtom(p.previous.Val)
	case p.match(tokenizer.TokenNumber):
		n := p.parseNumber()
		return runtime.MakeNumber(n)
	case p.match(tokenizer.TokenQuote):
		quoted := p.expression()
		return runtime.Cons(runtime.MakeAtom("quote"), runtime.Cons(quoted, runtime.MakeNil()))
	case p.match(tokenizer.TokenBackquote):
		return p.backquote()
	case p.match(tokenizer.TokenLeftParen):
		if p.match(tokenizer.TokenRightParen) {
			return runtime.MakeNil()
		}

		head := &runtime.Pair{Car: p.expression(), Cdr: runtime.Nil}
		list := runtime.MakePair(head)
		curr := &head.Cdr

		for !p.match(tokenizer.TokenRightParen) {
			if p.match(tokenizer.TokenDot) {
				elem := p.expression()
				*curr = elem
				p.consume(tokenizer.TokenRightParen)
				break
			}

			elem := p.expression()
			next := &runtime.Pair{Car: elem, Cdr: runtime.Nil}
			*curr = runtime.MakePair(next)
			curr = &next.Cdr
		}
		return list
	default:
		panic(p.errorf("unexpected token: expected expression, got %v", p.previous.Type))
	}
}

func (p *Parser) advance() {
	p.previous = p.current
	p.current = p.tokenizer.NextToken()
	if p.current.Type == tokenizer.TokenErr {
		panic(p.errorf("%s", p.current.Val))
	}
}

func (p *Parser) errorf(format string, args ...any) error {
	message := fmt.Sprintf(format, args...)
	return &ParseError{p.current.Line, message}
}

func (p *Parser) match(expected tokenizer.TokenType) bool {
	if p.current.Type != expected {
		return false
	}
	p.advance()
	return true
}

func (p *Parser) consume(expected tokenizer.TokenType) {
	if !p.match(expected) {
		panic(p.errorf("unexpected token: expected %v, got %v", expected, p.current.Type))
	}
}

func (p *Parser) parseNumber() float64 {
	if num, err := strconv.ParseInt(p.previous.Val, 0, 0); err == nil {
		return float64(num)
	}

	if num, err := strconv.ParseFloat(p.previous.Val, 64); err == nil {
		return num
	}

	panic(p.errorf("illegal number syntax"))
}

func (p *Parser) backquote() runtime.Value {
	switch {
	case p.match(tokenizer.TokenLeftParen):
		return p.quotedList()
	case p.match(tokenizer.TokenComma):
		return p.expression()
	default:
		quoted := p.expression()
		return runtime.Cons(runtime.MakeAtom("quote"), runtime.Cons(quoted, runtime.MakeNil()))
	}
}

func (p *Parser) quotedList() runtime.Value {
	switch {
	case p.match(tokenizer.TokenRightParen):
		return runtime.MakeNil()
	default:
		return runtime.Cons(runtime.MakeAtom("append"), p.quotedListElements())
	}
}

func (p *Parser) quotedListElements() runtime.Value {
	switch {
	case p.match(tokenizer.TokenRightParen):
		return runtime.MakeNil()
	case p.match(tokenizer.TokenCommaAt):
		return runtime.Cons(p.expression(), p.quotedListElements())
	default:
		elem := p.backquote()
		list := runtime.Cons(runtime.MakeAtom("list"), runtime.Cons(elem, runtime.MakeNil()))
		return runtime.Cons(list, p.quotedListElements())
	}
}
