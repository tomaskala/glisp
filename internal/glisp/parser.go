package glisp

import (
	"fmt"
	"io"
	"strconv"
	"strings"
)

type Parser struct {
	name      string     // Name of the source being tokenized, used for error reporting.
	tokenizer *tokenizer // The underlying tokenizer.
}

func NewParser(name, source string) *Parser {
	return &Parser{name: name, tokenizer: newTokenizer(source)}
}

func (p *Parser) expect(expected tokenType) error {
	t := p.tokenizer.nextToken()
	if t.typ != expected {
		return NewParseError(fmt.Sprintf("Unexpected token, expected %v", expected), p.name, t)
	}
	return nil
}

func (p *Parser) NextExpr() (Expr, error) {
	t := p.tokenizer.nextToken()
	return p.parseExpr(t)
}

// Expr:
//
// list | quote | number | atom
func (p *Parser) parseExpr(t token) (Expr, error) {
	switch t.typ {
	case tokenEOF:
		return Nil, io.EOF
	case tokenErr:
		return Nil, NewParseError(t.val, p.name, t)
	case tokenLeftParen:
		return p.parseList()
	case tokenQuote:
		return p.parseQuote()
	case tokenNumber:
		return p.parseNumber(t)
	case tokenAtom:
		return &Atom{t.val}, nil
	default:
		return Nil, NewParseError("Unexpected token", p.name, t)
	}
}

// List:
//
// "(" expr* ("." expr)? ")"
//
// "(" is past.
func (p *Parser) parseList() (Expr, error) {
	var list Expr = Nil
	curr := &list
	for {
		t := p.tokenizer.nextToken()
		switch t.typ {
		case tokenEOF:
			return Nil, NewParseError("Unexpected end of file", p.name, t)
		case tokenRightParen:
			return list, nil
		case tokenDot:
			expr, err := p.NextExpr()
			if err != nil {
				return Nil, err
			}
			if err := p.expect(tokenRightParen); err != nil {
				return Nil, err
			}
			*curr = expr
			return list, nil
		default:
			expr, err := p.parseExpr(t)
			if err != nil {
				return Nil, err
			}
			next := &Cons{expr, Nil}
			*curr = next
			curr = &next.cdr
		}
	}
}

// Quote:
//
// "'" expr
//
// "'" is past.
func (p *Parser) parseQuote() (Expr, error) {
	quote := &Atom{"quote"}
	expr, err := p.NextExpr()
	if err != nil {
		return Nil, err
	}
	return &Cons{quote, &Cons{expr, Nil}}, nil
}

// Number:
//
// number literal
//
// Logic taken from https://cs.opensource.google/go/go/+/master:src/text/template/parse/node.go.
func (p *Parser) parseNumber(t token) (Expr, error) {
	isUint := false
	isInt := false
	isFloat := false
	var uintVal uint64
	var intVal int64
	var floatVal float64
	u, err := strconv.ParseUint(t.val, 0, 64) // will fail for -0; fixed below.
	if err == nil {
		isUint = true
		uintVal = u
	}
	i, err := strconv.ParseInt(t.val, 0, 64)
	if err == nil {
		isInt = true
		intVal = i
		if i == 0 {
			isUint = true // in case of -0.
			uintVal = u
		}
	}
	// If an integer extraction succeeded, promote the float.
	if isInt {
		isFloat = true
		floatVal = float64(intVal)
	} else if isUint {
		isFloat = true
		floatVal = float64(uintVal)
	} else {
		f, err := strconv.ParseFloat(t.val, 64)
		if err == nil {
			// If we parsed it as a float but it looks like an integer,
			// it's a huge number too large to fit in an int. Reject it.
			if !strings.ContainsAny(t.val, ".eEpP") {
				return nil, NewParseError("Integer overflow", p.name, t)
			}
			isFloat = true
			floatVal = f
			// If a floating-point extraction succeeded, extract the int if needed.
			if !isInt && float64(int64(f)) == f {
				isInt = true
				intVal = int64(f)
			}
			if !isUint && float64(uint64(f)) == f {
				isUint = true
				uintVal = uint64(f)
			}
		}
	}
	if isInt {
		return &Number{float64(intVal)}, nil
	}
	if isUint {
		return &Number{float64(uintVal)}, nil
	}
	if isFloat {
		return &Number{floatVal}, nil
	}
	return nil, NewParseError("Illegal number syntax", p.name, t)
}
