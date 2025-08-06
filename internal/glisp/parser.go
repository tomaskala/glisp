package glisp

import (
	"fmt"
	"strconv"
	"strings"
)

type Parser struct {
	tokenizer  *tokenizer
	parenDepth int
}

func NewParser(name, source string) *Parser {
	return &Parser{tokenizer: newTokenizer(name, source)}
}

func (p *Parser) expect(expected tokenType) error {
	t := p.tokenizer.nextToken()
	if t.typ != expected {
		return NewParseError(fmt.Sprintf("Unexpected token, expected %v", expected), p.tokenizer.name, t)
	}
	return nil
}

func (p *Parser) NextExpr() (Expr, error) {
	t := p.tokenizer.nextToken()
	return p.parseExpr(t)
}

func (p *Parser) parseExpr(t token) (Expr, error) {
	switch t.typ {
	case tokenEOF:
		if p.parenDepth != 0 {
			return nil, NewParseError("Unexpected end of file", p.tokenizer.name, t)
		}
		return nil, EOFError{}
	case tokenErr:
		return nil, NewParseError(t.val, p.tokenizer.name, t)
	case tokenLeftParen:
		p.parenDepth++
		return p.parseList()
	case tokenRightParen:
		return nil, NewParseError("Unexpected right paren", p.tokenizer.name, t)
	case tokenDot:
		return nil, NewParseError("Unexpected dot", p.tokenizer.name, t)
	case tokenQuote:
		return p.parseQuote()
	case tokenNumber:
		return p.parseNumber(t)
	case tokenAtom:
		return &Atom{t.val}, nil
	default:
		return nil, NewParseError("Unrecognized token type", p.tokenizer.name, t)
	}
}

func (p *Parser) parseList() (Expr, error) {
	t := p.tokenizer.nextToken()
	if t.typ == tokenRightParen {
		p.parenDepth--
		return Nil, nil
	}
	if t.typ == tokenDot {
		cdr, err := p.NextExpr()
		if err != nil {
			return nil, err
		}
		if err := p.expect(tokenRightParen); err != nil {
			return nil, err
		}
		p.parenDepth--
		return cdr, nil
	}
	car, err := p.parseExpr(t)
	if err != nil {
		return nil, err
	}
	cdr, err := p.parseList()
	if err != nil {
		return nil, err
	}
	return &Cons{car, cdr}, nil
}

func (p *Parser) parseQuote() (*Cons, error) {
	quote := &Atom{"quote"}
	expr, err := p.NextExpr()
	if err != nil {
		return nil, err
	}
	return &Cons{car: quote, cdr: &Cons{car: expr, cdr: Nil}}, nil
}

// Logic taken from https://cs.opensource.google/go/go/+/master:src/text/template/parse/node.go.
func (p *Parser) parseNumber(t token) (*Number, error) {
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
				return nil, NewParseError("Integer overflow", p.tokenizer.name, t)
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
	return nil, NewParseError("Illegal number syntax", p.tokenizer.name, t)
}
