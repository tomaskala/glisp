package parser

import (
	"fmt"
	"strconv"
	"strings"

	"tomaskala.com/glisp/internal/ast"
	"tomaskala.com/glisp/internal/tokenizer"
)

type ParseError struct {
	Position tokenizer.Position
	Message  string
}

func (e *ParseError) Error() string {
	return fmt.Sprintf("parse error at %d:%d: %s", e.Position.Line, e.Position.Column, e.Message)
}

type Parser struct {
	name       string               // Name of the source being tokenized, used for error reporting.
	defineName string               // Stored from the latest encountered "define", spliced into lambdas.
	tok        tokenizer.Token      // The last read token.
	tokenizer  *tokenizer.Tokenizer // The underlying tokenizer.
}

func Parse(name, source string) (prog *ast.Program, err error) {
	defer func() {
		if r := recover(); r != nil {
			err = r.(*ParseError)
		}
	}()
	tokenizer := tokenizer.NewTokenizer(source)
	parser := &Parser{name: name, tokenizer: tokenizer}
	parser.next() // Initialize the first token.
	prog = parser.parseProgram()
	return prog, err
}

func (p *Parser) next() {
	p.tok = p.tokenizer.NextToken()
	if p.tok.Type == tokenizer.TokenErr {
		panic(p.errorf("%s", p.tok.Val))
	}
}

func (p *Parser) errorf(format string, args ...any) error {
	message := fmt.Sprintf(format, args...)
	return &ParseError{p.tok.Pos, message}
}

func (p *Parser) expect(expected tokenizer.TokenType) {
	if p.tok.Type != expected {
		panic(p.errorf("Unexpected token: expected %v, got %v", expected, p.tok.Type))
	}
}

func (p *Parser) consume(expected tokenizer.TokenType) {
	p.next()
	p.expect(expected)
}

// Program:
//
// expr*
func (p *Parser) parseProgram() *ast.Program {
	prog := &ast.Program{}
	for p.tok.Type != tokenizer.TokenEOF {
		expr := p.parseExpr()
		prog.Exprs = append(prog.Exprs, expr)
		p.next()
	}
	prog.Tok = p.tok
	return prog
}

// Expr:
//
// call | quote | number | atom
func (p *Parser) parseExpr() ast.Node {
	switch tok := p.tok; tok.Type {
	case tokenizer.TokenLeftParen:
		p.next()
		return p.parseCall()
	case tokenizer.TokenQuote:
		p.next()
		return p.parseQuote(tok)
	case tokenizer.TokenNumber:
		return p.parseNumber(tok)
	case tokenizer.TokenAtom:
		return &ast.Atom{Name: tok.Val, Tok: tok}
	default:
		panic(p.errorf("Unexpected token: expected expression, got %v", p.tok.Type))
	}
}

// Call:
//
// "(" ")" | "(" special-form ")" | "(" expr list ")"
//
// "(" is past
//
// Special forms are built in to the language and their parameters differ.
func (p *Parser) parseCall() ast.Node {
	switch tok := p.tok; tok.Val {
	case ")":
		return &ast.Nil{}
	case "quote":
		p.next()
		return p.parseLongQuote(tok)
	case "define":
		p.next()
		return p.parseDefine(tok)
	case "let":
		p.next()
		return p.parseLet(tok, ast.LetPlain)
	case "let*":
		p.next()
		return p.parseLet(tok, ast.LetStar)
	case "letrec":
		p.next()
		return p.parseLet(tok, ast.LetRec)
	case "lambda":
		p.next()
		return p.parseLambda(tok)
	case "if":
		p.next()
		return p.parseIf(tok)
	case "cond":
		p.next()
		return p.parseCond(tok)
	case "and":
		p.next()
		return p.parseAnd(tok)
	case "or":
		p.next()
		return p.parseOr(tok)
	case "set!":
		p.next()
		return p.parseSet(tok)
	case "begin":
		p.next()
		return p.parseBegin(tok)
	default:
		callee := p.parseExpr()
		p.next()
		args := p.parseList()
		return &ast.Call{Func: callee, Args: args, Tok: tok}
	}
}

// Quote:
//
// "(" "quote" expr ")"
//
// "(" is past
// "quote" is past
//
// Parses the long form (quote expr), as opposed to the shorthand 'expr.
func (p *Parser) parseLongQuote(tok tokenizer.Token) ast.Node {
	quotedExpr := p.parseExpr()
	p.consume(tokenizer.TokenRightParen)
	return &ast.Quote{Value: quotedExpr, Tok: tok}
}

// Define:
//
// "(" "define" atom expr ")"
//
// "(" is past
// "define" is past
func (p *Parser) parseDefine(tok tokenizer.Token) ast.Node {
	p.expect(tokenizer.TokenAtom)
	name := p.tok.Val
	p.defineName = name
	p.next()
	value := p.parseExpr()
	p.consume(tokenizer.TokenRightParen)
	return &ast.Define{Name: name, Value: value, Tok: tok}
}

// Let:
//
// "(" "let" "(" ("(" atom expr ")")* ")" expr ")"
// "(" "let*" "(" ("(" atom expr ")")* ")" expr ")"
// "(" "letrec" "(" ("(" atom expr ")")* ")" expr ")"
//
// "(" is past
// "let"/"let*"/"letrec" is past
func (p *Parser) parseLet(tok tokenizer.Token, kind ast.LetKind) ast.Node {
	var bindings []ast.Binding
	p.expect(tokenizer.TokenLeftParen)
	p.next()
	for p.tok.Type != tokenizer.TokenRightParen {
		p.expect(tokenizer.TokenLeftParen)
		p.consume(tokenizer.TokenAtom)
		name := p.tok.Val
		p.next()
		value := p.parseExpr()
		p.consume(tokenizer.TokenRightParen)
		p.next()
		bindings = append(bindings, ast.Binding{Name: name, Value: value})
	}
	p.next()
	body := p.parseExpr()
	p.consume(tokenizer.TokenRightParen)
	return &ast.Let{Kind: kind, Bindings: bindings, Body: body, Tok: tok}
}

// Lambda:
//
// "(" "lambda" (atom | "(" expr* ("." expr)? ")") expr ")"
//
// "(" is past
// "lambda" is past
func (p *Parser) parseLambda(tok tokenizer.Token) ast.Node {
	var params []string
	var dotParam string
	switch p.tok.Type {
	case tokenizer.TokenLeftParen:
		p.next()
		params, dotParam = p.parseStringList()
	case tokenizer.TokenAtom:
		dotParam = p.tok.Val
	default:
		panic(p.errorf("Unexpected lambda parameter: %v", p.tok.Type))
	}
	p.next()
	body := p.parseExpr()
	p.consume(tokenizer.TokenRightParen)
	return &ast.Function{Name: p.defineName, Params: params, RestParam: dotParam, Body: body, Tok: tok}
}

// If:
//
// "(" "if" expr expr expr ")"
//
// "(" is past
// "if" is past
func (p *Parser) parseIf(tok tokenizer.Token) ast.Node {
	cond := p.parseExpr()
	p.next()
	thenBranch := p.parseExpr()
	p.next()
	elseBranch := p.parseExpr()
	p.consume(tokenizer.TokenRightParen)
	return &ast.If{Cond: cond, Then: thenBranch, Else: elseBranch, Tok: tok}
}

// Cond:
//
// "(" "cond" ( "(" expr expr ")" )* ")"
//
// "(" is past
// "cond" is past
func (p *Parser) parseCond(tok tokenizer.Token) ast.Node {
	var clauses []ast.CondClause
	for p.tok.Type != tokenizer.TokenRightParen {
		p.expect(tokenizer.TokenLeftParen)
		p.next()
		cond := p.parseExpr()
		p.next()
		value := p.parseExpr()
		p.consume(tokenizer.TokenRightParen)
		p.next()
		clauses = append(clauses, ast.CondClause{Cond: cond, Value: value})
	}
	return &ast.Cond{Clauses: clauses, Tok: tok}
}

// And:
//
// "(" "and" expr* ")"
//
// "(" is past
// "and" is past
func (p *Parser) parseAnd(tok tokenizer.Token) ast.Node {
	exprs := p.parseList()
	return &ast.And{Exprs: exprs, Tok: tok}
}

// Or:
//
// "(" "or" expr* ")"
//
// "(" is past
// "or" is past
func (p *Parser) parseOr(tok tokenizer.Token) ast.Node {
	exprs := p.parseList()
	return &ast.Or{Exprs: exprs, Tok: tok}
}

// Set:
//
// "(" "set!" atom expr ")"
//
// "(" is past
// "set!" is past
func (p *Parser) parseSet(tok tokenizer.Token) ast.Node {
	p.expect(tokenizer.TokenAtom)
	id := p.tok.Val
	p.next()
	value := p.parseExpr()
	p.consume(tokenizer.TokenRightParen)
	return &ast.Set{Variable: id, Value: value, Tok: tok}
}

// Begin:
//
// "(" "begin" expr* expr ")"
//
// "(" is past
// "begin" is past
func (p *Parser) parseBegin(tok tokenizer.Token) ast.Node {
	exprs := p.parseList()
	if len(exprs) == 0 {
		panic(p.errorf("begin requires at least one expression"))
	}
	return &ast.Begin{Exprs: exprs[:len(exprs)-1], Tail: exprs[len(exprs)-1], Tok: tok}
}

// List:
//
// "(" expr* ")"
//
// "(" is past
func (p *Parser) parseList() []ast.Node {
	var args []ast.Node
	for p.tok.Type != tokenizer.TokenRightParen {
		expr := p.parseExpr()
		args = append(args, expr)
		p.next()
	}
	return args
}

// String list:
//
// "(" atom* ("." atom)? ")"
//
// "(" is past
//
// This is not a part of the Lisp grammar, but the function is used
// as a utility to parse a lambda function parameters.
func (p *Parser) parseStringList() ([]string, string) {
	var args []string
	var dotArg string
	for {
		if p.tok.Type == tokenizer.TokenRightParen {
			break
		}
		if p.tok.Type == tokenizer.TokenDot {
			p.consume(tokenizer.TokenAtom)
			dotArg = p.tok.Val
			p.consume(tokenizer.TokenRightParen)
			break
		}
		p.expect(tokenizer.TokenAtom)
		args = append(args, p.tok.Val)
		p.next()
	}
	return args, dotArg
}

// Quote:
//
// "'" expr
//
// "'" is past
func (p *Parser) parseQuote(tok tokenizer.Token) ast.Node {
	expr := p.parseExpr()
	return &ast.Quote{Value: expr, Tok: tok}
}

// Number:
//
// number literal
//
// Logic taken from https://cs.opensource.google/go/go/+/master:src/text/template/parse/node.go.
func (p *Parser) parseNumber(tok tokenizer.Token) ast.Node {
	isUint := false
	isInt := false
	isFloat := false
	var uintVal uint64
	var intVal int64
	var floatVal float64
	u, err := strconv.ParseUint(p.tok.Val, 0, 64) // will fail for -0; fixed below.
	if err == nil {
		isUint = true
		uintVal = u
	}
	i, err := strconv.ParseInt(p.tok.Val, 0, 64)
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
		f, err := strconv.ParseFloat(p.tok.Val, 64)
		if err == nil {
			// If we parsed it as a float but it looks like an integer,
			// it's a huge number too large to fit in an int. Reject it.
			if !strings.ContainsAny(p.tok.Val, ".eEpP") {
				panic(p.errorf("Integer overflow"))
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
		return &ast.Number{Value: float64(intVal), Tok: tok}
	}
	if isUint {
		return &ast.Number{Value: float64(uintVal), Tok: tok}
	}
	if isFloat {
		return &ast.Number{Value: floatVal, Tok: tok}
	}
	panic(p.errorf("Illegal number syntax"))
}
