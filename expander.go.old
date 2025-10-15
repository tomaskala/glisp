package expander

import (
	"fmt"
	"slices"

	"tomaskala.com/glisp/internal/ast"
)

type expander struct{}

func Expand(program *ast.Program) ast.Node {
	e := &expander{}
	return ast.Transform(e, program)
}

func (v *expander) Visit(node ast.Node) (ast.Node, bool) {
	switch n := node.(type) {
	case *ast.Let:
		switch n.Kind {
		case ast.LetPlain:
			return transformLet(n), true
		case ast.LetStar:
			return transformLetStar(n), true
		case ast.LetRec:
			return transformLetRec(n), true
		default:
			panic(fmt.Sprintf("Unrecognized let kind: %v", n.Kind))
		}
	default:
		return node, true
	}
}

// The following let expression:
//
//	(let ((k1 v1) (k2 v2) ... (kN vN)) body)
//
// is equivalent to the following lambda invocation:
//
//	((lambda (k1 k2 ... kN) body) v1 v2 ... vN)
func transformLet(let *ast.Let) *ast.Call {
	params := make([]string, len(let.Bindings))
	args := make([]ast.Node, len(let.Bindings))
	for i, binding := range let.Bindings {
		params[i] = binding.Name
		args[i] = binding.Value
	}
	function := &ast.Function{
		Name:   "let",
		Params: params,
		Body:   let.Body,
		Tok:    ast.Token(let),
	}
	return &ast.Call{Func: function, Args: args, Tok: ast.Token(let)}
}

// The following let* expression:
//
//	(let* ((k1 v1) (k2 v2) ... (kN vN)) body)
//
// is equivalent to the following lambda invocation:
//
//	((lambda (k1) ((lambda (k2) ... ((lambda (kN) body) vN) ... v2) v1))
func transformLetStar(let *ast.Let) ast.Node {
	node := let.Body
	for _, binding := range slices.Backward(let.Bindings) {
		function := &ast.Function{
			Name:   "let*",
			Params: []string{binding.Name},
			Body:   node,
			Tok:    ast.Token(let),
		}
		node = &ast.Call{
			Func: function,
			Args: []ast.Node{binding.Value},
			Tok:  ast.Token(let),
		}
	}
	return node
}

// The following letrec expression:
//
//	(letrec ((k1 v1) (k2 v2) ... (kN vN)) body)
//
// is equivalent to the following lambda invocation:
//
//	((lambda (k1 k2 ... kN)
//	  (begin
//	    (set! k1 v1)
//	    (set! k2 v2)
//	    ...
//	    (set! kN vN)
//	    body)) () () ... ())
func transformLetRec(let *ast.Let) *ast.Call {
	params := make([]string, len(let.Bindings))
	args := make([]ast.Node, len(let.Bindings))
	setExprs := make([]ast.Node, len(let.Bindings))
	for i, binding := range let.Bindings {
		params[i] = binding.Name
		args[i] = &ast.Nil{Tok: ast.Token(binding.Value)}
		setExprs[i] = &ast.Set{
			Variable: binding.Name,
			Value:    binding.Value,
			Tok:      ast.Token(binding.Value),
		}
	}
	begin := &ast.Begin{
		Exprs: setExprs,
		Tail:  let.Body,
		Tok:   ast.Token(let),
	}
	function := &ast.Function{
		Name:   "letrec",
		Params: params,
		Body:   begin,
		Tok:    ast.Token(let),
	}
	return &ast.Call{Func: function, Args: args, Tok: ast.Token(let)}
}
