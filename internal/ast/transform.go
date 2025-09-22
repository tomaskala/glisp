package ast

import "fmt"

// Visitor implements the visitor pattern over the AST.
type Visitor interface {
	// Visit is invoked for each node encountered by Walk.
	// It returns a replacement node (can be the same node) and a boolean indicating
	// whether the current node's children should be traversed after the replacement.
	Visit(Node) (Node, bool)
}

func Transform(v Visitor, node Node) Node {
	if node == nil {
		return nil
	}
	newNode, keepGoing := v.Visit(node)
	if !keepGoing {
		return newNode
	}
	return transformChildren(v, newNode)
}

func transformNodeList(v Visitor, nodes []Node) []Node {
	newNodes := make([]Node, len(nodes))
	for i, node := range nodes {
		newNodes[i] = Transform(v, node)
	}
	return newNodes
}

func transformChildren(v Visitor, node Node) Node {
	switch n := node.(type) {
	case *Program:
		return &Program{transformNodeList(v, n.Exprs), n.Tok}
	case *Nil, *Atom, *Number: // Leaf nodes.
		return n
	case *Quote:
		return &Quote{Transform(v, n.Value), n.Tok}
	case *Call:
		return &Call{Transform(v, n.Func), transformNodeList(v, n.Args), n.Tok}
	case *Function:
		return &Function{
			n.Name,
			n.Params,
			n.RestParam,
			Transform(v, n.Body),
			n.Tok,
		}
	case *Define:
		return &Define{n.Name, Transform(v, n.Value), n.Tok}
	case *Let:
		newBindings := make([]Binding, len(n.Bindings))
		for i, binding := range n.Bindings {
			newBindings[i] = Binding{
				binding.Name,
				Transform(v, binding.Value),
			}
		}
		return &Let{n.Kind, newBindings, Transform(v, n.Body), n.Tok}
	case *If:
		return &If{
			Transform(v, n.Cond),
			Transform(v, n.Then),
			Transform(v, n.Else),
			n.Tok,
		}
	case *Cond:
		newClauses := make([]CondClause, len(n.Clauses))
		for i, clause := range n.Clauses {
			newClauses[i] = CondClause{
				Transform(v, clause.Cond),
				Transform(v, clause.Value),
			}
		}
		return &Cond{newClauses, n.Tok}
	case *And:
		return &And{transformNodeList(v, n.Exprs), n.Tok}
	case *Or:
		return &Or{transformNodeList(v, n.Exprs), n.Tok}
	case *Set:
		return &Set{n.Variable, Transform(v, n.Value), n.Tok}
	case *Begin:
		return &Begin{transformNodeList(v, n.Exprs), Transform(v, n.Tail), n.Tok}
	default:
		panic(fmt.Sprintf("ast.Walk: Unexpected node type %T", n))
	}
}
