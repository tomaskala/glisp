package glisp

type NilExpr struct{}

var Nil = &NilExpr{}

func (n *NilExpr) String() string {
	return "()"
}

func (n *NilExpr) Equal(o Expr) bool {
	return o == Nil
}

func (n *NilExpr) Eval(env *Env) (Expr, error) {
	return Nil, nil
}
