package glisp

type Builtin struct {
	name string
	fun  func(Expr, *Env) (Expr, error)
}

func (b *Builtin) String() string {
	return b.name
}

func (b *Builtin) Equal(o Expr) bool {
	if o, ok := o.(*Builtin); ok {
		return b.name == o.name
	}
	return false
}

func (b *Builtin) Eval(env *Env) (Expr, error) {
	return b, nil
}
