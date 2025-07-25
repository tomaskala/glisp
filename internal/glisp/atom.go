package glisp

type Atom struct {
	name string
}

var True = &Atom{name: "#t"}

func (a *Atom) String() string {
	return a.name
}

func (a *Atom) Equal(o Expr) bool {
	if o, ok := o.(*Atom); ok {
		return a.name == o.name
	}
	return false
}

func (a *Atom) Eval(env *Env) (Expr, error) {
	if a == True {
		return True, nil
	}
	expr, err := env.Get(a)
	if err != nil {
		return Nil, err
	}
	return expr, nil
}
