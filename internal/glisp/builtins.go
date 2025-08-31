package glisp

import "fmt"

func builtinEval(arg Expr, e *Evaluator) (Expr, error) {
	val, err := e.evalList(arg)
	if err != nil {
		return Nil, err
	}
	cons, ok := val.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'eval' expects one argument")
	}
	if cons.cdr != Nil {
		return Nil, e.runtimeError("'eval' expects one argument")
	}
	return e.Eval(cons.car)
}

func builtinQuote(arg Expr, e *Evaluator) (Expr, error) {
	cons, ok := arg.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'quote' expects arguments")
	}
	return cons.car, nil
}

func builtinCons(arg Expr, e *Evaluator) (Expr, error) {
	val, err := e.evalList(arg)
	if err != nil {
		return Nil, err
	}
	cons, ok := val.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'cons' expects two arguments")
	}
	car := cons.car
	cons, ok = cons.cdr.(*Cons)
	if !ok || cons.cdr != Nil {
		return Nil, e.runtimeError("'cons' expects two arguments")
	}
	cdr := cons.car
	return &Cons{car, cdr}, nil
}

func builtinCar(arg Expr, e *Evaluator) (Expr, error) {
	val, err := e.evalList(arg)
	if err != nil {
		return Nil, err
	}
	cons, ok := val.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'car' expects one argument")
	}
	if cons.cdr != Nil {
		return Nil, e.runtimeError("'car' expects one argument")
	}
	cons, ok = cons.car.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'car' expects a cons cell as its argument")
	}
	return cons.car, nil
}

func builtinCdr(arg Expr, e *Evaluator) (Expr, error) {
	val, err := e.evalList(arg)
	if err != nil {
		return Nil, err
	}
	cons, ok := val.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'cdr' expects one argument")
	}
	if cons.cdr != Nil {
		return Nil, e.runtimeError("'cdr' expects one argument")
	}
	cons, ok = cons.car.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'cdr' expects a cons cell as its argument")
	}
	return cons.cdr, nil
}

func builtinAdd(arg Expr, e *Evaluator) (Expr, error) {
	numbers, err := extractNumbers(arg, e)
	if err != nil {
		return Nil, err
	}
	if len(numbers) == 0 {
		return &Number{0}, nil
	}
	result := numbers[0].value
	for i := 1; i < len(numbers); i++ {
		result += numbers[i].value
	}
	return &Number{result}, nil
}

func builtinSub(arg Expr, e *Evaluator) (Expr, error) {
	numbers, err := extractNumbers(arg, e)
	if err != nil {
		return Nil, err
	}
	if len(numbers) == 0 {
		return Nil, e.runtimeError("'-' requires at least one argument")
	}
	if len(numbers) == 1 {
		// Special case: unary minus.
		return &Number{-numbers[0].value}, nil
	}
	result := numbers[0].value
	for i := 1; i < len(numbers); i++ {
		result -= numbers[i].value
	}
	return &Number{result}, nil
}

func builtinMul(arg Expr, e *Evaluator) (Expr, error) {
	numbers, err := extractNumbers(arg, e)
	if err != nil {
		return Nil, err
	}
	if len(numbers) == 0 {
		return &Number{1}, nil
	}
	result := numbers[0].value
	for i := 1; i < len(numbers); i++ {
		result *= numbers[i].value
	}
	return &Number{result}, nil
}

func builtinDiv(arg Expr, e *Evaluator) (Expr, error) {
	numbers, err := extractNumbers(arg, e)
	if err != nil {
		return Nil, err
	}
	if len(numbers) == 0 {
		return Nil, e.runtimeError("'/' requires at least one argument")
	}
	if len(numbers) == 1 {
		if numbers[0].value == 0 {
			return Nil, e.runtimeError("Division by zero")
		}
		// Special case: reciprocal value.
		return &Number{1.0 / numbers[0].value}, nil
	}
	result := numbers[0].value
	for i := 1; i < len(numbers); i++ {
		if numbers[i].value == 0 {
			return Nil, e.runtimeError("Division by zero")
		}
		result /= numbers[i].value
	}
	return &Number{result}, nil
}

func extractNumbers(arg Expr, e *Evaluator) ([]*Number, error) {
	var numbers []*Number
	val, err := e.evalList(arg)
	if err != nil {
		return nil, err
	}
	for {
		if curr, ok := val.(*Cons); ok {
			num, ok := curr.car.(*Number)
			if !ok {
				return nil, e.runtimeError("Expected a number, got %v", curr.car)
			}
			numbers = append(numbers, num)
			val = curr.cdr
		} else if val == Nil {
			break
		} else {
			return nil, e.runtimeError("Expected arguments")
		}
	}
	return numbers, nil
}

func builtinInt(arg Expr, e *Evaluator) (Expr, error) {
	val, err := e.evalList(arg)
	if err != nil {
		return Nil, err
	}
	cons, ok := val.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'int' expects one argument")
	}
	if cons.cdr != Nil {
		return Nil, e.runtimeError("'int' expects one argument")
	}
	num, ok := cons.car.(*Number)
	if !ok {
		return Nil, e.runtimeError("'int' expects a number as its argument")
	}
	return &Number{float64(int64(num.value))}, nil
}

func builtinLt(arg Expr, e *Evaluator) (Expr, error) {
	val, err := e.evalList(arg)
	if err != nil {
		return Nil, err
	}
	cons, ok := val.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'<' expects two arguments")
	}
	fst := cons.car
	cons, ok = cons.cdr.(*Cons)
	if !ok || cons.cdr != Nil {
		return Nil, e.runtimeError("'<' expects two arguments")
	}
	snd := cons.car
	num1, ok := fst.(*Number)
	if !ok {
		return Nil, e.runtimeError("'<' expects a number as its first argument")
	}
	num2, ok := snd.(*Number)
	if !ok {
		return Nil, e.runtimeError("'<' expects a number as its second argument")
	}
	if num1.value < num2.value {
		return True, nil
	}
	return Nil, nil
}

func builtinGt(arg Expr, e *Evaluator) (Expr, error) {
	val, err := e.evalList(arg)
	if err != nil {
		return Nil, err
	}
	cons, ok := val.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'>' expects two arguments")
	}
	fst := cons.car
	cons, ok = cons.cdr.(*Cons)
	if !ok || cons.cdr != Nil {
		return Nil, e.runtimeError("'>' expects two arguments")
	}
	snd := cons.car
	num1, ok := fst.(*Number)
	if !ok {
		return Nil, e.runtimeError("'>' expects a number as its first argument")
	}
	num2, ok := snd.(*Number)
	if !ok {
		return Nil, e.runtimeError("'>' expects a number as its second argument")
	}
	if num1.value > num2.value {
		return True, nil
	}
	return Nil, nil
}

func builtinEq(arg Expr, e *Evaluator) (Expr, error) {
	val, err := e.evalList(arg)
	if err != nil {
		return Nil, err
	}
	cons, ok := val.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'eq?' expects two arguments")
	}
	fst := cons.car
	cons, ok = cons.cdr.(*Cons)
	if !ok || cons.cdr != Nil {
		return Nil, e.runtimeError("'eq?' expects two arguments")
	}
	snd := cons.car
	if fst == snd || numbersEq(fst, snd) || atomsEq(fst, snd) {
		return True, nil
	}
	return Nil, nil
}

func numbersEq(arg1, arg2 Expr) bool {
	num1, ok1 := arg1.(*Number)
	num2, ok2 := arg2.(*Number)
	return ok1 && ok2 && num1.value == num2.value
}

func atomsEq(arg1, arg2 Expr) bool {
	atom1, ok1 := arg1.(*Atom)
	atom2, ok2 := arg2.(*Atom)
	return ok1 && ok2 && atom1.name == atom2.name
}

func builtinPair(arg Expr, e *Evaluator) (Expr, error) {
	val, err := e.evalList(arg)
	if err != nil {
		return Nil, err
	}
	cons, ok := val.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'pair?' expects one argument")
	}
	if cons.cdr != Nil {
		return Nil, e.runtimeError("'pair?' expects one argument")
	}
	if _, ok := cons.car.(*Cons); ok {
		return True, nil
	}
	return Nil, nil
}

func builtinOr(arg Expr, e *Evaluator) (Expr, error) {
	curr := arg
	var result Expr = Nil
	var err error
	for {
		if curr == Nil {
			break
		}
		cons, ok := curr.(*Cons)
		if !ok {
			return Nil, e.runtimeError("'or' expects arguments")
		}
		result, err = e.Eval(cons.car)
		if err != nil {
			return Nil, err
		}
		if result != Nil {
			break
		}
		curr = cons.cdr
	}
	return result, nil
}

func builtinAnd(arg Expr, e *Evaluator) (Expr, error) {
	curr := arg
	var result Expr = True
	var err error
	for {
		if curr == Nil {
			break
		}
		cons, ok := curr.(*Cons)
		if !ok {
			return Nil, e.runtimeError("'and' expects arguments")
		}
		result, err = e.Eval(cons.car)
		if err != nil {
			return Nil, err
		}
		if result == Nil {
			break
		}
		curr = cons.cdr
	}
	return result, nil
}

func builtinNot(arg Expr, e *Evaluator) (Expr, error) {
	val, err := e.evalList(arg)
	if err != nil {
		return Nil, err
	}
	cons, ok := val.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'not' expects one argument")
	}
	if cons.cdr != Nil {
		return Nil, e.runtimeError("'not' expects one argument")
	}
	if cons.car == Nil {
		return True, nil
	}
	return Nil, nil
}

func builtinCond(arg Expr, e *Evaluator) (Expr, error) {
	curr := arg
	var pair *Cons
	for {
		cons, ok := curr.(*Cons)
		if !ok {
			return Nil, e.runtimeError("'cond' expects that at least one condition evaluates to #t")
		}
		pair, ok = cons.car.(*Cons)
		if !ok {
			return Nil, e.runtimeError("'cond' expects two-element cons cells as its arguments")
		}
		val, err := e.Eval(pair.car)
		if err != nil {
			return Nil, err
		}
		if val != Nil {
			break
		}
		curr = cons.cdr
	}
	expr, ok := pair.cdr.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'cond' expects two-element cons cells as its arguments")
	}
	return e.Eval(expr.car)
}

func builtinIf(arg Expr, e *Evaluator) (Expr, error) {
	cons1, ok := arg.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'if' expects arguments")
	}
	cons2, ok := cons1.cdr.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'if' expects then branch")
	}
	cons3, ok := cons2.cdr.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'if' expects else branch")
	}
	if cons3.cdr != Nil {
		return Nil, e.runtimeError("'if' expects three arguments")
	}
	condition, err := e.Eval(cons1.car)
	if err != nil {
		return Nil, err
	}
	if condition == Nil {
		// Evaluate the 'else' branch.
		return e.Eval(cons3.car)
	}
	// Evaluate the 'then' branch.
	return e.Eval(cons2.car)
}

func builtinLeta(arg Expr, e *Evaluator) (Expr, error) {
	cons1, ok := arg.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'let*' expects arguments")
	}
	cons2, ok := cons1.cdr.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'let*' expects body")
	}
	if cons2.cdr != Nil {
		return Nil, e.runtimeError("'let*' expects two arguments")
	}
	bindings := cons1.car
	body := cons2.car
	e.pushFrame(Frame{&Closure{name: "let*"}, make(map[string]Expr)})
	defer e.popFrame()
	if err := bind(bindings, e); err != nil {
		return Nil, err
	}
	return e.Eval(body)
}

func bind(bindings Expr, e *Evaluator) error {
	curr := bindings
	for curr != Nil {
		cons, ok := curr.(*Cons)
		if !ok {
			return e.runtimeError("'let*' expects bindings")
		}
		binding, ok := cons.car.(*Cons)
		if !ok {
			return e.runtimeError("'let*' expects bindings to be cons cells")
		}
		atom, ok := binding.car.(*Atom)
		if !ok {
			return e.runtimeError("'let*' expects binding names to be atoms")
		}
		exprCons, ok := binding.cdr.(*Cons)
		if !ok {
			return e.runtimeError("'let*' expects bindings to have expressions")
		}
		if exprCons.cdr != Nil {
			return e.runtimeError("'let*' expects bindings to consists of two values")
		}
		expr, err := e.Eval(exprCons.car)
		if err != nil {
			return err
		}
		e.store(atom.name, expr)
		curr = cons.cdr
	}
	return nil
}

func builtinLambda(arg Expr, e *Evaluator) (Expr, error) {
	cons, ok := arg.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'lambda' expects arguments")
	}
	body, ok := cons.cdr.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'lambda' expects body")
	}
	captured := captureEnvironment(e)
	return &Closure{"", cons.car, body.car, captured}, nil
}

// This function is extremely simplified, because it captures the entire environment,
// instead of only those variables actually referenced inside the closure body.
func captureEnvironment(e *Evaluator) map[string]Expr {
	captured := make(map[string]Expr)
	// Skip the 0th frame containing the always accessible globals.
	for i := 1; i < len(e.frames); i++ {
		// The innermost frame wins.
		for name, value := range e.frames[i].symbols {
			captured[name] = value
		}
	}
	return captured
}

func builtinDefine(arg Expr, e *Evaluator) (Expr, error) {
	cons, ok := arg.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'define' expects arguments")
	}
	atom, ok := cons.car.(*Atom)
	if !ok {
		return Nil, e.runtimeError("'define' expects an atom as its first argument")
	}
	tail, ok := cons.cdr.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'define' expects a value")
	}
	value, err := e.Eval(tail.car)
	if err != nil {
		return Nil, err
	}
	if closure, ok := value.(*Closure); ok {
		// Copy the bound name to the closure for better error reporting.
		closure.name = atom.name
	}
	e.storeGlobal(atom.name, value)
	return atom, nil
}

func builtinDisplay(arg Expr, e *Evaluator) (Expr, error) {
	cons, ok := arg.(*Cons)
	if !ok {
		return Nil, e.runtimeError("'display' expects arguments")
	}
	val, err := e.Eval(cons.car)
	if err != nil {
		return Nil, err
	}
	fmt.Println(val)
	return Nil, nil
}

func LoadBuiltins() map[string]Expr {
	return map[string]Expr{
		"#t":      True,
		"eval":    &Builtin{"eval", builtinEval},
		"quote":   &Builtin{"quote", builtinQuote},
		"cons":    &Builtin{"cons", builtinCons},
		"car":     &Builtin{"car", builtinCar},
		"cdr":     &Builtin{"cdr", builtinCdr},
		"+":       &Builtin{"+", builtinAdd},
		"-":       &Builtin{"-", builtinSub},
		"*":       &Builtin{"*", builtinMul},
		"/":       &Builtin{"/", builtinDiv},
		"int":     &Builtin{"int", builtinInt},
		"<":       &Builtin{"<", builtinLt},
		">":       &Builtin{">", builtinGt},
		"eq?":     &Builtin{"eq?", builtinEq},
		"pair?":   &Builtin{"pair?", builtinPair},
		"or":      &Builtin{"or", builtinOr},
		"and":     &Builtin{"and", builtinAnd},
		"not":     &Builtin{"not", builtinNot},
		"cond":    &Builtin{"cond", builtinCond},
		"if":      &Builtin{"if", builtinIf},
		"let*":    &Builtin{"let*", builtinLeta},
		"lambda":  &Builtin{"lambda", builtinLambda},
		"define":  &Builtin{"define", builtinDefine},
		"display": &Builtin{"display", builtinDisplay},
	}
}
