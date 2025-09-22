package compiler

import (
	"fmt"
	"strings"
)

var True = Atom("#t")

func Cons(car, cdr Value) *Pair {
	return &Pair{car, cdr}
}

func Car(v Value) (Value, bool) {
	if cons, ok := v.(*Pair); ok {
		return cons.Car, true
	}
	return nil, false
}

func Cdr(v Value) (Value, bool) {
	if cons, ok := v.(*Pair); ok {
		return cons.Cdr, true
	}
	return nil, false
}

func IsTruthy(v Value) bool {
	return v != Null
}

type Value interface {
	String() string
	Equal(other Value) bool
}

type Number float64

type Atom string

type Nil struct{}

var Null = &Nil{}

type Pair struct {
	Car Value
	Cdr Value
}

type Upvalue struct {
	Index     int   // Index to the VM stack if IsClosed == false.
	IsClosed  bool  // true: ClosedVal holds the captured value; false: vm.stack[Index] holds the value.
	ClosedVal Value // Captured value if closed.
}

type Function struct {
	Name         string
	Arity        int
	HasRestParam bool
	Chunk        Chunk
	Upvalues     []UpvalueSpec
}

type Closure struct {
	Function *Function
	Upvalues []*Upvalue
}

type NativeFunction struct {
	Name  string
	Arity int
	Func  func([]Value) (Value, error)
}

func (n Number) String() string {
	return fmt.Sprintf("%g", n)

}
func (a Atom) String() string {
	return string(a)
}
func (Nil) String() string {
	return "()"
}
func (c *Pair) String() string {
	var curr Value = c
	var sb strings.Builder
	for sb.WriteByte('('); ; sb.WriteByte(' ') {
		if cons, ok := curr.(*Pair); ok {
			sb.WriteString(cons.Car.String())
			curr = cons.Cdr
			if _, ok := curr.(*Nil); ok {
				break
			}
		}
		if _, ok := curr.(*Pair); !ok {
			sb.WriteString(" . ")
			sb.WriteString(curr.String())
			break
		}
	}
	sb.WriteByte(')')
	return sb.String()
}
func (u *Upvalue) String() string {
	if u.IsClosed {
		return fmt.Sprintf("<closed upvalue: %s>", u.ClosedVal)
	}
	return fmt.Sprintf("<open upvalue: %d>", u.Index)
}
func (f *Function) String() string {
	if f.Name == "" {
		return "<lambda>"
	}
	return fmt.Sprintf("<function %s>", f.Name)
}
func (c *Closure) String() string {
	if c.Function.Name == "" {
		return "<lambda closure>"
	}
	return fmt.Sprintf("<closure %s>", c.Function.Name)
}
func (n *NativeFunction) String() string {
	return fmt.Sprintf("<native %s>", n.Name)
}

func (n Number) Equal(other Value) bool {
	n2, ok := other.(Number)
	if !ok {
		return false
	}
	return n == n2
}
func (a Atom) Equal(other Value) bool {
	a2, ok := other.(Atom)
	if !ok {
		return false
	}
	return a == a2
}
func (Nil) Equal(other Value) bool {
	_, ok := other.(*Nil)
	return ok
}
func (c *Pair) Equal(other Value) bool {
	c2, ok := other.(*Pair)
	if !ok {
		return false
	}
	return c.Car.Equal(c2.Car) && c.Cdr.Equal(c2.Cdr)
}
func (u *Upvalue) Equal(other Value) bool {
	u2, ok := other.(*Upvalue)
	if !ok {
		return false
	}
	return u.Index == u2.Index && u.IsClosed == u2.IsClosed && (u.ClosedVal == u2.ClosedVal || u.ClosedVal.Equal(u2.ClosedVal))
}
func (f *Function) Equal(other Value) bool {
	// Function is only equal to itself.
	return f == other
}
func (c *Closure) Equal(other Value) bool {
	// Closure is only equal to itself.
	return c == other
}
func (n *NativeFunction) Equal(other Value) bool {
	// Native function is only equal to itself.
	return n == other
}
