package compiler

import (
	"fmt"
	"strings"
	"unique"
	"unsafe"
)

type ValueType int

const (
	TypeNil ValueType = iota
	TypeNumber
	TypeAtom
	TypePair
	TypeUpvalue
	TypeFunction
	TypeClosure
)

type Atom unique.Handle[string]

func NewAtom(s string) Atom {
	return Atom(unique.Make(s))
}

func (a Atom) Value() string {
	return unique.Handle[string](a).Value()
}

type Value struct {
	typ  ValueType
	num  float64
	atom Atom
	ptr  unsafe.Pointer
}

// Constructors

func MakeNil() Value {
	return Value{}
}

func MakeNumber(n float64) Value {
	return Value{typ: TypeNumber, num: n}
}

func MakeAtom(s string) Value {
	return Value{typ: TypeAtom, atom: NewAtom(s)}
}

func MakePair(p *Pair) Value {
	return Value{typ: TypePair, ptr: unsafe.Pointer(p)}
}

func MakeFunction(f *Function) Value {
	return Value{typ: TypeFunction, ptr: unsafe.Pointer(f)}
}

func MakeClosure(c *Closure) Value {
	return Value{typ: TypeClosure, ptr: unsafe.Pointer(c)}
}

// Type checks

func (v Value) IsNil() bool    { return v.typ == TypeNil }
func (v Value) IsNumber() bool { return v.typ == TypeNumber }
func (v Value) IsAtom() bool   { return v.typ == TypeAtom }
func (v Value) IsPair() bool   { return v.typ == TypePair }

// Type casts

func (v Value) AsNumber() (float64, bool) {
	if v.typ != TypeNumber {
		return 0, false
	}
	return v.num, true
}

func (v Value) AsAtom() (Atom, bool) {
	if v.typ != TypeAtom {
		return EmptyAtom, false
	}
	return v.atom, true
}

func (v Value) AsPair() (*Pair, bool) {
	if v.typ != TypePair {
		return nil, false
	}
	return (*Pair)(v.ptr), true
}

func (v Value) AsUpvalue() (*Upvalue, bool) {
	if v.typ != TypeUpvalue {
		return nil, false
	}
	return (*Upvalue)(v.ptr), true
}

func (v Value) AsFunction() (*Function, bool) {
	if v.typ != TypeFunction {
		return nil, false
	}
	return (*Function)(v.ptr), true
}

func (v Value) AsClosure() (*Closure, bool) {
	if v.typ != TypeClosure {
		return nil, false
	}
	return (*Closure)(v.ptr), true
}

var (
	EmptyAtom = NewAtom("")
	Nil       = MakeNil()
	True      = MakeAtom("#t")
)

func Cons(car, cdr Value) Value {
	return MakePair(&Pair{car, cdr})
}

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
	Name         Atom
	Arity        int
	HasRestParam bool
	Chunk        Chunk
	Upvalues     []UpvalueSpec
}

type Closure struct {
	Function *Function
	Upvalues []*Upvalue
}

func (v Value) String() string {
	switch v.typ {
	case TypeNil:
		return "()"
	case TypeNumber:
		return fmt.Sprintf("%g", v.num)
	case TypeAtom:
		return v.atom.Value()
	case TypePair:
		return (*Pair)(v.ptr).String()
	case TypeClosure:
		c := (*Closure)(v.ptr)
		if c.Function.Name == EmptyAtom {
			return "<lambda closure>"
		}
		return fmt.Sprintf("<closure %s>", c.Function.Name.Value())
	default:
		return "<unknown>"
	}
}

func (p *Pair) String() string {
	var curr Value = MakePair(p)
	var sb strings.Builder
	for sb.WriteByte('('); ; sb.WriteByte(' ') {
		if cons, ok := curr.AsPair(); ok {
			sb.WriteString(cons.Car.String())
			curr = cons.Cdr
			if curr.IsNil() {
				break
			}
		}
		if !curr.IsPair() {
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
	if f.Name == EmptyAtom {
		return "<lambda>"
	}
	return fmt.Sprintf("<function %s>", f.Name.Value())
}

func (c *Closure) String() string {
	if c.Function.Name == EmptyAtom {
		return "<lambda closure>"
	}
	return fmt.Sprintf("<closure %s>", c.Function.Name.Value())
}

func (v Value) Equal(other Value) bool {
	if v.typ != other.typ {
		return false
	}
	switch v.typ {
	case TypeNil:
		return true
	case TypeNumber:
		return v.num == other.num
	case TypeAtom:
		return v.atom == other.atom
	case TypePair:
		p1 := (*Pair)(v.ptr)
		p2 := (*Pair)(other.ptr)
		return p1.Car.Equal(p2.Car) && p1.Cdr.Equal(p2.Cdr)
	default:
		// Pointer equality for functions, closures, etc.
		return v.ptr == other.ptr
	}
}
