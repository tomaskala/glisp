package runtime

import (
	"fmt"
	"strings"
	"unique"
	"unsafe"
)

type valueType int

const (
	typeNil valueType = iota
	typeNumber
	typeAtom
	typePair
	typeUpvalue
	typeFunction
	typeClosure
	typeBuiltin
)

type Macro struct {
	Name      Atom
	Params    []Atom
	RestParam Atom
	Body      Value
}

func NewMacro(name Atom, body Value) Macro {
	return Macro{Name: name, RestParam: EmptyAtom, Body: body}
}

type Program struct {
	Function *Function
}

type Atom unique.Handle[string]

func NewAtom(s string) Atom {
	return Atom(unique.Make(s))
}

func (a Atom) Value() string {
	return unique.Handle[string](a).Value()
}

type Value struct {
	typ  valueType
	num  float64
	atom Atom
	ptr  unsafe.Pointer
}

// Constructors

func MakeNil() Value {
	return Value{}
}

func MakeNumber(n float64) Value {
	return Value{typ: typeNumber, num: n}
}

func MakeAtom(s string) Value {
	return Value{typ: typeAtom, atom: NewAtom(s)}
}

func MakePair(p *Pair) Value {
	return Value{typ: typePair, ptr: unsafe.Pointer(p)}
}

func MakeFunction(f *Function) Value {
	return Value{typ: typeFunction, ptr: unsafe.Pointer(f)}
}

func MakeClosure(c *Closure) Value {
	return Value{typ: typeClosure, ptr: unsafe.Pointer(c)}
}

func MakeBuiltin(b *Builtin) Value {
	return Value{typ: typeBuiltin, ptr: unsafe.Pointer(b)}
}

// Type checks

func (v Value) IsNil() bool     { return v.typ == typeNil }
func (v Value) IsNumber() bool  { return v.typ == typeNumber }
func (v Value) IsAtom() bool    { return v.typ == typeAtom }
func (v Value) IsPair() bool    { return v.typ == typePair }
func (v Value) IsClosure() bool { return v.typ == typeClosure }
func (v Value) IsBuiltin() bool { return v.typ == typeBuiltin }

// Type casts

func (v Value) AsNumber() float64 {
	return v.num
}

func (v Value) AsAtom() Atom {
	return v.atom
}

func (v Value) AsPair() *Pair {
	return (*Pair)(v.ptr)
}

func (v Value) AsUpvalue() *Upvalue {
	return (*Upvalue)(v.ptr)
}

func (v Value) AsFunction() *Function {
	return (*Function)(v.ptr)
}

func (v Value) AsClosure() *Closure {
	return (*Closure)(v.ptr)
}

func (v Value) AsBuiltin() *Builtin {
	return (*Builtin)(v.ptr)
}

var (
	EmptyAtom = NewAtom("")
	Nil       = MakeNil()
	True      = MakeAtom("#t")
)

func IsTruthy(v Value) bool {
	return v != Nil
}

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

type UpvalueSpec struct {
	Index   int  // Captured value index.
	IsLocal bool // true: capture parent's local at Index; false: capture parent's upvalue at Index.
}

type Closure struct {
	Function *Function
	Upvalues []*Upvalue
}

type Builtin struct {
	Name     Atom
	Arity    int
	Function func([]Value) (Value, error)
}

func (v Value) String() string {
	switch v.typ {
	case typeNil:
		return "()"
	case typeNumber:
		return fmt.Sprintf("%g", v.num)
	case typeAtom:
		return v.atom.Value()
	case typePair:
		return (*Pair)(v.ptr).String()
	case typeClosure:
		c := (*Closure)(v.ptr)
		if c.Function.Name == EmptyAtom {
			return "<lambda closure>"
		}
		return fmt.Sprintf("<closure %s>", c.Function.Name.Value())
	case typeBuiltin:
		b := (*Builtin)(v.ptr)
		return fmt.Sprintf("<builtin %s>", b.Name.Value())
	default:
		return "<unknown>"
	}
}

func (p *Pair) String() string {
	curr := MakePair(p)
	var sb strings.Builder
	for sb.WriteByte('('); ; sb.WriteByte(' ') {
		if curr.IsPair() {
			cons := curr.AsPair()
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

func (v Value) Equal(other Value) bool {
	if v.typ != other.typ {
		return false
	}
	switch v.typ {
	case typeNil:
		return true
	case typeNumber:
		return v.num == other.num
	case typeAtom:
		return v.atom == other.atom
	case typePair:
		p1 := (*Pair)(v.ptr)
		p2 := (*Pair)(other.ptr)
		return p1.Car.Equal(p2.Car) && p1.Cdr.Equal(p2.Cdr)
	default:
		// Pointer equality for functions, closures, etc.
		return v.ptr == other.ptr
	}
}
