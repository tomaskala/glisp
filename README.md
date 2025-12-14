# glisp

A LISP implementation in Go. This a single-pass bytecode compiler with a 
stack-based virtual machine.

## What's implemented

- REPL with a readline library
- Short-circuiting `if` and `cond` conditionals as well as `and` and `or` forms
- Macros (have to be defined and bound to a name before expansion, because we 
  are using a single-pass compiler)
- Quoting (`'`, `` ` ``, `,`, `,@`)
- Proper tail-call optimization
- [Standard library](internal/runtime/stdlib.ss) embedded into the executable
- Line number tracking for errors
- Disassembler for debugging

## What's missing

- Modules/imports
- Strings
- File I/O
- Error handling
- call/cc

## Data types

- Atoms (interned strings efficiently compared using identity)
- Number (64-bit floating points)
- Booleans (the empty list evaluates to false, everything else is truthy)
- Pairs (a pair of expressions, used to construct the usual linked list)

## Implemented LISP primitives

### Host primitives

The following either have a dedicated opcode, or are compiled into other forms.

- `quote` (including the short form `'`)
- `define` (global definition)
- `lambda` (anonymous function)
- `macro` (macro definition, must be bound to a name)
- `if` (short-circuiting if-then-else)
- `cond` (short-circuiting conditional expression)
- `and` (short-circuiting and expression)
- `or` (short-circuiting or expression)
- `set!` (set the value of a binding in place)
- `begin` (sequential execution returning the last expression)
- `apply` (apply a function to a list)

### Built-in functions

The following are implemented as Go functions in the runtime.

- `gensym` (generate a unique identifier)
- `eval` (evaluate an expression)
- `cons` (construct a pair)
- `car` get the first element of a pair)
- `cdr` (get the second element of a pair)
- `+` (variadic addition)
- `-` (variadic subtraction)
- `*` (variadic multiplication)
- `/` (variadic division)
- `int` (integer part of a number)
- `=` (number equality)
- `<` (number comparison)
- `<=` (number comparison)
- `>` (number comparison)
- `>=` (number comparison)
- `eq?` (atom equality)
- `atom?` (atom test)
- `nil?` (nil test)
- `pair?` (pair test)
- `set-car!` (set the car of a pair in place)
- `set-cdr!` (set the cdr of a pair in place)
- `display` (print an expression)
- `newline` (print a newline)

### Macros

- `defmacro` (shorthand for `(define <name> (macro <params> <body>))`)
- `defun` (shorthand for `(define <name> (lambda <params> (begin <body>...)))`)
- `let` (local bindings)
- `let*` (local bindings evaluated one by one)
- `letrec` (local bindings permitting (mutual) recursion)
- `when` (conditionally evaluate actions without an else clause)
- `unless` (inverse of when)
- `->` (thread-first)
- `->>` (thread-last)
- `as->` (thread with a name binding)
- `some->` (-> short-circuiting on nil)
- `some->>` (->> short-circuiting on nil)
- `case` (pattern matching)
