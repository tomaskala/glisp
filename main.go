package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"os"

	"tomaskala.com/glisp/internal/glisp"

	"github.com/chzyer/readline"
)

const (
	exitSuccess      = 0
	exitIoError      = 1
	exitParseError   = 2
	exitEvalError    = 3
	exitGeneralError = 4
)

func eval(env *glisp.Env, parser *glisp.Parser) (glisp.Expr, error) {
	var result glisp.Expr
	for {
		expr, err := parser.NextExpr()
		if err != nil {
			// We will eventually hit this case because the parser will return an EOF error.
			return result, err
		}
		result, err = expr.Eval(env)
		if err != nil {
			return result, err
		}
	}
}

func runRepl() int {
	completer := readline.NewPrefixCompleter()
	rl, err := readline.NewEx(&readline.Config{
		Prompt:       "λ ",
		AutoComplete: completer,
	})
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening a REPL: %v", err)
		return exitIoError
	}
	defer rl.Close()
	global := glisp.NewEnv(nil)
	for {
		line, err := rl.Readline()
		if err == readline.ErrInterrupt {
			if len(line) == 0 {
				break
			}
			continue
		} else if err == io.EOF {
			break
		} else if err != nil {
			fmt.Fprintf(os.Stderr, "%v\n", err)
			return exitIoError
		}
		parser := glisp.NewParser("REPL", line)
		expr, err := eval(global, parser)
		var lispError glisp.LispError
		if errors.As(err, &lispError) && lispError.Type == glisp.ErrorEOF {
			fmt.Println(expr)
		} else {
			fmt.Printf("%v\n", err)
		}
	}
	return exitSuccess
}

func runScript(path string) int {
	source, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening %s: %v", path, err)
		return exitIoError
	}
	global := glisp.NewEnv(nil)
	parser := glisp.NewParser(path, string(source))
	_, err = eval(global, parser)
	var lispError glisp.LispError
	if errors.As(err, &lispError) {
		switch lispError.Type {
		case glisp.ErrorEOF:
			return exitSuccess
		case glisp.ErrorParse:
			fmt.Fprintf(os.Stderr, "%v\n", lispError)
			return exitParseError
		case glisp.ErrorEval:
			fmt.Fprintf(os.Stderr, "%v\n", lispError)
			return exitEvalError
		default:
			fmt.Fprintf(os.Stderr, "Unrecognized lisp error: %v\n", lispError)
			return exitGeneralError
		}
	} else {
		fmt.Fprintf(os.Stderr, "%v\n", err)
		return exitGeneralError
	}
}

func main() {
	flag.Parse()
	args := flag.Args()
	if len(args) == 0 {
		os.Exit(runRepl())
	}
	os.Exit(runScript(args[0]))
}
