package main

import (
	"errors"
	"flag"
	"fmt"
	"os"

	"tomaskala.com/glisp/internal/glisp"
)

const (
	exitSuccess    = 0
	exitIoError    = 1
	exitParseError = 2
	exitEvalError  = 3
)

func runRepl() int {
	return 0
}

func runScript(path string) int {
	source, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening %s: %v", path, err)
		return exitIoError
	}
	global := glisp.NewEnv(nil)
	parser := glisp.NewParser(path, string(source))
	for {
		expr, err := parser.NextExpr()
		var lispError glisp.LispError
		if errors.As(err, &lispError) && lispError.Type == glisp.ErrorEOF {
			return exitSuccess
		}
		if err != nil {
			fmt.Fprintf(os.Stderr, "%v\n", err)
			return exitParseError
		}
		_, err = expr.Eval(global)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%v\n", err)
			return exitEvalError
		}

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
