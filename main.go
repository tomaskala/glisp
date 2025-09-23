package main

import (
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/chzyer/readline"

	"tomaskala.com/glisp/internal/compiler"
	"tomaskala.com/glisp/internal/parser"
)

const (
	exitSuccess      = 0
	exitIoError      = 1
	exitParseError   = 2
	exitCompileError = 3
	exitRuntimeError = 4
	exitGeneralError = 5
)

var disassemble bool

func init() {
	flag.BoolVar(&disassemble, "disassemble", false, "Whether to disassemble each expression")
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
		program, err := parser.Parse("REPL", line)
		if err != nil {
			fmt.Fprintf(rl.Stderr(), "Parse error: %v\n", err)
			continue
		}
		compiledProgram, err := compiler.Compile("REPL", program)
		if err != nil {
			fmt.Fprintf(rl.Stderr(), "Compile error: %v\n", err)
			continue
		}
		if disassemble {
			compiledProgram.Disassemble(rl.Stderr())
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
	program, err := parser.Parse(path, string(source))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return exitParseError
	}
	compiledProgram, err := compiler.Compile(path, program)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return exitCompileError
	}
	if disassemble {
		compiledProgram.Disassemble(os.Stderr)
	}
	return exitSuccess
}

func main() {
	flag.Parse()
	args := flag.Args()
	if len(args) == 0 {
		os.Exit(runRepl())
	}
	os.Exit(runScript(args[0]))
}
