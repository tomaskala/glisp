package main

import (
	"flag"
	"fmt"
	"io"
	"os"

	"github.com/chzyer/readline"
)

const (
	exitSuccess      = 0
	exitIoError      = 1
	exitParseError   = 2
	exitRuntimeError = 3
	exitGeneralError = 4
)

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
		// TODO: Parse & evaluate the line & print the result.
	}
	return exitSuccess
}

func runScript(path string) int {
	_, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening %s: %v", path, err)
		return exitIoError
	}
	// TODO: Parse & Evaluate the source & print the errors if any.
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
