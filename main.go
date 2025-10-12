package main

import (
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"os"
	"runtime"
	"runtime/pprof"
	"strings"

	"github.com/chzyer/readline"
	"tomaskala.com/glisp/internal/compiler"
	"tomaskala.com/glisp/internal/vm"
)

const (
	exitSuccess      = 0
	exitIoError      = 1
	exitCompileError = 2
	exitRuntimeError = 3

	prompt = "λ "
)

var (
	disassemble = flag.Bool("disassemble", false, "disassemble each expression")
	cpuprofile  = flag.String("cpuprofile", "", "write cpu profile to `file`")
	memprofile  = flag.String("memprofile", "", "write memory profile to `file`")
)

func balancedParentheses(lines []string) bool {
	var stack []rune
	for _, line := range lines {
		for _, b := range line {
			switch b {
			case '(':
				stack = append(stack, b)
			case ')':
				if len(stack) == 0 {
					return false
				}
				top := stack[len(stack)-1]
				if top != '(' {
					return false
				}
				stack = stack[:len(stack)-1]
			}
		}
	}
	return len(stack) == 0
}

func runRepl() int {
	completer := readline.NewPrefixCompleter()
	rl, err := readline.NewEx(&readline.Config{
		Prompt:       prompt,
		AutoComplete: completer,
	})
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening a REPL: %v", err)
		return exitIoError
	}
	defer rl.Close()

	evaluator := vm.NewVM()
	var lines []string
	reset := func() {
		lines = lines[:0]
		rl.SetPrompt(prompt)
	}

	for {
		line, err := rl.Readline()
		if errors.Is(err, readline.ErrInterrupt) {
			if len(line) == 0 {
				if len(lines) == 0 {
					break
				}
				reset()
			}
			continue
		} else if errors.Is(err, io.EOF) {
			break
		} else if err != nil {
			fmt.Fprintf(os.Stderr, "%v\n", err)
			return exitIoError
		}

		lines = append(lines, line)
		if !balancedParentheses(lines) {
			rl.SetPrompt("> ")
			continue
		}

		compiledProgram, err := compiler.Compile("REPL", strings.Join(lines, " "))
		reset()
		if err != nil {
			fmt.Fprintf(rl.Stderr(), "Compile error: %v\n", err)
			continue
		}

		if *disassemble {
			compiler.Disassemble(compiledProgram, rl.Stderr())
		}

		result, err := evaluator.Run(compiledProgram)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Runtime error: %v\n", err)
			continue
		}
		fmt.Println(result)
	}

	return exitSuccess
}

func runScript(path string) int {
	source, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error opening %s: %v", path, err)
		return exitIoError
	}

	compiledProgram, err := compiler.Compile(path, string(source))
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return exitCompileError
	}

	if *disassemble {
		compiler.Disassemble(compiledProgram, os.Stderr)
	}

	evaluator := vm.NewVM()
	_, err = evaluator.Run(compiledProgram)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return exitRuntimeError
	}

	return exitSuccess
}

func run() int {
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Print("could not create CPU profile: ", err)
			return exitIoError
		}
		defer f.Close()
		if err := pprof.StartCPUProfile(f); err != nil {
			log.Print("could not start CPU profile: ", err)
			return exitIoError
		}
		defer pprof.StopCPUProfile()
	}

	args := flag.Args()
	var status int
	if len(args) == 0 {
		status = runRepl()
	} else {
		status = runScript(args[0])
	}

	if *memprofile != "" {
		f, err := os.Create(*memprofile)
		if err != nil {
			log.Print("could not create memory profile: ", err)
			return exitIoError
		}
		defer f.Close()
		runtime.GC()
		if err := pprof.Lookup("allocs").WriteTo(f, 0); err != nil {
			log.Print("could not write memory profile: ", err)
			return exitIoError
		}
	}

	return status
}

func main() {
	flag.Parse()
	os.Exit(run())
}
