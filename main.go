package main

import (
	_ "embed"
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
	"tomaskala.com/glisp/internal/parser"
	glispruntime "tomaskala.com/glisp/internal/runtime"
	"tomaskala.com/glisp/internal/vm"
)

//go:embed stdlib.ss
var stdlib string

const (
	exitSuccess      = 0
	exitIoError      = 1
	exitRuntimeError = 2

	prompt = "λ "
)

var (
	disassemble = flag.Bool("disassemble", false, "disassemble each expression")
	cpuprofile  = flag.String("cpuprofile", "", "write cpu profile to `file`")
	memprofile  = flag.String("memprofile", "", "write memory profile to `file`")
	repl        = flag.Bool("repl", false, "open a REPL after loading a file")
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

func evaluate(evaluator *vm.VM, name, source string, out io.Writer) (glispruntime.Value, error) {
	p := parser.NewParser(name, source)
	c := compiler.NewCompiler(name, evaluator)
	result := glispruntime.MakeNil()

	for !p.AtEOF() {
		expr, err := p.Expression()
		if err != nil {
			return result, err
		}

		program, err := c.Compile(expr)
		if err != nil {
			return result, err
		}

		if *disassemble {
			compiler.Disassemble(program, out)
		}

		result, err = evaluator.Run(program)
		if err != nil {
			return result, err
		}
	}

	return result, nil
}

func runRepl(evaluator *vm.VM) int {
	rl, err := readline.New(prompt)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error opening REPL: %v", err)
		return exitIoError
	}
	defer rl.Close()

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

		source := strings.Join(lines, " ")
		reset()

		result, err := evaluate(evaluator, "REPL", source, rl.Stdout())
		if err != nil {
			fmt.Fprintln(rl.Stderr(), err)
			continue
		}

		fmt.Println(result)
	}

	return exitSuccess
}

func runScript(evaluator *vm.VM, path string) int {
	source, err := os.ReadFile(path)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error opening %s: %v", path, err)
		return exitIoError
	}

	_, err = evaluate(evaluator, path, string(source), os.Stdout)
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
	evaluator := vm.NewVM()

	_, err := evaluate(evaluator, "stdlib", stdlib, os.Stdout)
	if err != nil {
		fmt.Fprintf(os.Stderr, "error loading the standard library: %v", err)
	}

	if len(args) == 0 {
		status = runRepl(evaluator)
	} else {
		status = runScript(evaluator, args[0])
		if *repl {
			status = runRepl(evaluator)
		}
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
