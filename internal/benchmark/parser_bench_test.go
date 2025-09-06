package benchmark

import (
	"strings"
	"testing"

	"tomaskala.com/glisp/internal/parser"
)

func BenchmarkParseLargeProgram(b *testing.B) {
	for b.Loop() {
		_, err := parser.Parse("bench", largeLispProgram)
		if err != nil {
			b.Fatal(err)
		}
	}
}

func BenchmarkParseDeepNesting(b *testing.B) {
	depth := 50
	source := strings.Repeat("(f ", depth) + "x" + strings.Repeat(")", depth)

	for b.Loop() {
		_, err := parser.Parse("bench", source)
		if err != nil {
			b.Fatal(err)
		}
	}
}
