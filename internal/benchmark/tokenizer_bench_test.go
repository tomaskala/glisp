package benchmark

import (
	"strings"
	"testing"

	"tomaskala.com/glisp/internal/tokenizer"
)

func BenchmarkTokenizeLargeProgram(b *testing.B) {
	for b.Loop() {
		var token tokenizer.Token
		t := tokenizer.NewTokenizer(largeLispProgram)
		for {
			token = t.NextToken()
			if token.Type == tokenizer.TokenEOF || token.Type == tokenizer.TokenErr {
				break
			}
		}
		if token.Type == tokenizer.TokenErr {
			b.Fatal(token.Val)
		}
	}
}

func BenchmarkTokenizeDeepNesting(b *testing.B) {
	depth := 100
	source := strings.Repeat("(", depth) + "x" + strings.Repeat(")", depth)

	for b.Loop() {
		var token tokenizer.Token
		t := tokenizer.NewTokenizer(source)
		for {
			token = t.NextToken()
			if token.Type == tokenizer.TokenEOF || token.Type == tokenizer.TokenErr {
				break
			}
		}
		if token.Type == tokenizer.TokenErr {
			b.Fatal(token.Val)
		}
	}
}
