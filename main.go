package main

import (
	"fmt"
	"go-template-parser/lexer"
	// "go-template-parser/parser"
)

func main() {
	content :=  []byte(" {{-ddo$is da gest}} {{-}} }} {{-  /* jdkfdf. dfd /* */ -}} <p>{{- melimelo(Hello)darker -}} -- {{- dd}}, {{  \n  .Name \n \n}}</p>")
	content = append(content, []byte("<p>{{   $rita    \n:=\n.user.friends.favorite \r\t }} -- {{ print .user.friends.best.age }}</p>")...)
	content = append(content, []byte("\n {{ print \"dummy text\" | convert_to_int | isOkay }}")...)
	content = append(content, []byte("\n{{ print$name -- }}")...)
	content = append(content, []byte("\n{{ $dodo$malcore ")...)
	content = append(content, []byte("\n{{ $dodo$malcore }} ")...)
	// content = []byte("<p>Petterson, {{ $name : = dict \"little timmy\" 23 }} !</p>")
	// content = []byte("<p>Petterson, {{ $name := dict 'little timmy' 23 }} !</p>")

	tokens, failedTokens, errs := lexer.Tokenize(content)
	fmt.Println(lexer.PrettyFormater(failedTokens))
	fmt.Print("")
	_ = errs
	_ = failedTokens
	_ = tokens

	// rootNode := parser.Parse(tokens)
	// parser.SemanticalAnalisis(rootNode)
}

