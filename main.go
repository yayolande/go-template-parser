package main

import (
	"fmt"
	"go-template-parser/lexer"
	// "go-template-parser/parser"
)

func main() {
	content :=  []byte("<p>Hello, {{  \n  .Name \n \n}}</p>")
	content = append(content, []byte("<p>{{   $rita    \n:=\n.user.friends.favorite \r\t }} -- {{ print .user.friends.best.age }}</p>")...)
	content = append(content, []byte("\n {{ print \"dummy text\" | convert_to_int | isOkay }}")...)
	// content = []byte("<p>Petterson, {{ $name : = dict \"little timmy\" 23 }} !</p>")
	// content = []byte("<p>Petterson, {{ $name := dict 'little timmy' 23 }} !</p>")

	// _ = lexer.Tokenize(content)
	tokens := lexer.Tokenize(content)
	fmt.Println(lexer.PrettyTokenFormater(tokens))

	// str := lexer.PrettyTokenFormater(tokens)
	// fmt.Printf("%s", str)

	/*
	rootNode := parser.Parse(tokens)
	parser.SemanticalAnalisis(rootNode)
	*/
}

