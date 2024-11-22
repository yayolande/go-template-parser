package main

import (
	"fmt"
	"log"
	"os"

	"github.com/yayolande/gota/lexer"
	"github.com/yayolande/gota/parser"
	"github.com/yayolande/gota"
)


func ddd(files ...int) {
	fmt.Println(files)
}


func main() {
	// 0. Init
	func () {
		file, err := os.Create("log_main.txt")
		if err != nil {
			fmt.Println("error while creating log file; ", err.Error())
			return
		}

		log.SetOutput(file)
		log.SetFlags(log.LstdFlags | log.Llongfile)
		log.SetPrefix("[LOG] ")
	}()

	// 1. Open all files under root directory
	// TODO: make so that the file extension scheme can be choosed from the root eg. passed through by variable instead of hard-coded
	// TODO: rename to 'sourceCodeInWorkspace'
	rootDir := "."
	fileExtension := ".go"

	filesContentInWorkspace := gota.OpenProjectFiles(rootDir, fileExtension)
	parsedFilesInWorkspace, _ := gota.ParseFilesInWorkspace(filesContentInWorkspace)
	_ = gota.DefinitionAnalisisWithinWorkspace(parsedFilesInWorkspace)

	// TODO: error coming from 'parseFilesInWorkspace()' and 'definitionAnalisisWithinWorkspace()' 
	// should be combine and then sent to the lsp user


	// {{ define "my_fabulous_template" }} Yeah new template {{ end }}

	return
	// 3. Find all template definition
	for longFileName, parseTree := range parsedFilesInWorkspace {
		_ = longFileName
		_ = parseTree
		fmt.Println()
		fmt.Println()
		fmt.Println()

		fmt.Printf("filename: %s\n----------\n%v\n----------", longFileName, parseTree)
	}

	// 4. Make 'semantical anylisi v1' aka 'definitionAnalisis()' using template def found earlier

	// 5. Make 'typeAnalysis()'

	return
	content :=  []byte(" {{- ddo $is da gest}} {{-}} }} {{- /* jdkfdf. dfd /* */ -}} <p>{{- melimelo(Hello)darker -}} -- {{- dd}}, {{  \n  .Name \n \n}}</p>")
	content = append(content, []byte("<p>{{   $rita    \n:=\n.user.friends.favorite \r\t }} -- {{ print .user.friends.best.age }}</p>")...)
	content = append(content, []byte("\n {{ print \"dummy text\" 23 | convert_to_int | isOkay }}")...)
	content = append(content, []byte("\n{{ print$name -- }}")...)
	content = append(content, []byte("\n{{ $dodo$malcore ")...)
	content = append(content, []byte("\n{{ $dodo$malcore }} ")...)
	// content = []byte("<p>Petterson, {{ $name : = dict \"little timmy\" 23 }} !</p>")
	// content = []byte("<p>Petterson, {{ $name := dict \"little timmy\" 23 }} !</p>")
	content = []byte("{{$is_user=isNotNil .user}}\npette {{if $is_user}} name : {{.user.name }} {{end}}")
	// content = append(content, []byte("{{else if $missota := dict . | printf \"%q\" .user}} {{ .}} ")...)
	// content = append(content, []byte("{{else if $steveen := \"name\" | convert | printf \"%t\" 23.0}}{{ else }} {{ .user.friend.name }} {{ end }} !")...)
	content = append(content, []byte("\nd {{ range $id, $el := dict .players}} \n {{ else }} djd {{ end}}")...)
	content = append(content, []byte("\n{{define \"lorepsum\"}} my name is {{.name}} with age {{.age}}\n {{end}}")...)
	content = append(content, []byte("\n{{block \"book_4\" $karma := \"lost family\"}}your karma is to {{.}}{{end}}")...)
	content = append(content, []byte("\n{{with $is_user}} are you a user {{.}} {{else with .user}} username: {{.name}}, age: {{.age}} {{end}}")...)

	// TODO: add 'Range' to root group node; differentiate between variable and function error
	// TODO: are 'LexerError' and 'ParseError' needed ? Aren't they the same ? Not sure for now
	// TODO: remove 'multiExpressionNode' in favor of just 'ExpressionNode'

	// TODO: 'lexer.tokenizeLine()' should only one error, not multiple
	// in order to send the first useful error to user

	// TODO: add 'definitionAnalisis' for template definition and template use,
	// so that you cannot use a template without initializing it, 
	// only 'define' and 'template' will be needed to be checked
	// I recommend using a structure containing all definitions for 'definitionAnalisis()'
	// (global & local var, function definition, template definition)

	tokens, failedTokens, errs := lexer.Tokenize(content)

	// fmt.Println(lexer.PrettyFormater(tokens))
	// fmt.Println(lexer.PrettyFormater(failedTokens))
	fmt.Print("")
	_ = errs
	_ = failedTokens
	_ = tokens

	rootNode, pErrs := parser.Parse(tokens)
	_ = rootNode
	_ = pErrs
	// fmt.Println(rootNode)
	if pErrs != nil {
		fmt.Println(pErrs)
		return
	} else {
		// fmt.Println(rootNode)
	}
	// fmt.Println(rootNode)
	// fmt.Println(pErrs)
	// fmt.Println(parser.PrettyAstNodeFormater(rootNode))
	// fmt.Println(lexer.PrettyFormater(errs))
	// fmt.Println(rootNode)

	// fmt.Println(rootNode)
	// fmt.Println(parser.PrettyFormater(pErrs))

	// semanticErr := parser.SemanticalAnalisis(rootNode)
	// fmt.Println(lexer.PrettyFormater(semanticErr))
}

