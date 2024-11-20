package main

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"maps"
	"os"
	"path/filepath"
	"strings"

	"go-template-parser/lexer"
	"go-template-parser/parser"
)

// Recursively open files from 'rootDir'
func openProjectFiles(rootDir string) map[string][]byte {
	// TODO: Change the extension type later on
	// targetExtension := ".html"
	targetExtension := ".go"

	list, err := os.ReadDir(rootDir)
	if err != nil {
		panic("error while reading directory content: " + err.Error())
	}

	fileNamesToContent := make(map[string][]byte)

	for _, entry := range list {
		fileName := filepath.Join(rootDir, entry.Name())

		if entry.IsDir() {
			subDir := fileName
			subFiles := openProjectFiles(subDir)

			maps.Copy(fileNamesToContent, subFiles)
			continue
		}

		if ! strings.HasSuffix(fileName, targetExtension) {
			continue
		}

		file, err := os.Open(fileName)
		if err != nil {
			log.Println("unable to open file, ", err.Error())
			continue
		}

		fileContent, err := io.ReadAll(file)
		fileNamesToContent[fileName] = fileContent
	}

	if len(fileNamesToContent) == 0 {
		return nil
	}

	return fileNamesToContent
}

// TODO: properly handly error return later on (intended for lsp user)
func parseFilesInWorkspace(workspaceFiles map[string][]byte) (map[string]*parser.GroupStatementNode, error) {
	// 2. Parse the opened files --- parseFilesInWorkspace(workspace)
	parsedFilesInWorkspace := make(map[string]*parser.GroupStatementNode)

	for longFileName, content := range workspaceFiles {
		// TODO: change lexer.Tokenizer to unexported type
		// TODO; name return type for 'lexer.Tokenize()' so that it is easier to read the meaning of the return vallues
		tokens, failedTokens, tokenErr := lexer.Tokenize(content)

		if tokenErr != nil {
			// TODO: Do something in case of error, maybe sent it back to lsp user
			// fmt.Println("errors while 'lexing' <--> ", longFileName)
			// fmt.Println(lexer.PrettyFormater(failedTokens))
			// log.Println("file containing erroneous go-template code: ", longFileName)
			// fmt.Println()
			_ = failedTokens
		}

		if len(tokens) == 0 {
			continue
		}

		parseTree, parseError := parser.Parse(tokens)
		if parseError != nil {
			// TODO: Return the error to user
			// fmt.Println("errors while 'parsing' <--> ", longFileName)
			// fmt.Println(parser.PrettyFormater(parseError))
			// log.Println("file containing erroneous go-template code: ", longFileName)
			// fmt.Println()safvis bali
		}

		parsedFilesInWorkspace[longFileName] = parseTree
	}

	if len(parsedFilesInWorkspace) == 0 {
		return nil, nil
	}

	return parsedFilesInWorkspace, nil
}

// TODO: change return type. it should accurate represent error data that the user can work with (lsp)
func definitionAnalisisWithinWorkspace(parsedFilesInWorkspace map[string]*parser.GroupStatementNode) error {
	// 4. Definition Analysis (SemanticalAnalisis v1) (in all workspace files)
	// TODO: refactor this code to a function : definitionAnalisisWithinWorkspace(parsedFilesInWorkspace)
	var cloneParsedFilesInWorkspace map[string]*parser.GroupStatementNode

	var globalVariableDefinition, localVariableDefinition, functionDefinition parser.SymbolDefinition
	var workspaceTemplateDefinition parser.SymbolDefinition

	for longFileName, fileParseTree := range parsedFilesInWorkspace {
		cloneParsedFilesInWorkspace = maps.Clone(parsedFilesInWorkspace)
		delete(cloneParsedFilesInWorkspace, longFileName)

		workspaceTemplateDefinition = getWorkspaceTemplateDefinitionExcludingActiveFile(cloneParsedFilesInWorkspace)
		globalVariableDefinition = getBuiltinVariableDefinition()
		localVariableDefinition = parser.SymbolDefinition{}
		functionDefinition = getBuiltinFunctionDefinition()

		// TODO: put 'err' into a slice
		errs := fileParseTree.DefinitionAnalysis(globalVariableDefinition, localVariableDefinition, functionDefinition, workspaceTemplateDefinition)
		_ = errs
		fmt.Println(parser.PrettyFormater(errs))
	}

	return nil
}

// get template definition within the root scope only (no recursive traversal). NB: the root scope to operate must be 'non-nil'
func getRootTemplateDefinition(root *parser.GroupStatementNode) parser.SymbolDefinition {
	if root == nil {
		panic("cannot find the template definition available in a non-existant ('nil') scope. Scope must exist before performing the operation")
	}

	listTemplateDefinition := make(parser.SymbolDefinition)

	for _, statement := range root.Statements {
		if statement == nil {
			panic("unexpected 'nil' statement found in scope holder (group) while listing template definition available in parent scope")
		}

		if statement.GetKind() != parser.KIND_DEFINE_TEMPLATE {
			continue
		}

		templateScope, ok := statement.(*parser.GroupStatementNode)
		if !ok {
			panic("unexpected type found. As per standard, 'template' parent should be 'GroupStatementNode' type only")
		}

		templateHeader, ok := templateScope.ControlFlow.(*parser.TemplateStatementNode)
		if !ok {
			panic("unexpected type found. As per standard, 'template' header should be wrapped by 'TemplateStatementNode' type only")
		}

		templateName := string(bytes.Clone(templateHeader.TemplateName.Value))
		listTemplateDefinition[templateName] = statement
	}

	if len(listTemplateDefinition) == 0 {
		return nil
	}

	return listTemplateDefinition
}

// getGlobalTemplateDefinition(workspaceTemplateDefinitionExcludingActiveFile)
func getWorkspaceTemplateDefinitionExcludingActiveFile(parsedFilesInWorkspace map[string]*parser.GroupStatementNode) parser.SymbolDefinition {
	workspaceTemplateDefinition := make(parser.SymbolDefinition)

	for _, parseTree := range parsedFilesInWorkspace {
		fileTemplateDefinition := getRootTemplateDefinition(parseTree)
		maps.Copy(workspaceTemplateDefinition, fileTemplateDefinition)
	}

	/*
	fmt.Println("list workspace template definition")
	for templateName, node := range workspaceTemplateDefinition {
		fmt.Printf("\nname: %s -- %v\n----------\n", templateName, node)
	}
	*/

	return workspaceTemplateDefinition
}

func getBuiltinVariableDefinition() parser.SymbolDefinition {
	globalVariables := parser.SymbolDefinition {
		".": nil,
		"$": nil,
	}

	return globalVariables
}

func getBuiltinFunctionDefinition() parser.SymbolDefinition {
	builtinFunctionDefinition := parser.SymbolDefinition {
		"and": nil,
		"call": nil,
		"html": nil,
		"index": nil,
		"slice": nil,
		"js": nil,
		"len": nil,
		"not": nil,
		"or": nil,
		"print": nil,
		"printf": nil,
		"println": nil,
		"urlquery": nil,
		"eq": nil,
		"ne": nil,
		"lt": nil,
		"le": nil,
		"gt": nil,
		"ge": nil,
		"true": nil,	// unsure about this
		"false": nil,	// unsure about this
		"continue": nil,	// unsure about this
		"break": nil,	// uncertain about this
	}

	return builtinFunctionDefinition
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

	filesContentInWorkspace := openProjectFiles(rootDir)
	parsedFilesInWorkspace, _ := parseFilesInWorkspace(filesContentInWorkspace)
	_ = definitionAnalisisWithinWorkspace(parsedFilesInWorkspace)

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

