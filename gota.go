package gota

import (
	"fmt"
	"log"
	"os"
	"io"
	"strings"
	"bytes"
	"maps"
	"path/filepath"

	"github.com/yayolande/gota/lexer"
	"github.com/yayolande/gota/parser"
)

// Recursively open files from 'rootDir'
func OpenProjectFiles(rootDir, withFileExtension string) map[string][]byte {
	// TODO: Change the extension type later on
	// targetExtension := ".html"
	// targetExtension := ".go"

	list, err := os.ReadDir(rootDir)
	if err != nil {
		panic("error while reading directory content: " + err.Error())
	}

	fileNamesToContent := make(map[string][]byte)

	for _, entry := range list {
		fileName := filepath.Join(rootDir, entry.Name())

		if entry.IsDir() {
			subDir := fileName
			subFiles := OpenProjectFiles(subDir, withFileExtension)

			maps.Copy(fileNamesToContent, subFiles)
			continue
		}

		if ! strings.HasSuffix(fileName, withFileExtension) {
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
func ParseFilesInWorkspace(workspaceFiles map[string][]byte) (map[string]*parser.GroupStatementNode, error) {
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
func DefinitionAnalisisWithinWorkspace(parsedFilesInWorkspace map[string]*parser.GroupStatementNode) error {
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

func Print(node ...parser.AstNode) {
	str := parser.PrettyFormater(node)
	fmt.Println(str)
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

