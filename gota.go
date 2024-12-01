package gota

import (
	"bytes"
	"fmt"
	"io"
	"log"
	"maps"
	"os"
	"path/filepath"
	"strings"

	"github.com/yayolande/gota/lexer"
	"github.com/yayolande/gota/parser"
	"github.com/yayolande/gota/types"
)

type Error = types.Error

// Recursively open files from 'rootDir'
// TODO: put a limit on how deep the recursion can go (recommended MAX = 6)
func OpenProjectFiles(rootDir, withFileExtension string) map[string][]byte {
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

	if fileNamesToContent == nil {
		panic("'OpenProjectFiles()' should never return a 'nil' file hierarchy. return an empty map instead")
	}

	return fileNamesToContent
}

func ParseSingleFile(source []byte) (*parser.GroupStatementNode, []Error) {
	tokens, _, tokenErrs := lexer.Tokenize(source)
	parseTree, parseErrs := parser.Parse(tokens)

	parseErrs = append(parseErrs, tokenErrs...)
	return parseTree, parseErrs
}

// TODO: properly handly error return later on (intended for lsp user)
func ParseFilesInWorkspace(workspaceFiles map[string][]byte) (map[string]*parser.GroupStatementNode, []Error) {
	// 2. Parse the opened files --- parseFilesInWorkspace(workspace)
	parsedFilesInWorkspace := make(map[string]*parser.GroupStatementNode)

	var errs []Error
	for longFileName, content := range workspaceFiles {
		tokens, failedTokens, tokenErr := lexer.Tokenize(content)

		errs = append(errs, tokenErr...)
		_ = failedTokens

		if len(tokens) == 0 {
			continue
		}

		parseTree, parseError := parser.Parse(tokens)

		parsedFilesInWorkspace[longFileName] = parseTree
		errs = append(errs, parseError...)
	}

	if parsedFilesInWorkspace == nil {
		panic("'ParseFilesInWorkspace()' should never return a 'nil' workspace. return an empty map instead")
	}

	return parsedFilesInWorkspace, errs
}

// TODO: disallow circular dependencies for 'template definition'
func DefinitionAnalysisSingleFile(fileName string, parsedFilesInWorkspace map[string]*parser.GroupStatementNode) []Error {
	if parsedFilesInWorkspace == nil {
		panic("'parsedFilesInWorkspace' cannot be nil during definition analysis (single file)")
	}

	parseTreeActiveFile, ok := parsedFilesInWorkspace[fileName]
	if !ok {
		panic(fileName + " is outside the current workspace, cant compute definition analysis for that file." +
			" to resolve the matter add that file to the workspace, or create a new workspace with that file int it")
	}

	if parseTreeActiveFile == nil {
		log.Printf("fatal, filename = %s \n parsedFilesInWorkspace = %#s", fileName, parsedFilesInWorkspace)
		panic("'nil' is not accept as a 'root ast' inside a workspace. this has been uncovered during 'definitionAnalisisWithinWorkspace()'")
	}

	clonedParsedFilesInWorkspace := maps.Clone(parsedFilesInWorkspace)
	delete(clonedParsedFilesInWorkspace, fileName)

	workspaceTemplateDefinition := getWorkspaceTemplateDefinition(clonedParsedFilesInWorkspace)
	globalVariableDefinition := getBuiltinVariableDefinition()
	localVariableDefinition := parser.SymbolDefinition{}
	functionDefinition := getBuiltinFunctionDefinition()

	errs := parseTreeActiveFile.DefinitionAnalysis(globalVariableDefinition, localVariableDefinition, functionDefinition, workspaceTemplateDefinition)

	return errs
}

// TODO: change return type. it should accurate represent error data that the user can work with (lsp)
func DefinitionAnalisisWithinWorkspace(parsedFilesInWorkspace map[string]*parser.GroupStatementNode) []Error {
	if len(parsedFilesInWorkspace) == 0 {
		return nil
	}

	// 4. Definition Analysis (SemanticalAnalisis v1) (in all workspace files)
	// TODO: refactor this code to a function : definitionAnalisisWithinWorkspace(parsedFilesInWorkspace)
	var cloneParsedFilesInWorkspace map[string]*parser.GroupStatementNode

	var globalVariableDefinition, localVariableDefinition, functionDefinition parser.SymbolDefinition
	var workspaceTemplateDefinition parser.SymbolDefinition

	var errs []types.Error

	for longFileName, fileParseTree := range parsedFilesInWorkspace {
		if fileParseTree == nil {
			log.Printf("fata, fileName = %s \n parsedFilesInWorkspace = %#s\n", longFileName, parsedFilesInWorkspace)
			panic("a 'root ast' node should never be nil. make sure to only insert non-nil ast node into the workspace. " + 
				"error found at 'DefinitionAnalisisWithinWorkspace()' for fileName = " + longFileName)
		}
		// All this is equivalent ot 'DefinitionAnalysisSingleFile(longFileName, parsedFilesInWorkspace)'

		// a. Get all the template definition of other project files except the current/active one
		cloneParsedFilesInWorkspace = maps.Clone(parsedFilesInWorkspace)
		delete(cloneParsedFilesInWorkspace, longFileName)

		workspaceTemplateDefinition = getWorkspaceTemplateDefinition(cloneParsedFilesInWorkspace)
		globalVariableDefinition = getBuiltinVariableDefinition()
		localVariableDefinition = parser.SymbolDefinition{}
		functionDefinition = getBuiltinFunctionDefinition()

		// b. With the template definition, begin file definition analysis
		// TODO: put 'err' into a slice
		localErrs := fileParseTree.DefinitionAnalysis(globalVariableDefinition, localVariableDefinition, functionDefinition, workspaceTemplateDefinition)
		errs = append(errs, localErrs...)
	}

	return errs
}

func Print(node ...types.AstNode) {
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

		if statement.GetKind() != types.KIND_DEFINE_TEMPLATE {
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
func getWorkspaceTemplateDefinition(parsedFilesInWorkspace map[string]*parser.GroupStatementNode) parser.SymbolDefinition {
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

