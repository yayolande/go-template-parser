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

// Most of the time, the 'MapFromFileNameToGroupStatementNode' variable must remain not be nil.
// however its map 'values' can be 'nil' if needed. Warning though, the map 'keys' should never be 'nil'
type MapFromFileNameToGroupStatementNode = map[string]*parser.GroupStatementNode
type Error = types.Error

// Recursively open files from 'rootDir'.
// However there is a depth limit for the recursion (current MAX_DEPTH = 5)
func OpenProjectFiles(rootDir, withFileExtension string) map[string][]byte {
	const maxDepth int = 5
	var currentDepth int = 0

	return openProjectFilesSafely(rootDir, withFileExtension, currentDepth, maxDepth)
}

func openProjectFilesSafely(rootDir, withFileExtension string, currentDepth, maxDepth int) map[string][]byte {
	if currentDepth > maxDepth {
		return nil
	}

	list, err := os.ReadDir(rootDir)
	if err != nil {
		panic("error while reading directory content: " + err.Error())
	}

	fileNamesToContent := make(map[string][]byte)

	for _, entry := range list {
		fileName := filepath.Join(rootDir, entry.Name())

		if entry.IsDir() {
			currentDepth++

			subDir := fileName
			subFiles := openProjectFilesSafely(subDir, withFileExtension, currentDepth, maxDepth)

			maps.Copy(fileNamesToContent, subFiles)
			continue
		}

		if !strings.HasSuffix(fileName, withFileExtension) {
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
		panic("'openProjectFilesSafely()' should never return a 'nil' file hierarchy. return an empty map instead")
	}

	return fileNamesToContent
}

// Parse a file content (buffer). The output is an AST node, and an error list containing parsing error and suggestions
func ParseSingleFile(source []byte) (*parser.GroupStatementNode, []Error) {
	tokens, _, tokenErrs := lexer.Tokenize(source)
	parseTree, parseErrs := parser.Parse(tokens)

	parseErrs = append(parseErrs, tokenErrs...)

	return parseTree, parseErrs
}

// Parse all files within a workspace.
// The output is an AST node, and an error list containing parsing error and suggestions
func ParseFilesInWorkspace(workspaceFiles map[string][]byte) (MapFromFileNameToGroupStatementNode, []Error) {
	parsedFilesInWorkspace := make(MapFromFileNameToGroupStatementNode)

	var errs []Error
	for longFileName, content := range workspaceFiles {
		tokens, _, tokenErr := lexer.Tokenize(content)
		parseTree, parseError := parser.Parse(tokens)

		parsedFilesInWorkspace[longFileName] = parseTree

		errs = append(errs, tokenErr...)
		errs = append(errs, parseError...)
	}

	if len(workspaceFiles) != len(parsedFilesInWorkspace) {
		panic("number of parsed files do not match the amount present in the workspace")
	}

	if parsedFilesInWorkspace == nil {
		panic("'ParseFilesInWorkspace()' should never return a 'nil' workspace. return an empty map instead")
	}

	return parsedFilesInWorkspace, errs
}

// TODO: disallow circular dependencies for 'template definition'
func DefinitionAnalysisSingleFile(fileName string, parsedFilesInWorkspace MapFromFileNameToGroupStatementNode) []Error {
	if parsedFilesInWorkspace == nil {
		panic("'parsedFilesInWorkspace' cannot be nil during definition analysis (single file)")
	}

	parseTreeActiveFile, ok := parsedFilesInWorkspace[fileName]
	if !ok {
		log.Printf("fatal, fileName = %s\n parsedFilesInWorkspace = %s\n", fileName, parsedFilesInWorkspace)
		panic(fileName + " is outside the current workspace, cant compute definition analysis for that file." +
			" to resolve the matter, add that file to the workspace, or create a new workspace with that file in it")
	}

	if parseTreeActiveFile == nil {
		return nil
	}

	clonedParsedFilesInWorkspace := maps.Clone(parsedFilesInWorkspace)
	delete(clonedParsedFilesInWorkspace, fileName)

	workspaceTemplateDefinition := getWorkspaceTemplateDefinition(clonedParsedFilesInWorkspace)
	globalVariableDefinition := getBuiltinVariableDefinition()
	localVariableDefinition := parser.SymbolDefinition{}
	functionDefinition := getBuiltinFunctionDefinition()

	log.Printf("global template def: %#v\n\n", workspaceTemplateDefinition)

	if workspaceTemplateDefinition == nil || globalVariableDefinition == nil || localVariableDefinition == nil || functionDefinition == nil {
		panic("'global/local/funciton/template' definition is nil. that map should always be instanciated, even if empty, for 'DefinitionAnalysis' processing")
	}

	errs := parseTreeActiveFile.DefinitionAnalysis(globalVariableDefinition, localVariableDefinition, functionDefinition, workspaceTemplateDefinition)

	return errs
}

// Definition analysis for all files within a workspace.
// It should only be done after 'ParseFilesInWorkspace()' or similar
func DefinitionAnalisisWithinWorkspace(parsedFilesInWorkspace MapFromFileNameToGroupStatementNode) []Error {
	if parsedFilesInWorkspace == nil {
		panic("'parsedFilesInWorkspace' cannot be nil during definition analysis")
	}

	if len(parsedFilesInWorkspace) == 0 {
		return nil
	}

	var cloneParsedFilesInWorkspace MapFromFileNameToGroupStatementNode

	var globalVariableDefinition, localVariableDefinition, functionDefinition parser.SymbolDefinition
	var workspaceTemplateDefinition parser.SymbolDefinition

	var errs []types.Error

	for longFileName, fileParseTree := range parsedFilesInWorkspace {
		if fileParseTree == nil {
			continue
		}

		// a. Get all the template definition of other project files except the current/active one
		cloneParsedFilesInWorkspace = maps.Clone(parsedFilesInWorkspace)
		delete(cloneParsedFilesInWorkspace, longFileName)

		workspaceTemplateDefinition = getWorkspaceTemplateDefinition(cloneParsedFilesInWorkspace)
		globalVariableDefinition = getBuiltinVariableDefinition()
		localVariableDefinition = parser.SymbolDefinition{}
		functionDefinition = getBuiltinFunctionDefinition()

		if workspaceTemplateDefinition == nil || globalVariableDefinition == nil || localVariableDefinition == nil || functionDefinition == nil {
			panic("'global/local/funciton/template' definition is nil. that map should always be instanciated, even if empty, for 'DefinitionAnalysis' processing")
		}

		// b. With the template definition, begin file definition analysis
		localErrs := fileParseTree.DefinitionAnalysis(globalVariableDefinition, localVariableDefinition, functionDefinition, workspaceTemplateDefinition)
		errs = append(errs, localErrs...)
	}

	return errs
}

// Print in JSON format the AST node to the screen. Use a program like 'jq' for pretty formatting
func Print(node ...types.AstNode) {
	str := parser.PrettyFormater(node)
	fmt.Println(str)
}

// Obtains all template definition available at the root of the group nodes only (no node traversal)
func getRootTemplateDefinition(root *parser.GroupStatementNode) parser.SymbolDefinition {
	if root == nil {
		return nil
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

// Get a list of all template definition (identified with "define" keyword) within the workspace
func getWorkspaceTemplateDefinition(parsedFilesInWorkspace MapFromFileNameToGroupStatementNode) parser.SymbolDefinition {
	workspaceTemplateDefinition := make(parser.SymbolDefinition)

	for _, parseTree := range parsedFilesInWorkspace {
		fileTemplateDefinition := getRootTemplateDefinition(parseTree)
		maps.Copy(workspaceTemplateDefinition, fileTemplateDefinition)
	}

	if workspaceTemplateDefinition == nil {
		panic("'workspaceTemplateDefinition()' should never return a 'nil' workspace. return an empty map instead")
	}

	log.Printf("'getWorkspaceTemplateDefinition() res : %#v\n\n", workspaceTemplateDefinition)

	return workspaceTemplateDefinition
}

func getBuiltinVariableDefinition() parser.SymbolDefinition {
	globalVariables := parser.SymbolDefinition{
		".": nil,
		"$": nil,
	}

	return globalVariables
}

func getBuiltinFunctionDefinition() parser.SymbolDefinition {
	builtinFunctionDefinition := parser.SymbolDefinition{
		"and":      nil,
		"call":     nil,
		"html":     nil,
		"index":    nil,
		"slice":    nil,
		"js":       nil,
		"len":      nil,
		"not":      nil,
		"or":       nil,
		"print":    nil,
		"printf":   nil,
		"println":  nil,
		"urlquery": nil,
		"eq":       nil,
		"ne":       nil,
		"lt":       nil,
		"le":       nil,
		"gt":       nil,
		"ge":       nil,
		"true":     nil, // unsure about this
		"false":    nil, // unsure about this
		"continue": nil, // unsure about this
		"break":    nil, // uncertain about this
	}

	return builtinFunctionDefinition
}
