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
	checker "github.com/yayolande/gota/analyzer"
)

// TODO: I have an architecture/design mistake concerning the handling of error while parsing
// First, the problem.
// As it stands, the parsing pipeline is as follows: 
//
// text -> extract template -> lexer -> parsing -> analysis
//
// While lexing, if an error occur on a template line, that line is dropped altogether. The same goes for the parsing
// This mean that the output of the lexer and parser drop template line
// The issue with this design decision start with group-like template. For instance
// `{{ if -- }} hello {{ end }}` has an error on the first template line, and because of this only "{{ end }}" token
// will be output while lexing. Then you may ask, what may go wrong with this output ?
// Well you see, every 'if' statement should be close by an 'end' statement. At least that's how the parser 
// expect thing to be. However, since the 'if' statement have been dropped, the analyzer will only see the "{{ end }}" AST
// then it will swiftly report an Error
// thus an error on "{{ if ... }}" during lexing trigger an automatic error on "{{ end }}" while parsing. That's just wrong
// {{ end }} do not have any issue, only "{{ if ... }}" does. this could mislead the user to think that he made a syntax error
// or that there is too many {{ end }} statement
//
// I there recommend, on a future version, to overhaul the lexer and parser architecture.
// 2 things need to change
// first, is the tokens returned by the lexer. second, the lexer and parser should return failed tokens and ast all along
//
// For the first, `[]Token` should become `TokenFile struct { listToken [][]Token; listTokenSucessStatus []bool }` for the return type
// Or maybe `TokenStatements { statement []Token; status bool }` and `TokenFile { line []TokenStatements }`. 
// But I still wonder, is 'TokenFile' mandatory ?
//
// token < token line (statement == "{{ ... }}") < token file
// token line (statement) = list of tokens with the last token being 'EOL'
// token file = list of token line
//
// For the second, the lexer and parser should do as much as possible to return the closest valid token/ast, so that
// those tokens and their states (boolean status) are seen by the parser. The parser then could make adjustment onto which statement 
// can return error while parsing. obviously, a line of tokens that have failed should in any case return a parse error, since the 
// whole goal of this process is to make sure the 'analysis' phase do not output indesirable error to the user
// On the same vain, a field should be added in the AST, allowing to identify whether the ast is failed parsing or not, since now all
// failing and successful ast are returned. `ast { isParseError bool }`

// Most of the time, the 'MapFromFileNameToGroupStatementNode' variable must remain not be nil.
// however its map 'values' can be 'nil' if needed. Warning though, the map 'keys' should never be 'nil'
type MapFromFileNameToGroupStatementNode = map[string]*parser.GroupStatementNode
type Error = lexer.Error

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
			subDir := fileName
			subFiles := openProjectFilesSafely(subDir, withFileExtension, currentDepth + 1, maxDepth)

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
	// globalVariableDefinition := getBuiltinVariableDefinition()
	// localVariableDefinition := parser.SymbolDefinition{}
	// functionDefinition := getBuiltinFunctionDefinition()


	log.Printf("global template def: %#v\n\n", workspaceTemplateDefinition)

	if workspaceTemplateDefinition == nil {
		panic("'global/local/funciton/template' definition is nil. that map should always be instanciated, even if empty, for 'DefinitionAnalysis' processing")
	}

	file, errs := checker.DefinitionAnalysis(fileName, parseTreeActiveFile, workspaceTemplateDefinition)
	_ = file
	// errs := parseTreeActiveFile.DefinitionAnalysis(globalVariableDefinition, localVariableDefinition, functionDefinition, workspaceTemplateDefinition, localTemplateDefinition)

	return errs
}

// Definition analysis for all files within a workspace.
// It should only be done after 'ParseFilesInWorkspace()' or similar
// TODO: REMAKE THIS FUNCTION
func DefinitionAnalisisWithinWorkspace(parsedFilesInWorkspace MapFromFileNameToGroupStatementNode) []Error {
	if parsedFilesInWorkspace == nil {
		panic("'parsedFilesInWorkspace' cannot be nil during definition analysis")
	}

	if len(parsedFilesInWorkspace) == 0 {
		return nil
	}

	var cloneParsedFilesInWorkspace MapFromFileNameToGroupStatementNode

	// var globalVariableDefinition, localVariableDefinition, functionDefinition parser.SymbolDefinition
	var workspaceTemplateDefinition []*checker.TemplateDefinition

	var errs []lexer.Error

	for longFileName, fileParseTree := range parsedFilesInWorkspace {
		if fileParseTree == nil {
			continue
		}

		// a. Get all the template definition of other project files except the current/active one
		cloneParsedFilesInWorkspace = maps.Clone(parsedFilesInWorkspace)
		delete(cloneParsedFilesInWorkspace, longFileName)

		workspaceTemplateDefinition = getWorkspaceTemplateDefinition(cloneParsedFilesInWorkspace)
		// workspaceTemplateDefinition := getWorkspaceTemplateDefinition(clonedParsedFilesInWorkspace)
		// localTemplateDefinition := types.SymbolDefinition{}
		// globalVariableDefinition = getBuiltinVariableDefinition()
		// localVariableDefinition = parser.SymbolDefinition{}
		// functionDefinition = getBuiltinFunctionDefinition()

		if workspaceTemplateDefinition == nil {
			panic("'global/local/funciton/template' definition is nil. that map should always be instanciated, even if empty, for 'DefinitionAnalysis' processing")
		}

		// b. With the template definition, begin file definition analysis
		// localErrs := fileParseTree.DefinitionAnalysis(globalVariableDefinition, localVariableDefinition, functionDefinition, workspaceTemplateDefinition, localTemplateDefinition)
		_, localErrs := checker.DefinitionAnalysis(longFileName, fileParseTree, workspaceTemplateDefinition)
		errs = append(errs, localErrs...)
	}

	return errs
}

// Print in JSON format the AST node to the screen. Use a program like 'jq' for pretty formatting
func Print(node ...parser.AstNode) {
	str := parser.PrettyFormater(node)
	fmt.Println(str)
}

// Obtains all template definition available at the root of the group nodes only (no node traversal)
func getRootTemplateDefinition(root *parser.GroupStatementNode, fileName string) []*checker.TemplateDefinition {
	if root == nil {
		return nil
	}

	var listTemplateDefinition []*checker.TemplateDefinition

	for _, statement := range root.Statements {
		if statement == nil {
			panic("unexpected 'nil' statement found in scope holder (group) while listing template definition available in parent scope")
		}

		if ! (statement.GetKind() == parser.KIND_DEFINE_TEMPLATE || statement.GetKind() == parser.KIND_BLOCK_TEMPLATE) {
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

		def := &checker.TemplateDefinition{}
		def.Name = templateName
		def.Node = templateScope
		def.Range = templateScope.Range
		def.FileName = fileName
		def.IsValid = true

		listTemplateDefinition = append(listTemplateDefinition, def)
		// listTemplateDefinition[templateName] = statement
	}

	return listTemplateDefinition
}

// Get a list of all template definition (identified with "define" keyword) within the workspace
func getWorkspaceTemplateDefinition(parsedFilesInWorkspace MapFromFileNameToGroupStatementNode) []*checker.TemplateDefinition {
	var workspaceTemplateDefinition []*checker.TemplateDefinition
	var fileTemplateDefinition []*checker.TemplateDefinition

	for fileName, parseTree := range parsedFilesInWorkspace {
		fileTemplateDefinition = getRootTemplateDefinition(parseTree, fileName)
		workspaceTemplateDefinition = append(workspaceTemplateDefinition, fileTemplateDefinition...)
	}

	/*
	if workspaceTemplateDefinition == nil {
		panic("'workspaceTemplateDefinition()' should never return a 'nil' workspace. return an empty map instead")
	}
	*/

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
