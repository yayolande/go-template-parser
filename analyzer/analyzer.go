// TODO: features to add 
//
// [ ] Unused variable detection 
// [ ] Check that 'keyword' and 'types' are not used as parameter names (functions)
// [x] Function declaration in comment have invalid 'Range'
// [ ] Dectection of cyclical import
// [ ] Advanced type-system
// [ ] Go-To Definition
// [ ] Testing for major stable features

package analyzer

import (
	"bytes"
	"errors"
	"fmt"
	"go/ast"
	goParser "go/parser"
	"go/printer"
	"go/scanner"
	"go/token"
	"log"
	"maps"
	"strconv"
	"strings"

	"github.com/yayolande/gota/lexer"
	"github.com/yayolande/gota/parser"
)

const (
	TYPE_INT string = "int"
	TYPE_FLOAT = "float"
	TYPE_STRING = "string"
	TYPE_ANY = "any"
	TYPE_ERROR = "error"
	TYPE_VOID = "void"
	TYPE_INVALID = "invalid type"
)

type DefinitionAnalysisParams struct {
	GlobalVariables	map[string]parser.AstNode
	LocalVariables		map[string]parser.AstNode
}

// -------------------------
// Analyzer Types definition
// -------------------------

// TODO: add 'Stringer' method for FunctionDefinition, VariableDefinition, DataStructureDefinition

type FunctionDefinition struct {
	Node				parser.AstNode
	Range				lexer.Range
	FileName			string
	Name				string
	ParameterNames	[]string
	ParameterTypes	[]string
	ReturnTypes		[2]string
	IsValid			bool
}

type VariableDefinition struct {
	Node				parser.AstNode		// direct node containing info about this variable
	Range				lexer.Range
	FileName			string
	Name				string
	Type				string
	IsValid			bool
}

type DataStructureDefinition struct {
	Node				parser.AstNode
	Range				lexer.Range
	FileName			string
	Name				string
	Fields			[]VariableDefinition
	IsValid			bool
}

type TemplateDefinition struct {
	Node				parser.AstNode
	Range				lexer.Range
	FileName			string
	Name				string
	InputType		*DataStructureDefinition
	IsValid			bool
}

type FileDefinition struct {
	Root						*parser.GroupStatementNode
	Name						string
	ScopeToVariables		map[*parser.GroupStatementNode][]*VariableDefinition
	Functions				[]*FunctionDefinition			// Functions only defined at root (comming from external source)
	Templates				[]*TemplateDefinition			// Templates only defined at root or from external source
	WorkspaceTemplates	[]*TemplateDefinition
}

func (f FileDefinition) GetScopedVariables(scope *parser.GroupStatementNode) map[string]parser.AstNode {
	scopedVariables := make(map[string]parser.AstNode)

	if scope == nil || f.ScopeToVariables == nil {
		return scopedVariables
	}

	listVariables, found := f.ScopeToVariables[scope]
	if ! found {
		return scopedVariables
	}

	var name string
	var node parser.AstNode

	for _, variable := range listVariables {
		name = variable.Name
		node = variable.Node

		scopedVariables[name] = node
	}

	return scopedVariables
}

func (f FileDefinition) GetFunctions() map[string]parser.AstNode {
	scopedFunctions := make(map[string]parser.AstNode)

	if f.Functions == nil {
		return scopedFunctions
	}

	var name string
	var node parser.AstNode

	for _, variable := range f.Functions {
		name = variable.Name
		node = variable.Node

		scopedFunctions[name] = node
	}

	return scopedFunctions
}

func (f FileDefinition) LookupFunction(funcName string) *FunctionDefinition {
	for _, function := range f.Functions {
		if function.Name == funcName {
			return function
		}
	}

	return nil
}

func (f FileDefinition) GetFileScopedTemplates() map[string]parser.AstNode {
	scopedTemplates := make(map[string]parser.AstNode)

	if f.Templates == nil {
		return scopedTemplates
	}

	var name string
	var node parser.AstNode

	for _, variable := range f.Templates {
		name = variable.Name
		node = variable.Node

		scopedTemplates[name] = node
	}

	return scopedTemplates
}

func (f FileDefinition) GetWorkspaceTemplates() map[string]parser.AstNode {
	workspaceTemplates := make(map[string]parser.AstNode)

	if f.WorkspaceTemplates == nil {
		return workspaceTemplates
	}

	var name string
	var node parser.AstNode

	for _, variable := range f.WorkspaceTemplates {
		name = variable.Name
		node = variable.Node

		workspaceTemplates[name] = node
	}


	return workspaceTemplates
}

func (f FileDefinition) GetSingleFunctionDefinition(functionName string) *FunctionDefinition {
	for _, fn := range f.Functions {
		if fn.Name == functionName {
			return fn
		}
	}

	return nil
}

func (f FileDefinition) GetSingleTemplateDefinition(template parser.AstNode) *TemplateDefinition {

	for _, def := range f.Templates {
		if def.Node == template {
			return def
		}
	}

	for _, def := range f.WorkspaceTemplates {
		if def.Node == template {
			return def
		}
	}

	return nil
}

// ------------
// Start Here -
// ------------
func getBuiltinFunctionDefinition() []*FunctionDefinition {
	dict := parser.SymbolDefinition{
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

	var builtinFunctionDefinition []*FunctionDefinition
	var def *FunctionDefinition

	for key, val := range dict {
		def = &FunctionDefinition{}
		def.Name = key
		def.Node = val
		def.FileName = "builtin"
		def.IsValid = true

		builtinFunctionDefinition = append(builtinFunctionDefinition, def)
	}

	return builtinFunctionDefinition
}

func NewDotVariableDefinition(fileName string, node parser.AstNode) *VariableDefinition {
	def := &VariableDefinition{}

	def.Name = "."
	def.Node = node
	def.Range = node.GetRange()
	def.FileName = fileName
	def.Type = TYPE_ANY
	def.IsValid = true

	return def
}

func NewDollarVariableDefinition(fileName string, node parser.AstNode) *VariableDefinition {
	def := &VariableDefinition{}

	def.Name = "$"
	def.Node = node
	def.Range = node.GetRange()
	def.FileName = fileName
	def.Type = TYPE_ANY
	def.IsValid = true

	return def
}

func NewVariableDefinition(variableName string, node parser.AstNode, fileName string) *VariableDefinition {
	def := &VariableDefinition{}

	def.Name = variableName
	def.FileName = fileName
	def.Type = TYPE_ANY
	def.IsValid = true

	if node != nil {
		def.Node = node
		def.Range = node.GetRange()
	}

	return def
}

func DefinitionAnalysis(fileName string, node *parser.GroupStatementNode, outterTemplate []*TemplateDefinition) (*FileDefinition, []lexer.Error) {
	if node == nil {
		return nil, nil
	}

	// TODO: should we deny when 'node.IsRoot()' == false ????

	fileInfo := new(FileDefinition)

	fileInfo.Name = fileName
	fileInfo.Root = node

	fileInfo.WorkspaceTemplates = outterTemplate
	fileInfo.Templates = nil
	fileInfo.Functions = getBuiltinFunctionDefinition()
	fileInfo.ScopeToVariables = make(map[*parser.GroupStatementNode][]*VariableDefinition)

	globalVariables := make(map[string]*VariableDefinition)
	localVariables := make(map[string]*VariableDefinition)

	// TODO: I do believe those variable '$' and '.' should at some point be saved into the 'FileDefinition'
	globalVariables["."] = NewVariableDefinition(".", nil, fileInfo.Name)
	globalVariables["$"] = NewVariableDefinition("$", nil, fileInfo.Name)

	statementType, errs := definitionAnalysisRecursive(node, nil, fileInfo, globalVariables, localVariables)
	_ = statementType

	return fileInfo, errs
}

func definitionAnalysisRecursive(node parser.AstNode, parent *parser.GroupStatementNode, file *FileDefinition, globalVariables, localVariables map[string]*VariableDefinition) ([2]string, []lexer.Error) {
	if globalVariables == nil || localVariables == nil {
		panic("arguments global/local variable defintion for 'definitionAnalysis()' shouldn't be 'nil'")
	}

	var errs []lexer.Error
	var statementType [2]string

	switch n := node.(type) {
	case *parser.GroupStatementNode:
		statementType, errs = definitionAnalysisGroupStatement(n, parent, file, globalVariables, localVariables)
	case *parser.TemplateStatementNode:
		statementType, errs = definitionAnalysisTemplatateStatement(n, parent, file, globalVariables, localVariables)
	case *parser.CommentNode:
		statementType, errs = definitionAnalysisComment(n, parent, file, globalVariables, localVariables)
	case *parser.VariableDeclarationNode:
		statementType, errs = definitionAnalysisVariableDeclaration(n, parent, file, globalVariables, localVariables)
	case *parser.VariableAssignationNode:
		statementType, errs = definitionAnalysisVariableAssignment(n, parent, file, globalVariables, localVariables)
	case *parser.MultiExpressionNode:
		statementType, errs = definitionAnalysisMultiExpression(n, parent, file, globalVariables, localVariables)
	case *parser.ExpressionNode:
		statementType, errs = definitionAnalysisExpression(n, parent, file, globalVariables, localVariables)
	default:
	}
	
	return statementType, errs
}

// TODO: Add 'listOfUsedVariables' in order to catch unused variables
func definitionAnalysisGroupStatement(node *parser.GroupStatementNode, parent *parser.GroupStatementNode, file *FileDefinition, globalVariables, localVariables map[string]*VariableDefinition) ([2]string, []lexer.Error) {
	// if globalVariables == nil || localVariables == nil || functionDefinitions == nil || templateDefinitionsGlobal == nil || templateDefinitionsLocal == nil {
	if globalVariables == nil || localVariables == nil {
		panic("arguments global/local/function/template defintion for 'DefinitionAnalysis()' shouldn't be 'nil' for 'GroupStatementNode'")
	}

	if node.IsRoot() == true && node.Parent() != nil {
		panic("only root node can be flaged as 'root' and with 'parent == nil'")
	}

	// NOTE: However non-root element could have 'v.parent == nil' when an error occurs

	// 1. Variables Init
	scopedGlobalVariables := map[string]*VariableDefinition{}
	// scopedTemplateDefinitionLocal := map[string]parser.AstNode{}

	maps.Copy(scopedGlobalVariables, globalVariables)
	maps.Copy(scopedGlobalVariables, localVariables)
	// maps.Copy(scopedTemplateDefinitionLocal, templateDefinitionsLocal)

	localVariables = map[string]*VariableDefinition{}		// 'localVariables' lost reference to the parent 'map', so no need to worry using it

	var errs []lexer.Error
	var localErrs []lexer.Error
	var statementType [2]string

	// 2. ControlFlow analysis
	switch node.Kind {
	case parser.KIND_IF, parser.KIND_ELSE_IF, parser.KIND_RANGE_LOOP, parser.KIND_WITH, 
		parser.KIND_ELSE_WITH, parser.KIND_DEFINE_TEMPLATE, parser.KIND_BLOCK_TEMPLATE:

		if node.ControlFlow == nil {
			log.Printf("fatal, 'controlFlow' not found for 'GroupStatementNode'. \n %s \n", node)
			panic("this 'GroupStatementNode' expect a non-nil 'controlFlow' based on its type ('Kind')")
		}

		statementType, errs = definitionAnalysisRecursive(node.ControlFlow, node, file, scopedGlobalVariables, localVariables)
	}

	// 3. Variables Scope
	switch node.Kind {
	case parser.KIND_IF, parser.KIND_ELSE, parser.KIND_ELSE_IF, parser.KIND_GROUP_STATEMENT, parser.KIND_END:

	case parser.KIND_RANGE_LOOP, parser.KIND_WITH, parser.KIND_ELSE_WITH:
		// TODO: I do believe those variable '$' and '.' should at some point be saved into the 'FileDefinition'

		scopedGlobalVariables["."] = NewVariableDefinition(".", node.ControlFlow, file.Name)
		file.ScopeToVariables[node] = append(file.ScopeToVariables[node], scopedGlobalVariables["."])

	case parser.KIND_DEFINE_TEMPLATE, parser.KIND_BLOCK_TEMPLATE:

		scopedGlobalVariables = make(map[string]*VariableDefinition)
		localVariables = make(map[string]*VariableDefinition)

		// TODO: I do believe those variable '$' and '.' should at some point be saved into the 'FileDefinition'
		scopedGlobalVariables["."] = NewVariableDefinition(".", node.ControlFlow, file.Name)
		scopedGlobalVariables["$"] = NewVariableDefinition("$", node.ControlFlow, file.Name)

		scopedGlobalVariables["."].Range = node.Range
		scopedGlobalVariables["$"].Range = node.Range

		file.ScopeToVariables[node] = append(file.ScopeToVariables[node], scopedGlobalVariables["."])
		file.ScopeToVariables[node] = append(file.ScopeToVariables[node], scopedGlobalVariables["$"])

	default:
		panic("found unexpected 'Kind' for 'GroupStatementNode' during 'DefinitionAnalysis()'\n node = " + node.String())
	}

	// 4. Statements analysis
	for _, statement := range node.Statements {
		if statement == nil {
			panic("statement within 'GroupStatementNode' cannot be nil. make to find where this nil value has been introduced and rectify it")
		}

		// Make DefinitionAnalysis for every children
		statementType, localErrs = definitionAnalysisRecursive(statement, node, file, globalVariables, localVariables)
		errs = append(errs, localErrs...)
	}

	return statementType, errs
}

func definitionAnalysisTemplatateStatement(node *parser.TemplateStatementNode, parent *parser.GroupStatementNode, file *FileDefinition, globalVariables, localVariables map[string]*VariableDefinition) ([2]string, []lexer.Error) {
	if parent == nil {
		panic("template cannot be parentless, it shoud be contain in at least 1 one scope")
	}

	templateDefinitionsGlobal := file.GetWorkspaceTemplates()
	templateDefinitionsLocal := file.GetFileScopedTemplates()

	if templateDefinitionsGlobal == nil || templateDefinitionsLocal == nil {
		panic("templateDefinitionsGlobal/templateDefinitionsLocal shouldn't be empty for 'TemplateStatementNode.DefinitionAnalysis()'")
	}

	if node.TemplateName == nil {
		panic("the template name should never be empty for a template expression. make sure the template has been parsed correctly.\n" + node.String())
	}

	var errs, localErrs []lexer.Error
	var expressionType [2]string

	// 1. Expression analysis, if any
	if node.Expression != nil {
		// localErr := node.Expression.DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitionsGlobal, templateDefinitionsLocal)
		// errs = append(errs, localErr...)
		expressionType, localErrs = definitionAnalysisRecursive(node.Expression, parent, file, globalVariables, localVariables)
		errs = append(errs, localErrs...)

		// TODO: Can a 'template' accept 2 return type ?
		// If yes, in what circumstance it can and cannot ?
	}

	// 2. template name analysis
	switch node.Kind {
	case parser.KIND_USE_TEMPLATE:
		templateName := string(node.TemplateName.Value)

		// Check that the template name is defined
		templateGlobal, foundGlobal := templateDefinitionsGlobal[templateName]
		templateLocal, foundLocal := templateDefinitionsLocal[templateName]

		if  ! foundGlobal && ! foundLocal {
			err := parser.NewParseError(node.TemplateName, errors.New("undefined template"))
			errs = append(errs, err)

			return expressionType, errs
		}

		// Check that the variable type match the template input type
		currentTemplate := templateGlobal
		if foundLocal {
			currentTemplate = templateLocal
		}

		def := file.GetSingleTemplateDefinition(currentTemplate)
		if def == nil {
			log.Println("found nil 'TemplateDefinition' for an existing template.\n file def = %#v\n", file)
			panic("'TemplateDefinition' cannnot be nil for an existing template")
		}

		// skip type analysis
		if def.Name == "any" || def.Name == "interface{}" || def.Name == "" {
			return expressionType, errs
		}

		if def.InputType == nil {
			if def.Name != expressionType[0] {
				err := parser.NewParseError(&lexer.Token{}, errors.New("template do not support this type"))
				err.Range = node.Expression.GetRange()

				errs = append(errs, err)
			}

			return expressionType, errs
		}
		
		// TODO: handle comlex data type. since 'expressionType' is a string, there need a function that will
		// convert it into '*DataStructureDefinition' and then compare it against 'def.InputType' type

	case parser.KIND_DEFINE_TEMPLATE, parser.KIND_BLOCK_TEMPLATE:
		// NOTE: v.parent == TemplateScope, so we need to go deeper to reach the outer scope
		// nodeScope := node.Parent().Parent()
		// if node.Parent().Parent() != nil && node.Parent().Parent().IsRoot() == false {
		// TODO: is it 'parent.IsRoot()' or 'parent.Parent().IsRoot()' ???????????????????????????????????????????????????????
		if parent.Parent().IsRoot() == false {
			err := parser.NewParseError(node.TemplateName, errors.New("template cannot be defined in local scope"))
			errs = append(errs, err)
		}

		templateName := string(node.TemplateName.Value)

		// Make sure that the template haven't already be defined in the local scope (root scope)
		_, found := templateDefinitionsLocal[templateName]
		if found {
			err := parser.NewParseError(node.TemplateName, errors.New("template already defined"))
			errs = append(errs, err)
		}

		templateDefinitionsLocal[templateName] = node.Parent()	// Not necessary since this variable is short lived

		def := &TemplateDefinition{}
		def.Name = templateName
		def.Node = node
		def.FileName = file.Name
		def.Range = node.TemplateName.Range
		def.InputType = &DataStructureDefinition{}
		def.InputType.Name = "any"
		def.InputType.IsValid = true

		file.Templates = append(file.Templates, def)

		if node.Parent() == nil {
			log.Printf("fatal, parent not found on template definition. template = \n %s \n", node)
			panic("'TemplateStatementNode' with unexpected empty parent node. " +
				"the template definition and block template must have a parent which will contain all the statements inside the template")
		}

		if node.Parent().Kind != node.Kind {
			panic("value mismatch. 'TemplateStatementNode.Kind' and 'TemplateStatementNode.parent.Kind' must similar")
		}
	default:
		panic("'TemplateStatementNode' do not accept any other type than 'KIND_DEFINE_TEMPLATE, KIND_BLOCK_TEMPLATE, KIND_USE_TEMPLATE'")
	}

	return expressionType, errs
}

func definitionAnalysisComment(comment *parser.CommentNode, parentScope *parser.GroupStatementNode, file *FileDefinition, globalVariables, localVariables map[string]*VariableDefinition) ([2]string, []lexer.Error) {
	if parentScope == nil {
		panic("'variable declaration' cannot be parentless, it shoud be contain in at least 1 one scope")
	}

	if comment.Kind != parser.KIND_COMMENT {
		panic("found value mismatch for 'CommentNode.Kind' during DefinitionAnalysis().\n " + comment.String())
	}

	if comment.GoCode == nil {
		return [2]string{}, nil
	}

	// 1. Find and store all functions and struct definitions
	expressionType := [2]string{}

	const virtualFileName = "comment_for_go_template_virtual_file.go"
	const virtualHeader = "package main\n"

	fileSet := token.NewFileSet()
	source := append([]byte(virtualHeader), comment.GoCode...)
	goNode, err := goParser.ParseFile(fileSet, virtualFileName, source, goParser.AllErrors)

	// ast.Inspect(goNode, inspectFunctionsWithinCommentGoCode(comment, parentScope, file, fileSet, virtualHeader))
	// ast.Inspect(comment, inspectDataStructuresWithinCommentGoCode(v.DataStructures))

	walker := NewWalker(comment, parentScope, file, fileSet, virtualHeader)
	ast.Walk(walker, goNode)

	log.Printf("ooo comment analysis after ast.Inspect() : %#v", comment)

	var errs []lexer.Error
	errs = append(errs, walker.errs...)

	if err != nil {
		log.Println("comment scanner error found, ", err)

		errorList, ok := err.(scanner.ErrorList)
		if !ok {
			panic("unexpected error, error obtained by go code parsing did not return expected type ('scanner.ErrorList')")
		}

		const randomColumnOffset int = 7

		for _, errScanner := range errorList {
			// A. Build diagnostic errors
			parseErr := NewParseErrorFromErrorList(errScanner, randomColumnOffset)
			parseErr.Range = remapRangeFromCommentGoCodeToSource(virtualHeader, comment.Range, parseErr.Range)
			log.Println("comment scanner error :: ", parseErr)

			errs = append(errs, parseErr)

			// B. Tag the functions that are erroneous 
			for _, function := range file.Functions {
				if function == nil {
					log.Println("nil function inserted into file definition\n FileDefinition = %#v\n", file)
					panic("nil function inserted into FileDefinition")
				}

				errorLine := parseErr.Range.Start.Line
				errorColumn := parseErr.Range.Start.Character

				if function.Range.Start.Line > errorLine {
					continue
				}

				if function.Range.End.Line < errorLine {
					continue
				}

				if function.Range.Start.Character > errorColumn {
					continue
				}

				function.IsValid = false
			}
		}
	}

	// 2. Do definition analysis by inserting those functions into the global scope

	/*
	for _, function := range file.Functions {
		if _, ok := functionDefinitions[function.Name]; ok {
			err := parser.NewParseError(&lexer.Token{}, errors.New("function redeclaration"))
			err.Range = function.Range

			errs = append(errs, err)

			continue
		}

		functionDefinitions[function.Name] = function.Node
	}
	*/

	// 3. Return errs
	log.Printf("end comment analysis : file = %#v\n", file)
	for _, def := range file.Functions {
			log.Printf("\n ff function = %#v\n", def)
	}

	return expressionType, errs
}

func definitionAnalysisVariableDeclaration(node *parser.VariableDeclarationNode, parentScope *parser.GroupStatementNode, file *FileDefinition, globalVariables, localVariables map[string]*VariableDefinition) ([2]string, []lexer.Error) {
	if parentScope == nil {
		panic("'variable declaration' cannot be parentless, it shoud be contain in at least 1 one scope")
	}

	if node.Kind != parser.KIND_VARIABLE_DECLARATION {
		panic("found value mismatch for 'VariableDeclarationNode.Kind' during DefinitionAnalysis()")
	}

	// TODO: is this necessry ????
	if node.Range == getZeroRangeValue() {
		panic("'Range' cannot be empty for VariableAssignationNode.\n" + node.String())
	}

	if localVariables == nil {
		panic("'localVariables' shouldn't be nil for 'VariableDeclarationNode.DefinitionAnalysis()'")
	}

	var errs []lexer.Error
	var expressionType [2]string

	// 0. Check that 'expression' is valid
	if node.Value != nil {
		var localErrs []lexer.Error
		expressionType, localErrs = definitionAnalysisMultiExpression(node.Value, parentScope, file, globalVariables, localVariables)

		errs = append(errs, localErrs...)
	} else {
		localErr := parser.NewParseError(&lexer.Token{}, errors.New("assignment expression cannot be empty"))
		localErr.Range = node.Range
		errs = append(errs, localErr)
	}

	// 1. Check at least var is declared
	if len(node.VariableNames) == 0 {
		errLocal := parser.ParseError{Err: errors.New("variable name is empty for the declaration"), Range: node.Range}
		errs = append(errs, errLocal)

		return expressionType, errs
	}

	// 2. Check existance of variable and process without error if 'var' is unique
	for count, variable := range node.VariableNames {
		if count > 2 {
			err := parser.NewParseError(variable,
				errors.New("can't have more than 2 variables declaration on same statement"))

			errs = append(errs, err)
			continue
		}

		if bytes.ContainsAny(variable.Value, ".") {
			err := parser.NewParseError(variable,
				errors.New("variable name cannot include the special character '.' while declaring"))

			errs = append(errs, err)
		}

		// Check if this variable name exist in the current scope. No need to check for function since 'var' always start with $
		key := string(variable.Value)

		if _, found := localVariables[key]; found {
			err := parser.NewParseError(variable, errors.New("variable redeclaration"))
			errs = append(errs, err)

			continue
		}

		// 2. Insert definition into dictionary, since there is no error whether the variable is already declared or not
		def := &VariableDefinition{}
		def.Node = node
		def.Name = key
		def.Type = expressionType[count]
		def.FileName = file.Name
		def.Range = variable.Range
		def.IsValid = false

		file.ScopeToVariables[parentScope] = append(file.ScopeToVariables[parentScope], def)
		localVariables[key] = def
	}

	return expressionType, errs
}

func definitionAnalysisVariableAssignment(node *parser.VariableAssignationNode, parent *parser.GroupStatementNode, file *FileDefinition, globalVariables, localVariables map[string]*VariableDefinition) ([2]string, []lexer.Error) {
	if node.Kind != parser.KIND_VARIABLE_ASSIGNMENT {
		panic("found value mismatch for 'VariableAssignationNode.Kind' during DefinitionAnalysis()\n" + node.String())
	}

	if globalVariables == nil || localVariables == nil {
		panic("'localVariables' or 'globalVariables' shouldn't be empty for 'VariableAssignationNode.DefinitionAnalysis()'")
	}

	var errs []lexer.Error
	var expressionType [2]string

	// 0. Check that 'expression' is valid
	if node.Value != nil {
		var localErrs []lexer.Error

		expressionType, localErrs = definitionAnalysisMultiExpression(node.Value, parent, file, globalVariables, localVariables)
		errs = append(errs, localErrs...)
	} else {
		errLocal := parser.NewParseError(nil, errors.New("assignment value cannot be empty"))
		errLocal.Range = node.Range
	}

	// 1. Check at least var is declared
	if node.VariableName == nil {
		errLocal := parser.ParseError{Err: errors.New("empty variable name. syntax should be 'variable = value'"), Range: node.Range}
		errLocal.Range = node.Range

		errs = append(errs, errLocal)
		return expressionType, errs
	}

	if bytes.ContainsAny(node.VariableName.Value, ".") {
		err := parser.NewParseError(node.VariableName,
			errors.New("variable name cannot contains any special character such '.' while assigning"))

		errs = append(errs, err)
		return expressionType, errs
	}

	// 2. Check if variable is defined, if not report error
	name := string(node.VariableName.Value)
	defLocal, isLocal := localVariables[name]
	defGlobal, isGlobal := globalVariables[name]

	var def *VariableDefinition

	if isLocal {
		def = defLocal
	} else if isGlobal {
		def = defGlobal
	} else {
		err := parser.NewParseError(node.VariableName, errors.New("undefined variable"))
		errs = append(errs, err)
		return expressionType, errs
	}

	if ! matchTypeWithHeuristicStrategy(def.Type, expressionType[0]) {
		err := parser.NewParseError(node.VariableName, errors.New("type mismatch; expected '" + def.Type + "' but got '" + expressionType[0] +"'"))
		errs = append(errs, err)

		expressionType[0] = TYPE_INVALID
		return expressionType, errs
	}

	// localVariables[name] = &node

	return expressionType, errs
}

func definitionAnalysisMultiExpression(node *parser.MultiExpressionNode, parent *parser.GroupStatementNode, file *FileDefinition, globalVariables, localVariables map[string]*VariableDefinition) ([2]string, []lexer.Error) {
	if node.Kind != parser.KIND_MULTI_EXPRESSION {
		panic("found value mismatch for 'MultiExpressionNode.Kind' during DefinitionAnalysis()\n" + node.String())
	}

	var errs, localErrs []lexer.Error
	var expressionType [2]string
	var count int = 0

	for _, expression := range node.Expressions {
		if expression == nil {
			log.Printf("fatal, nil element within expression list for MultiExpressionNode. \n %s \n", node.String())
			panic("element within expression list cannot be 'nil' for MultiExpressionNode. instead of inserting the nil value, omit it")
		}

		// normal processing when this is the first expression
		if count == 0 {
			expressionType, localErrs = definitionAnalysisExpression(expression, parent, file, globalVariables, localVariables)
			errs = append(errs, localErrs...)

			count ++
			continue
		}

		// when piping, you pass the result of the previous expression to the end position of the current expression
		// 
		// create a token group and insert it to the end of the expression
		tokenGroup := &lexer.Token{ID: lexer.GROUP, Value: []byte("$__TMP_GROUP"), Range: expression.Range}
		expression.Symbols = append(expression.Symbols, tokenGroup)

		// TODO: Introduce the concept of reserved variable name
		// In my case, variable that start with "$__" are reserved for parser internal use

		// then insert that token as variable within the file
		def := NewVariableDefinition("$__TMP_GROUP", nil, file.Name)
		def.Range = tokenGroup.Range
		def.IsValid = true
		def.Type = expressionType[0]
		// file.ScopeToVariables[parent] = append(file.ScopeToVariables[parent], def)
		localVariables[def.Name] = def

		// do the normal deed here (core processing)
		expressionType, localErrs = definitionAnalysisExpression(expression, parent, file, globalVariables, localVariables)
		errs = append(errs, localErrs...)

		// once, processing is over, remove the group created from the expression
		size := len(expression.Symbols)
		expression.Symbols = expression.Symbols[:size - 1]

		// remove temporary variable definition from file
		// TODO: do as tated above, and do a proper lookup
		// size = len(file.ScopeToVariables[parent])
		// defToRemove := file.ScopeToVariables[parent][size - 1]

		delete(localVariables, def.Name)
		// file.ScopeToVariables[parent] = file.ScopeToVariables[parent][:size - 1] // this is not good, nothing garanty that 'def' will this be at the end. look for it propertly

		/*
		if def != defToRemove {
			panic("removal of the wrong variable definition within file; may be do a for loop to find and replace the pointer instead")
		}
		*/

		// never forget about this one
		count ++
	}

	return expressionType, errs
}

func definitionAnalysisExpression(node *parser.ExpressionNode, parent *parser.GroupStatementNode, file *FileDefinition, globalVariables, localVariables map[string]*VariableDefinition) ([2]string, []lexer.Error) {
	if node.Kind != parser.KIND_EXPRESSION {
		panic("found value mismatch for 'ExpressionNode.Kind'. expected 'KIND_EXPRESSION' instead. current node \n" + node.String())
	}

	if globalVariables == nil || localVariables == nil {
		panic("'globalVariables' or 'localVariables' or shouldn't be empty for 'ExpressionNode.DefinitionAnalysis()'")
	}

	// TODO: is this necessary ??? If it was a pointer it will okay, but it is not
	if node.Range == getZeroRangeValue() {
		panic("empty 'Range' unauthorized for ExpressionNode \n" + node.String())
	}

	// 1. Check FunctionName is legit (var or func)
	expressionType := [2]string{}
	var errs []lexer.Error

	if len(node.Symbols) == 0 {
		err := parser.NewParseError(nil, errors.New("empty expression"))
		err.Range = node.Range
		errs = append(errs, err)

		return expressionType, errs
	}

	// ------------
	// ------------
	// New area
	// ------------
	// ------------

	defChecker := NewDefinitionAnalyzer(node.Symbols, file, node.Range)
	expressionType[0], errs = defChecker.makeSymboleDefinitionAnalysis(localVariables, globalVariables)

	log.Println("end expression !!!!!")
	
	return expressionType, errs

	// count := 0
	// count, expressionType[0], _, errs = makeSymboleDefinitionAnalysis(count, node.Symbols, localVariables, globalVariables, file)

	// ------------
	// ------------
	// End New area
	// ------------
	// ------------

	switch node.Symbols[0].ID {
	case lexer.STRING: expressionType[0] = "string"
	case lexer.NUMBER: expressionType[0] = "int"
	default: expressionType[0] = "invalid type"
	}

	name := string(node.Symbols[0].Value)

	// To handle those case: '$.Name', '$.'
	if index := strings.IndexByte(name, byte('.')); index >= 1 && len(name) != index+1 {
		name = name[:index]
	}

	isDotVariable := name[0] == '.'
	isDollarVariable := name[0] == '$'
	_, isLocalVariable := localVariables[name]
	_, isGlobalVariable := globalVariables[name]

	// TODO: What about 'method' ? a 'Dot_variable' can represent a method as well
	// So there needs a way to handle that case

	// Dot_variable cannot be checked on DefinitionAnalysis(), only on type checking analysis
	if isLocalVariable || isGlobalVariable || isDotVariable { // variable found
		if len(node.Symbols) > 1 {
			err := parser.NewParseError(node.Symbols[0], errors.New("variable cannot have arguments, only function. Remove the extraneous argument, or use a function instead"))
			errs = append(errs, err)

			return expressionType, errs
		}
	} else if isDollarVariable && !isLocalVariable && !isGlobalVariable { // dollar variable not found
		err := parser.NewParseError(node.Symbols[0], errors.New("undefined variable"))
		errs = append(errs, err)

		return expressionType, errs
	} else if node.Symbols[0].ID == lexer.STRING || node.Symbols[0].ID == lexer.NUMBER {
		expressionType[0] = "string"

		if len(node.Symbols) > 1 {
			err := parser.NewParseError(node.Symbols[0], errors.New("'"+node.Symbols[0].ID.String()+"' cannot have argument(s)"))
			errs = append(errs, err)

			return expressionType, errs
		}
	} else {
		// Function argument check
		functionDef := file.LookupFunction(name)
		if functionDef == nil {
			err := parser.NewParseError(node.Symbols[0], errors.New("undefined function"))
			errs = append(errs, err)

			return expressionType, errs
		}
	}
		functionDef := file.LookupFunction(name)



	expressionType[0] = functionDef.ReturnTypes[0]
	expressionType[1] = functionDef.ReturnTypes[1]

	if expressionType[0] == "" || ! (expressionType[1] == "" && expressionType[1] == "error") {
		err := parser.NewParseError(&lexer.Token{}, errors.New("function return type not well defined"))
		err.Range = node.Range
		errs = append(errs, err)

		expressionType = [2]string{}
		return expressionType, errs
	}

	// 2. check argument validity
	isGlobalVariable = false
	isLocalVariable = false
	// isFunction := false

	for count, arg := range node.Symbols[1:] {
		_ = count

		if arg.ID == lexer.STRING || arg.ID == lexer.NUMBER {

			argType := "string"
			if arg.ID == lexer.NUMBER { argType = "int" }

			if argType != functionDef.ParameterNames[count] {
				err := parser.NewParseError(arg, errors.New("type mismatch"))
				errs = append(errs, err)
			}

			continue
		}

		name = string(arg.Value)

		isDotVariable := name[0] == '.'
		_, isLocalVariable = localVariables[name]
		_, isGlobalVariable = globalVariables[name]

		// Dot_variable cannot be checked on DefinitionAnalysis(), only on type checking analysis
		if isLocalVariable || isGlobalVariable || isDotVariable {
			continue
		}

		anotherFunctionDef := file.LookupFunction(name)
		if anotherFunctionDef != nil {
			continue
		}

		err := parser.NewParseError(arg, errors.New(("argument not declared anywhere")))
		errs = append(errs, err)

		// node.isError = true
	}

	return expressionType, errs
}

// first make definition analysis to find all existing reference
// then make the type analysis
type definitionAnalyzer struct {
	symbols				[]*lexer.Token
	index					int
	isEOF					bool
	file					*FileDefinition
	rangeExpression	lexer.Range
}

func (a definitionAnalyzer) String() string {
str := fmt.Sprintf(`{ "symbols": %s, "index": %d, "file": %s, "rangeExpression": %s }`, lexer.PrettyFormater(a.symbols), a.index, a.file, a.rangeExpression.String())
	return str
}

func (a *definitionAnalyzer) peek() *lexer.Token {
	if a.index >= len(a.symbols) {
		panic("index out of bound for 'definitionAnalyzer'; check that you only use the provide method to move between tokens, like 'analyzer.nextToken()'")
	}

	return a.symbols[a.index]
}

func (a *definitionAnalyzer) peekAt(index int) *lexer.Token {
	if index < 0 {
		panic("negative index is not allowed")
	}

	if index >= len(a.symbols) {
		panic("index out of bound for 'definitionAnalyzer'; check that you only the index is comming from property 'analyzer.index'")
	}

	return a.symbols[index]
}

func (a *definitionAnalyzer) nextToken() {
	if a.index + 1 >= len(a.symbols) {
		a.isEOF = true
		return
	}

	a.index++
}

func (a definitionAnalyzer) isTokenAvailable() bool {
	if a.isEOF {
		return false
	}

	return a.index < len(a.symbols)
}

func NewDefinitionAnalyzer(symbols []*lexer.Token, file *FileDefinition, rangeExpr lexer.Range) *definitionAnalyzer {
	ana := &definitionAnalyzer{
		symbols: symbols,
		index: 0,
		file: file,
		rangeExpression: rangeExpr,
	}

	return ana
}

// fetch all tokens and sort them
func (p *definitionAnalyzer) makeSymboleDefinitionAnalysis(localVariables, globalVariables map[string]*VariableDefinition) (string, []lexer.Error) {
	var errs []lexer.Error

	log.Println("infinite loop ?????????")

	/*
	if len(p.symbols) == 0 {
		err := parser.NewParseError(&lexer.Token{}, errors.New("empty sub-expression"))
		err.Range = p.rangeExpression
		errs = append(errs, err)

		return TYPE_VOID, errs
	}
	*/

	listOfUsedFunctions := map[string]*FunctionDefinition{}
	processedToken := []*lexer.Token{}
	processedTypes := []string{}
	usedVariables := map[string]bool{}

	var symbol *lexer.Token
	startIndex := p.index

	count := 0
	for p.isTokenAvailable() {
		if count > 100 {
			log.Printf("too many loop.\n analyzer = %s\n", p)
			panic("loop lasted more than expected on 'expression definition analysis'")
		}

		count++
		symbol = p.peek()

		switch symbol.ID {
		case lexer.STRING:
			processedToken = append(processedToken, symbol)
			processedTypes = append(processedTypes, TYPE_STRING)

			p.nextToken()
		case lexer.NUMBER:
			processedToken = append(processedToken, symbol)
			processedTypes = append(processedTypes, TYPE_INT)

			p.nextToken()
		case lexer.FUNCTION:
			// Doe the function exist ??
			symbolType := TYPE_INVALID

			functionName := string(symbol.Value)
			def := p.file.GetSingleFunctionDefinition(functionName)

			if def == nil {
				err := parser.NewParseError(symbol, errors.New("undefined function"))
				errs = append(errs, err)

				symbolType = TYPE_INVALID
			} else {
				symbolType = def.ReturnTypes[0]
				listOfUsedFunctions[functionName] = def

				// in case the function that require arguments is placed in the middle of the expression, append an error
				if p.index > 0 && len(def.ParameterTypes) > 0 {
					err := parser.NewParseError(symbol, errors.New("function expect argument but received none. perhaps you wanted to use 'sub-expression' with parenthesis"))
					errs = append(errs, err)
				}
			}

			// Add token and types to tracker
			processedToken = append(processedToken, symbol)
			processedTypes = append(processedTypes, symbolType)

			p.nextToken()
		case lexer.DOLLAR_VARIABLE, lexer.GROUP:
			// TODO: check 'DOLLAR_VARIABLE' for 'method' when type system will be better established
			// do the var/method exist ?
			// 
			// is it defined ?? (can be a variable, or a method)

			symbolType := TYPE_INVALID

			variableName := string(symbol.Value)
			usedVariables[variableName] = true

			index := strings.IndexByte(variableName, '.')
			if index > 1 {
				variableName = variableName[:index]
			}

			// var def *VariableDefinition
			defLocal, foundLocal := localVariables[variableName]
			defGlobal, foundGlobal := globalVariables[variableName]

			if foundLocal {
				symbolType = defLocal.Type
			} else if foundGlobal {
				symbolType = defGlobal.Type
			} else if strings.HasPrefix(variableName, "$.") {
				symbolType = TYPE_ANY
			} else {
				err := parser.NewParseError(symbol, errors.New("undefined variable"))
				errs = append(errs, err)
			}

			processedToken = append(processedToken, symbol)
			processedTypes = append(processedTypes, symbolType)

			p.nextToken()
		case lexer.DOT_VARIABLE:
			// TODO: check 'DOT_VARIABLE' when type system will be better established
			// No check for variable definition
			// but check for variable type when type system is ready
			// but if method, make sure the type-check is going
			// 
			// since no existance and type check can be done due to the weak type system available right now,
			// make type = "any" for now
			processedToken = append(processedToken, symbol)
			processedTypes = append(processedTypes, TYPE_ANY)

			p.nextToken()
		case lexer.LEFT_PAREN:
			// skip opening token
			// count++
			startRange := p.peek().Range
			p.nextToken()

			// open new recursive func to handle the rest of input func(startIndex, fullSlice, ) (count, types, errs)
			// count, groupType, lastToken, localErrs := makeSymboleDefinitionAnalysis(count, symbols, localVariables, globalVariables, file)
			groupType, localErrs := p.makeSymboleDefinitionAnalysis(localVariables, globalVariables)
			errs = append(errs, localErrs...)

			// skip closing token 
			symbol = p.peek()

			if symbol.ID != lexer.RIGTH_PAREN {
				err := parser.NewParseError(symbol, errors.New("expected ')'"))
				errs = append(errs, err)
			}

			endRange := p.peek().Range
			// count++
			p.nextToken()
			// adjust current index for next symbol analysis
			// create a new 'token' and types that represent the returned value, so that analysis continue as if that group was in fact a mere token
			newSymbol := &lexer.Token{ ID: lexer.GROUP, Value: []byte("(PARENT_GROUP)"), Range: startRange, } 
			newSymbol.Range.End = endRange.End

			processedToken = append(processedToken, newSymbol)
			processedTypes = append(processedTypes, groupType)
		case lexer.RIGTH_PAREN:
			// do not skip this token
			// return to previous function, but
			// before return control, make analysis of all computed tokens and types so far
			// 
			// 
			// all token have been found for this sub-expression
			// exprRange := symbols[count].Range
			exprRange := p.peek().Range
			exprRange.Start = p.peekAt(startIndex).Range.Start
			// exprRange.End = symbol.Range.End

			groupType, localErrs := checkSymbolType(processedToken, processedTypes, exprRange, listOfUsedFunctions)
			errs = append(errs, localErrs...)

			return groupType, errs
			// only return overall 'type' and 'err'
		default:
			log.Printf("unexpectd token type. token = %#v\n", symbol.String())
			panic("unexpected token type to parse during 'definition analysis' on Expression. tok type = " + symbol.ID.String())
		}
	}

	exprRange := p.rangeExpression

	groupType, localErrs := checkSymbolType(processedToken, processedTypes, exprRange, listOfUsedFunctions)
	errs = append(errs, localErrs...)

	return groupType, errs
}

// This function assume every tokens have been processed correctly
// Thus if the first token of an expression have been omitted because it was not define, 
// it is not the responsibility of this function to handle it
// In fact, the parent function should have not called this, and handle the problem otherwise
// Best results is when the definition analysis worked out properly
//
// 1. Never call this function if the first token (function or varialbe) of the expression is invalid (definition analysis failed)
// 2. If tokens in the middle failed instead (the definition analysis), send the token over anyway, but with 'invalid type', 
// the rest will be handled by this function; the hope to always have an accurate depiction of the length of arguments for function/method
// and handle properly the type mismatch
// 3. Send over the file data or at least definition for all function that will be used (WIP for documentation of this behavior)
// 
// In conclusion, this function must always be called with all the token present in the expression, otherwise you will get inconsistant result
// If there is token that are not valid, just alter its type to 'invalid type'
// But as stated earlier, if the first token is rotten, dont even bother calling this function
// 
func checkSymbolType(tokens []*lexer.Token, types []string, exprRange lexer.Range, listOfUsedFunctions map[string]*FunctionDefinition) (exprType string, errs []lexer.Error) {
	if len(tokens) != len(types) {
		log.Printf("length mismatch 'tokens' and 'types' slices. \n tokens = %s \n types = %s \n")
		panic("the pair 'tokens' and its 'types' slices cannot have a length mismatch")
	}

	if len(tokens) == 0 {
		err := parser.NewParseError(&lexer.Token{}, errors.New("empty value for parenthis"))
		err.Range = exprRange
		errs = append(errs, err)

		return TYPE_VOID, errs
	} else if len(tokens) == 1 {
		tok := tokens[0]

		if types[0] == TYPE_INVALID {
			return TYPE_INVALID, nil
		}

		// 1. No params function
		if tok.ID == lexer.FUNCTION {
			def := listOfUsedFunctions[string(tok.Value)]

			if def == nil {
				panic("once a function is used within the current expression, it should be kept for type analysis as well")
			}

			size := len(def.ParameterTypes)

			if size > 0 {
				err := parser.NewParseError(tok, errors.New("expected " + strconv.Itoa(size) + " arguments but found none"))
				errs = append(errs, err)
			}

			return types[0], errs
		}


		// TODO: 2. method (dollar & dot varialble) not yet possible because type system in its infancy
		if tok.ID == lexer.DOLLAR_VARIABLE {
		}

		return types[0], nil
	}

	// This assume that len(tokens) > 1
	if types[0] == TYPE_INVALID {
		return TYPE_INVALID, nil
	}

	var functionDef *FunctionDefinition
	tok := tokens[0]

	switch tok.ID {
	case lexer.STRING, lexer.NUMBER, lexer.GROUP, lexer.DOT_VARIABLE, lexer.DOLLAR_VARIABLE:

		//	TODO: This assumption is totally wrong for 'method' (dollar and dot variable). So change it when type-system will be in a better place
		err := parser.NewParseError(tok, errors.New("expected a function"))
		errs = append(errs, err)

		return TYPE_INVALID, errs

	case lexer.FUNCTION:
		functionDef = listOfUsedFunctions[string(tok.Value)]

		if functionDef == nil {
			panic("first token in sub-expression cannot be a function that is both of 'valid type' and 'function definition == nil'")
		}

		difference := len(tokens[1:]) - len(functionDef.ParameterTypes)

		if difference > 0 {
			err := parser.NewParseError(tok, errors.New(strconv.Itoa(difference) + " more arguments than expected"))
			errs = append(errs, err)

			return types[0], errs
		} else if difference < 0 {
			err := parser.NewParseError(tok, errors.New(strconv.Itoa(-difference) + " fewer arguments than expected"))
			errs = append(errs, err)

			return types[0], errs
		}
		
	default:
		panic("unexpected type found during expression type checking on parse tree.\n faulty token = %s\n" + tok.String())
	}

	// TODO: there are some function that accept varialbe length argument, handle them later
	// eg. func dict (args ...int) string
	// 
	// Type check on every argument type against its corresponding parameter type
	sizeExpectedParameterTypes := len(functionDef.ParameterTypes)

	var argToken *lexer.Token
	var argType string

	for count := 1; count < len(tokens); count++ {	// skip the first token since it is the function/method name

		argToken = tokens[count]
		argType = types[count]		// safe to use this index, look at the begenning of the function

		if count - 1 >= sizeExpectedParameterTypes {
			err := parser.NewParseError(argToken, errors.New(strconv.Itoa(count - sizeExpectedParameterTypes) + " more arguments than expected"))
			errs = append(errs, err)
			break 
		}

		expectedType := functionDef.ParameterTypes[count - 1]	// safe to use this index, look within switch case 'lexer.FUNCTION'

		if expectedType == "" || argType == "" {
			log.Printf("empty types, func = %s\n argType = %s ::: expectedType = %s\n", functionDef, argType, expectedType)
			panic("argument and parameter type cannot be empty during type analysis")
		}

		if expectedType == TYPE_ANY || argType == TYPE_ANY || expectedType == TYPE_INVALID || argType == TYPE_INVALID {
			continue
		}

		// if expectedType != argType {
		if ! matchTypeWithHeuristicStrategy(expectedType, argType) {
			err := parser.NewParseError(argToken, errors.New("argument type mismatch, expected '" + expectedType + "' but got '" + argType + "'"))
			errs = append(errs, err)
		}
	}

	return types[0], errs
}

// ------------------
// ------------------
// New Functions Area
// ------------------
// ------------------

type astVisitor struct {
	file				*FileDefinition
	fileSet			*token.FileSet
	comment			*parser.CommentNode
	parentScope		*parser.GroupStatementNode
	functionsList	map[string]parser.AstNode
	header			string

	function			*FunctionDefinition
	errs				[]lexer.Error
}

func NewWalker(comment *parser.CommentNode, parentScope *parser.GroupStatementNode, file *FileDefinition, fileSet *token.FileSet, header string) *astVisitor {
	walker := &astVisitor{
		file: file,
		fileSet: fileSet,
		comment: comment,
		parentScope: parentScope,
		functionsList: file.GetFunctions(),
		header: header,
	}

	return walker;
}

func (v* astVisitor) Visit (node ast.Node) ast.Visitor {
	switch n := node.(type) {
	case *ast.File:
		return v
	case *ast.FuncDecl:
		function := &FunctionDefinition{}
		function.Node = v.comment
		function.Name = n.Name.Name
		function.IsValid = true
		function.ReturnTypes = [2]string {TYPE_VOID, TYPE_ERROR}		// Default return type

		startPos := v.fileSet.Position(n.Name.Pos())
		endPos := v.fileSet.Position(n.End())
		distance := goAstPositionToRange(startPos, endPos)

		function.Range = remapRangeFromCommentGoCodeToSource(v.header, v.comment.Range, distance)
		v.function = function

		_, found := v.functionsList[function.Name]
		if found {
			err := parser.NewParseError(&lexer.Token{}, errors.New("function redeclaration"))
			err.Range = function.Range
			v.errs = append(v.errs, err)

			return nil
		}

		if v.parentScope != v.file.Root {
			err := parser.NewParseError(&lexer.Token{}, errors.New("function type-hint cannot be done in local scope, only on global scope"))
			err.Range = function.Range
			v.errs = append(v.errs, err)

			return nil
		}

		v.functionsList[function.Name] = function.Node
		v.file.Functions = append(v.file.Functions, function)

		log.Printf("--> function Range : %s\n", v.function.Range)

		return v
	case *ast.FuncType:
		var err error

		var paramType string
		buf := new(bytes.Buffer)

		if n.Params != nil {
			for _, param := range n.Params.List {
				buf.Reset()

				err = printer.Fprint(buf, v.fileSet, param.Type)
				if err != nil {
					v.function.IsValid = false
					log.Println("found invalid type while inspecting ast from comment go code")
				}

				// TODO: check that the type exist (only possible when type-system will be completed)
				paramType = buf.String()

				for _, name := range param.Names {
					// TODO: check that 'name' is not a go keyword (string, int, ...)
					v.function.ParameterNames = append(v.function.ParameterNames, name.Name)
					v.function.ParameterTypes = append(v.function.ParameterTypes, paramType)
				}

				if len(param.Names) == 0 {
					v.function.ParameterNames = append(v.function.ParameterNames, "")
					v.function.ParameterTypes = append(v.function.ParameterTypes, paramType)
				}
			}
		}

		if n.Results != nil {
			for count, ret := range n.Results.List {
				if count > 2 {
					err := parser.NewParseError(&lexer.Token{}, errors.New("function's return values cannot be greater than 2"))
					err.Range = v.function.Range
					v.errs = append(v.errs, err)

					break
				}

				buf.Reset()

				err = printer.Fprint(buf, v.fileSet, ret.Type)
				if err != nil {
					v.function.IsValid = false
					log.Println("found invalid type while inspecting ast from comment go code")
				}

				// TODO: check that the type exist (only possible when type-system will be completed)
				paramType = buf.String()

				v.function.ReturnTypes[count] = paramType
			}

			// Check return type for validation
			if v.function.ReturnTypes[1] == "" {
				v.function.ReturnTypes[1] = TYPE_VOID
			} else if v.function.ReturnTypes[1] != TYPE_ERROR {
				err := parser.NewParseError(&lexer.Token{}, errors.New("the second return type can either be empty or an 'error' type"))
				err.Range = v.function.Range

				v.errs = append(v.errs, err)
			}

			if v.function.ReturnTypes[0] == "" {
				v.function.ReturnTypes[0] = TYPE_VOID
			}
		}

		return nil
	}	// End switch n := node.(type)

	return nil
}

// TODO: Review the 'Range' value, they are incorrect since they do not
// take into consideration the source file outside the comment go code
func inspectFunctionsWithinCommentGoCode(comment *parser.CommentNode, parent *parser.GroupStatementNode, file *FileDefinition, fileSet *token.FileSet, header string) func(node ast.Node) bool {
	// functions := file.ScopeToFunctions[parent]

	return func(node ast.Node) bool {
		return false
	}
}

// TODO: Review the 'Range' value, they are incorrect since they do not
// take into consideration the source file outside the comment go code
func inspectDataStructuresWithinCommentGoCode(comment *parser.CommentNode, fileSet *token.FileSet) func(node ast.Node) bool {
	panic("not implemented yet")
}

// it removes the added header from the 'position' count
func remapRangeFromCommentGoCodeToSource(header string, boundary, target lexer.Range) lexer.Range {
	maxLineInHeader := 0

	for _, char := range []byte(header) {
		if char == '\n' {
			maxLineInHeader++
		}
	}

	rangeRemaped := lexer.Range{}

	// NOTE: because of go/parser, 'target.Start.Line' always start at '1'
	rangeRemaped.Start.Line = boundary.Start.Line + target.Start.Line - 1 - maxLineInHeader
	rangeRemaped.End.Line = boundary.Start.Line + target.End.Line - 1 - maxLineInHeader

	rangeRemaped.Start.Character = target.Start.Character - 1
	rangeRemaped.End.Character = target.End.Character - 1

	if target.Start.Line - maxLineInHeader == 1 {
		rangeRemaped.Start.Character = boundary.Start.Character + len(header) + target.Start.Character
	}

	if target.End.Line - maxLineInHeader == 1 {
		rangeRemaped.End.Character = boundary.Start.Character + len(header) + target.End.Character
	}

	if rangeRemaped.End.Line > boundary.End.Line {
		msg := "boundary.End.Line = %d ::: rangeRemaped.End.Line = %d\n"
		log.Printf(msg, boundary.End.Line, rangeRemaped.End.Line)

		panic("remaped range cannot excede the comment GoCode boundary")
	}

	return rangeRemaped
}
func goAstPositionToRange(startPos, endPos token.Position) lexer.Range {
	distance := lexer.Range{
		Start: lexer.Position{
			Line: startPos.Line,
			Character: startPos.Column,
		},
		End: lexer.Position{
			Line: endPos.Line,
			Character: endPos.Column,
		},
	}

	return distance
}

func NewParseErrorFromErrorList(err *scanner.Error, randomColumnOffset int) *parser.ParseError {
	if err == nil {
		return nil
	}

	if randomColumnOffset < 0 {
		randomColumnOffset = 10
	}

	parseErr := &parser.ParseError{
		Err: errors.New(err.Msg),
		Range: lexer.Range{
			Start: lexer.Position{
				Line: err.Pos.Line,
				Character: err.Pos.Column,
			},
			End: lexer.Position{
				Line: err.Pos.Line,
				Character: err.Pos.Column + randomColumnOffset,
			},
		},
	}

	return parseErr
}

func matchTypeWithHeuristicStrategy(one, two string) bool {
	if one == TYPE_ANY || two == TYPE_ANY {
		return true
	}

	if one == two {
		return true
	}

	if one == TYPE_INVALID || two == TYPE_INVALID {
		return false
	}

	return false
}

func getZeroRangeValue() lexer.Range {
	return lexer.Range{}
}


//
//
//
// LSP-like helpter functions 
//
//
//

// TODO: rename to 'findTargetNodeInfo()', 
// TODO: not yet implemented, this code is buggy and not tested
func findLeadWithinAst (root parser.AstNode, position lexer.Position) (node parser.AstNode, name string, isTemplate bool) {
	if root == nil {
		return nil, "", false
	}

	if ! root.GetRange().Contains(position) {
		return nil, "", false
	}

	switch r := root.(type) {
	case *parser.GroupStatementNode:
		if r.ControlFlow.GetRange().Contains(position) {

			node, name, isTemplate = findLeadWithinAst(r.ControlFlow, position)
			if _, ok := node.(*parser.GroupStatementNode); ! ok {
				node = r
			}

			return node, name, isTemplate
		}

		for _, statement := range r.Statements {
			if ! statement.GetRange().Contains(position) {
				continue
			}

			node, name, isTemplate = findLeadWithinAst(statement, position)
			if _, ok := node.(*parser.GroupStatementNode); ! ok {
				node = r
			}

			return node, name, isTemplate
		}

		return nil, "", false
	case *parser.TemplateStatementNode:
		if r.Kind == parser.KIND_DEFINE_TEMPLATE || r.Kind == parser.KIND_BLOCK_TEMPLATE {
			return nil, "", false
		}

		if r.TemplateName == nil {
			return nil, "", false
		}

		if r.TemplateName.Range.Contains(position) {
			return r.Parent(), string(r.TemplateName.Value), true
		}

		if r.Expression == nil {
			return nil, "", false
		}

		if r.Expression.GetRange().Contains(position) {
			return findLeadWithinAst(r.Expression, position)
		}

		return nil, "", false

	case *parser.VariableDeclarationNode:
		for _, variable := range r.VariableNames {
			if ! variable.Range.Contains(position) {
				continue
			}

			return r, string(variable.Value), false
		}

		if r.Value.GetRange().Contains(position) {
			return findLeadWithinAst(r.Value, position)
		}

		return nil, "", false
	case *parser.VariableAssignationNode:
		if r.VariableName.Range.Contains(position) {
			return r, string(r.VariableName.Value), false
		}

		if r.Value.GetRange().Contains(position) {
			return findLeadWithinAst(r.Value, position)
		}

		return nil, "", false
	case *parser.MultiExpressionNode:
		for _, expression := range r.Expressions {
			if ! expression.GetRange().Contains(position) {
				continue
			}

			return findLeadWithinAst(expression, position)
		}

		return nil, "", false
	case *parser.ExpressionNode:
		for _, symbol := range r.Symbols {
			if ! symbol.Range.Contains(position) {
				continue
			}

			return r, string(symbol.Value), false
		}

		return nil, "", false
	case *parser.CommentNode:
		return nil, "", false
	default: 
		panic("unexpected AstNode type while finding corresponding node with a particular position")
	}
}

/*
func reverseFindDefinitionWithinFile(leave *parser.GroupStatementNode, nameToFind string, isTemplate bool) (foundNode parser.AstNode, reach lexer.Range) {
	panic("not implemented yet")

	if leave == nil {
		return nil, getZeroRangeValue()
	}

	// 0. look if user defined template
	if isTemplate {
		// TODO: there is a function that find all top level 'template' from a node, use it here (but it is at a higher module/gota)
	}

	// 1. look if variable
	for _, varialbe := range leave.Variables {
		if varialbe.Name == nameToFind {
			return varialbe.Node, varialbe.Range
		}
	}

	// 2. look if function (comment)
	// ....

	return reverseFindDefinitionWithinFile(leave.Parent(), nameToFind, isTemplate)
}
*/

func goToDefinitionForFileNodeOnly(position lexer.Range) (node parser.AstNode, reach lexer.Range) {
	panic("not implemented yet")
}

