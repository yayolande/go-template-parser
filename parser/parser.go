package parser

import (
	"bytes"
	"fmt"
	"go-template-parser/lexer"
)

// TODO: Refactor this to 'parser' package
type ParseError struct {
	Err	error
	Range	lexer.Range
}


var (
	input []lexer.Token
	OpenedNodeStack []AstNode
)

type SymbolDefinition map[string]AstNode

// Shallow clone, only to used as a container
func cloneSymboleDefinitions(src SymbolDefinition) SymbolDefinition {
	dst := make(map[string]AstNode)

	for key, val := range map[string]AstNode(src) {
		dst[key] = val
	}

	return SymbolDefinition(dst)
}

type SemanticAnalizer struct {
	rootAstNode	AstNode
	builtinFunctionDefinition	SymbolDefinition
	customFunctionDefinition	SymbolDefinition	// Not sure it is necessary
	variableDefinition	SymbolDefinition
}

func createSemanticAnalizer(rootNode AstNode) *SemanticAnalizer {
	analizer := SemanticAnalizer{
		rootAstNode: rootNode,
		builtinFunctionDefinition: SymbolDefinition {
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
		},
		customFunctionDefinition: SymbolDefinition {
		},	// ????????????????????????/
		variableDefinition: SymbolDefinition {
			".": nil,
			"$": nil,
		},
	}

	return &analizer
}

func (a *SemanticAnalizer) definitionAnalysis() {
	root := a.rootAstNode
	root.definitionAnalysis(a.variableDefinition, a.builtinFunctionDefinition)

	_ = root
}

func SemanticalAnalisis(root AstNode) {
	analizer := createSemanticAnalizer(root)
	_ = analizer

	analizer.definitionAnalysis()
}

func Parse(tokens []lexer.Token) AstNode {
	input = make([]lexer.Token, len(tokens))
	copy(input, tokens)

	nodes := []AstNode{}

	for len(input) > 0 {
		node := StatementParser()
		nodes = append(nodes, node)
		// fmt.Printf("\n %#v \n", node)
		// fmt.Printf("\n input = %s \n", input)
	}

	defaultGroupStatementNode := GroupStatementNode{
		Kind: KIND_GROUP_STATEMENT,
		Statements: nodes,
	}

	fmt.Println(defaultGroupStatementNode)

	return defaultGroupStatementNode
}


func StatementParser() AstNode {
	// TODO: Group Statement implementation missing

	// if acceptAt(1, ":=") {
	if acceptAt(1, lexer.ASSIGNEMENT_DEFINITION) {
		// ID := expression
		variable := peek()
		nextToken()

		nextToken()	// ":="

		expression := multiExpressionParser()

		varDeclarationNode := VariableDeclarationNode {
			Kind: KIND_VARIABLE_DECLARATION,
			VariableName: variable,
			Value: expression,
		}

		nextToken()	// consume/skip EOL token
		// expect(EOL)
		// found = true
		// fmt.Println(varDeclarationNode)
		
		return varDeclarationNode

	} else if acceptAt(1, lexer.ASSIGNEMENT) {
		// TODO
		variable := peek()

		nextToken()

		nextToken()	// "="

		expression := multiExpressionParser()

		varAssignation := VariableAssignationNode{
			Kind: KIND_VARIABLE_ASSIGNMENT,
			VariableName: variable,
			Value: expression,
		}
		// found = true

		nextToken()	// consume/skip EOL token
		// expect(EOL)
		// found = true

		return varAssignation
	} else {	// Define block type statement (identifiable with the help of language 'keywords')
		// if, end, else, define, template, ...
		// OpenedNodeStack ++
		tokenValue := peek()

		// TODO: NOT IMPLEMENTED YET !!!!!
		if bytes.Compare(tokenValue, []byte("if")) == 0 {
			// {{ if ... }}
			nextToken()	// skip keyword "if"

			multiExpressionParser()

			// found = true
			return ExpressionNode{}

		}
		if bytes.Compare(tokenValue, []byte("end")) == 0 {
			// {{ end }}
			accept(lexer.EOL)
			nextToken()
			// found = true
			return ExpressionNode{}
		}
	}

	expression := multiExpressionParser()

	nextToken()	// consume/skip EOL token
	// expect(EOL)
	// found = true

	// endOfragment = Token { ID: EOL, Value: []byte("#EOF") }
	// accept("#EOF")
	// for ! accept(getEndOfLineValue()) {

	return expression
}

func multiExpressionParser() MultiExpressionNode {
	expression := MultiExpressionNode{}
	expression.Kind = KIND_MULTI_EXPRESSION

	expr := expressionParser()
	expression.Expressions = append(expression.Expressions, expr)

	// for accept("|") {
	for accept(lexer.PIPE) {
		nextToken()

		expr := expressionParser()
		expression.Expressions = append(expression.Expressions, expr)
	}

	return expression
}

func expressionParser() ExpressionNode {
	expression := ExpressionNode{}
	expression.Kind = KIND_EXPRESSION

	count := 0
	for accept(lexer.IDENTIFIER) || accept(lexer.VARIABLE) || accept(lexer.STRING) {
		name := peek()
		name = bytes.Clone(name)

		if count == 0 {
			expression.FunctionName = name
		} else {
			expression.ArgumentsName = append(expression.ArgumentsName, name)
		}

		count++
		nextToken()
	}

	return expression
}

func peek() []byte {
	return bytes.Clone(input[0].Value)
}

func peekAt(pos int) []byte {
	if pos < len(input) {
		return bytes.Clone(input[pos].Value)
	}

	return nil
}

func lookAhead() {
}

func nextToken() {
	input = input[1:]
}

/*
func accept(str string) bool {
	if bytes.Compare(input[0].Value, []byte(str)) == 0 {
		return true
	}

	return false
}
*/

func accept(kind lexer.Kind) bool {
	return input[0].ID == kind
}

func acceptAt(pos int, kind lexer.Kind) bool {
	if pos >= len(input) {
		return false
	}

	return input[pos].ID == kind
}

/*
func acceptAt(pos int, str string) bool {
	if pos >= len(input) {
		return false
	}

	if bytes.Compare(input[pos].Value, []byte(str)) == 0 {
		return true
	}

	return false
}
*/

func expect() {
}

func getEndOfLineValue() string {
	return ("#EOL")
}


