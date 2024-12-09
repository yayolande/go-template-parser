package types

import (
)

// ----------------------
// Lexer Types definition
// ----------------------

type Position struct {
	Line			int
	Character	int
}

type Range struct {
	Start	Position
	End	Position
}

type LexerKind int

type Token struct {
	ID		LexerKind
	Range	Range
	Value	[]byte
}

// -----------------------
// Parser Types definition
// -----------------------

type ParserKind int
type SymbolDefinition map[string]AstNode

// TODO: add 'Stringer' method for FunctionDefinition, VariableDefinition, DataStructureDefinition
type FunctionDefinition struct {
	Node				AstNode
	Range				Range
	Name				string
	ParameterNames	[]string
	ParameterTypes	[]string
	ReturnTypes		[]string
	IsValid			bool
}

type VariableDefinition struct {
	Node				AstNode
	Range				Range
	Name				string
	Type				string
	IsValid			bool
}

type DataStructureDefinition struct {
	Node			AstNode
	Range			Range
	Name			string
	Fields		[]VariableDefinition
	IsValid		bool
}

type AstNode interface {
	String()	string
	GetKind()	ParserKind
	GetRange()	Range
	SetKind(val ParserKind)
	DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitionsGlobal, templateDefinitionsLocal SymbolDefinition) []Error
	// typeAnalysis()
}

// ------------
// Global Types
// ------------

type Error interface {
	GetError()	string
	GetRange()	Range
	String()		string
}

