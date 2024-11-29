package types

import (
)

// ----------------------
// Lexer Types definition
// ----------------------

type Position struct {
	Line	int
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

type AstNode interface {
	String()	string
	GetKind()	ParserKind
	GetRange()	*Range
	SetKind(val ParserKind)
	DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitions SymbolDefinition) []Error
	// typeAnalysis()
}

// ------------
// Global Types
// ------------

type Error interface {
	GetError()		string
	GetRange() *Range
	String()		string
}

