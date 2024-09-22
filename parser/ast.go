package parser

import (
	"bytes"
	"errors"
	"go-template-parser/lexer"
)

type Kind int

const (
	KIND_VARIABLE_DECLARATION	Kind = iota
	KIND_VARIABLE_ASSIGNMENT
	KIND_EXPRESSION
	KIND_MULTI_EXPRESSION
	KIND_GROUP_STATEMENT
)

type AstNode interface {
	String()	string
	getKind() Kind
	definitionAnalysis(varDefinition, functionDefinition SymbolDefinition) []lexer.ParseError
	// typeAnalysis()
}

type VariableDeclarationNode struct {
	Kind	Kind
	VariableName	[]byte
	// Value	AstNode	// of type expression
	Value	MultiExpressionNode	// of type expression
}

func (v VariableDeclarationNode) getKind() Kind {
	return v.Kind
}

func (v VariableDeclarationNode) definitionAnalysis(varDefinition, functionDefinition SymbolDefinition) []lexer.ParseError {
	// TODO:  Move this somewhere else, this is a syntax analysis, not a semantical (definitionAnalysis) analysis
	// 0. Check the syntax of the variable, if it is valid
	var errs []lexer.ParseError

	if v.VariableName[0] != byte('$') {
		errMsg := errors.New("variable name must start with '$' to be declared")
		err := lexer.ParseError{
			Err: errMsg,
			Range: lexer.Range{},
		}

		errs = append(errs, err)
		// errs = appendError(errs, err)
	}

	// TODO:  Move this somewhere else, this is a syntax analysis, not a semantical (definitionAnalysis) analysis
	if bytes.ContainsAny(v.VariableName, ".,") {
		errMsg := errors.New("variable name cannot contains any special character such '.', ',' ...")
		err := lexer.ParseError{
			Err: errMsg,
			Range: lexer.Range{},
		}

		errs = append(errs, err)
		// errs = appendError(errs, err)
	}

	// 1. Check if variable is defined, if not define it
	if errs ==  nil {
		key := string(v.VariableName)
		_, found := varDefinition[key]

		if found {
			errMsg := errors.New("variable already exists, can't redeclare it")
			err := lexer.ParseError{
				Err: errMsg,
				Range: lexer.Range{},
			}

			errs = append(errs, err)
			// errs = appendError(errs, err)
		} else {
			varDefinition[key] = AstNode(v)
		}
	}

	// 2. Check that 'expression' is valid
	errLocal := v.Value.definitionAnalysis(varDefinition, functionDefinition)
	errs = append(errs, errLocal...)
	// errs = appendError(errs, errLocal...)

	return errs
}

type VariableAssignationNode struct {
	Kind	Kind
	VariableName	[]byte
	// Value	AstNode	// of type expression
	Value	MultiExpressionNode	// of type expression
}

func (v VariableAssignationNode) getKind() Kind {
	return v.Kind
}

func (v VariableAssignationNode) definitionAnalysis(varDefinition, functionDefinition SymbolDefinition) []lexer.ParseError {
	return nil
}

type MultiExpressionNode struct {
	Kind
	Expressions	[]ExpressionNode
}

func (v MultiExpressionNode) getKind() Kind {
	return v.Kind
}

func (v MultiExpressionNode) definitionAnalysis(varDefinition, functionDefinition SymbolDefinition) []lexer.ParseError {
	var errs, localErr []lexer.ParseError

	for _, expression := range v.Expressions {
		localErr = expression.definitionAnalysis(varDefinition, functionDefinition)
		// WARNING: What will happen if 'localErr == nil' ? Or if 'len(localErr) == 0'
		errs = append(errs, localErr...)
		// errs = appendError(errs, localErr...)
	}

	return errs
}

type ExpressionNode struct {
	Kind
	FunctionName	[]byte
	ArgumentsName	[][]byte
	// TODO: Vet them. Not sure of those below
	isError			bool
	isFunctionCall	bool
}

func (e ExpressionNode) getKind() Kind {
	return e.Kind
}


// TODO: 'ParseError.Range' is not properly implemented
func (v ExpressionNode) definitionAnalysis(varDefinition, functionDefinition SymbolDefinition) []lexer.ParseError {
	// 1. Check FunctionName is legit (var or func)
	// create local variable that will hold all variable and found found in this process 'definitionAnalysis'
	// so that 'typeAnalysis' will use that instead
	var errs []lexer.ParseError

	v.isFunctionCall = false
	name := string(v.FunctionName)

	_, found := varDefinition[name]
	if found {
		if len(v.ArgumentsName) > 0 {
			errMsg := errors.New("Variable cannot have arguments, only function can. Remove the extraneous argument, or use a function instead")
			err := lexer.ParseError{
				Err: errMsg,
				Range: lexer.Range{},
			}

			errs = append(errs, err)
			// errs = appendError(errs, err)
			v.isError = true
		}
	} else {
		v.isFunctionCall = true

		_, found = functionDefinition[name]
		if !found {

			errMsg := errors.New("Cannot use a function or variable that hasn't been declared")
			err := lexer.ParseError{
				Err: errMsg,
				Range: lexer.Range{},
			}

			errs = append(errs, err)
			// errs = appendError(errs, err)
			v.isError = true
			v.isFunctionCall = false
		}
	}

	// 2. check argument validity
	for _, argName := range v.ArgumentsName {
		name = string(argName)

		_, found = varDefinition[name]
		if found {
			continue
		}

		// TODO: Check if it is a 'string' or a 'number'

		// check if it is a function
		_, found = functionDefinition[name]
		if !found {
			errMsg := errors.New("argument not declared anywhere")
			err := lexer.ParseError{
				Err: errMsg,
				Range: lexer.Range{},
			}

			errs = append(errs, err)
			// errs = appendError(errs, err)
			v.isError = true
		}
	}

	return errs
}

type GroupStatementNode struct {
	Kind
	Statements	[]AstNode
}

func (g GroupStatementNode) getKind() Kind {
	return g.Kind
}

func (v GroupStatementNode) definitionAnalysis(varDefinition, functionDefinition SymbolDefinition) []lexer.ParseError {
	scopedVarDefinition := cloneSymboleDefinitions(varDefinition)
	scopedFunctionDefinition := cloneSymboleDefinitions(functionDefinition)

	var errs []lexer.ParseError = nil
	var localErr []lexer.ParseError = nil

	for _, statement := range v.Statements {
		localErr = statement.definitionAnalysis(scopedVarDefinition, scopedFunctionDefinition)
		// WARNING: What will happen if 'localErr == nil' ? Or if 'len(localErr) == 0'
		errs = append(errs, localErr...)
		// errs = appendError(errs, localErr...)
	}

	return errs
}

func appendError(errs []lexer.ParseError, err ...lexer.ParseError) []lexer.ParseError {
	if err == nil {
		return errs
	}

	// Not sure about this !!!!
	var newErr []lexer.ParseError
	for _, e := range err {
		if e.Err != nil {
			newErr = append(newErr, e)
		}
	}

	err = newErr
	return append(errs, err...)
}

