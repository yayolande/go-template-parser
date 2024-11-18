package parser

import (
	"bytes"
	"errors"
	"go-template-parser/lexer"
	"maps"
)

type Kind int

const (
	KIND_VARIABLE_DECLARATION	Kind = iota
	KIND_VARIABLE_ASSIGNMENT
	KIND_EXPRESSION
	KIND_MULTI_EXPRESSION
	KIND_COMMENT

	KIND_GROUP_STATEMENT

	KIND_IF
	KIND_ELSE_IF
	KIND_ELSE

	KIND_WITH
	KIND_ELSE_WITH

	KIND_RANGE_LOOP

	KIND_DEFINE_TEMPLATE
	KIND_BLOCK_TEMPLATE
	KIND_USE_TEMPLATE

	KIND_END
)

//go:generate go run ./generate.go
type AstNode interface {
	String()	string
	GetKind()	Kind
	GetRange()	*lexer.Range
	SetKind(val Kind)
	// TODO: refactor this, very updated
	DefinitionAnalysis(globalVariables, localVariables, functionDefinitions SymbolDefinition) []ParseError
	// typeAnalysis()
}

type VariableDeclarationNode struct {
	Kind	Kind
	Range	lexer.Range
	VariableNames	[]lexer.Token
	Value	*MultiExpressionNode	// of type expression
}

func (v VariableDeclarationNode) GetKind() Kind {
	return v.Kind
}

func (v VariableDeclarationNode) GetRange() *lexer.Range {
	return &v.Range
}

func (v *VariableDeclarationNode) SetKind(val Kind) {
	v.Kind = val
}

func (v VariableDeclarationNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions SymbolDefinition) []ParseError {
	var errs []ParseError

	// 0. Check that 'expression' is valid
	if v.Value != nil {
		errLocal := v.Value.DefinitionAnalysis(globalVariables, localVariables, functionDefinitions)
		errs = append(errs, errLocal...)
	}

	// 1. Check that variable is not a sub-variable (property)
	if len(v.VariableNames) == 0 {
		errLocal := ParseError{ Err: errors.New("unexpected error, somehow the variable name hasn't been defined"), Range: v.Range }
		errs = append(errs, errLocal)

		return errs
	}

	for _, variable := range v.VariableNames {
		// if bytes.ContainsAny(v.VariableName.Value, ".") {
		if bytes.ContainsAny(variable.Value, ".") {
			err := createParseError(&variable, 
				errors.New("variable name cannot contains any special character such '.', ..."))

			errs = append(errs, *err)
		}

		// 2. Check if variable is defined, if not define it
		key := string(variable.Value)
		_, isLocal := localVariables[key]

		if isLocal {
			err := createParseError(&variable, errors.New("variable already exists, can't redeclare it"))
			errs = append(errs, *err)
		} else {
			localVariables[key] = AstNode(&v)
		}
	}


	return errs
}

type VariableAssignationNode struct {
	Kind	Kind
	Range	lexer.Range
	VariableName	*lexer.Token
	// Value	AstNode	// of type expression
	Value	*MultiExpressionNode	// of type expression
}

func (v VariableAssignationNode) GetKind() Kind {
	return v.Kind
}

func (v VariableAssignationNode) GetRange() *lexer.Range {
	return &v.Range
}

func (v *VariableAssignationNode) SetKind(val Kind) {
	v.Kind = val
}

func (v VariableAssignationNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions SymbolDefinition) []ParseError {
	var errs []ParseError

	// 0. Check that 'expression' is valid
	if v.Value != nil {
		errLocal := v.Value.DefinitionAnalysis(globalVariables, localVariables, functionDefinitions)
		errs = append(errs, errLocal...)
	}

	// 1. Check that variable is not a sub-variable (property)
	if v.VariableName == nil {
		errLocal := ParseError{ Err: errors.New("unexpected error, somehow the variable name hasn't been defined"), Range: v.Range }
		errs = append(errs, errLocal)

		return errs
	}

	if bytes.ContainsAny(v.VariableName.Value, ".") {
		err := createParseError(v.VariableName, 
			errors.New("variable name cannot contains any special character such '.', ..."))

		errs = append(errs, *err)
		return errs
	}

	// 2. Check if variable is defined, if not report error
	key := string(v.VariableName.Value)
	_, isLocal := localVariables[key]
	_, isGlobal := globalVariables[key]

	if ! isLocal && ! isGlobal {
		err := createParseError(v.VariableName, errors.New("variable do not exist, declare it first"))
		errs = append(errs, *err)
	}

	return errs
}

type MultiExpressionNode struct {
	Kind
	Range	lexer.Range
	Expressions	[]ExpressionNode
}

func (m MultiExpressionNode) GetKind() Kind {
	return m.Kind
}

func (m MultiExpressionNode) GetRange() *lexer.Range {
	return &m.Range
}

func (m *MultiExpressionNode) SetKind(val Kind) {
	m.Kind = val
}

func (v* MultiExpressionNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions SymbolDefinition) []ParseError {
	var errs, localErr []ParseError

	for _, expression := range v.Expressions {
		localErr = expression.DefinitionAnalysis(globalVariables, localVariables, functionDefinitions)
		errs = append(errs, localErr...)
	}

	return errs
}

type ExpressionNode struct {
	Kind
	Range	lexer.Range
	Symbols []lexer.Token
	// TODO: Vet them. Not sure of those below
	isError			bool
	isFunctionCall	bool
}

func (e ExpressionNode) GetKind() Kind {
	return e.Kind
}

func (e ExpressionNode) GetRange() *lexer.Range {
	return &e.Range
}

func (v *ExpressionNode) SetKind(val Kind) {
	v.Kind = val
}

// TODO: 'ParseError.Range' is not properly implemented
func (v ExpressionNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions SymbolDefinition) []ParseError {
	// 1. Check FunctionName is legit (var or func)
	// create local variable that will hold all variable and found found in this process ('DefinitionAnalysis')
	// so that 'typeAnalysis' will use that instead
	var errs []ParseError

	if len(v.Symbols) == 0 {
		err := createParseError(&lexer.Token{}, errors.New("empty expression is not allowed"))
		errs = append(errs, *err)
		v.isError = true

		return errs
	}

	name := string(v.Symbols[0].Value)
	id := v.Symbols[0].ID
	isDotVariable := name[0] == '.'	
	_, isLocalVariable := localVariables[name]
	_, isGlobalVariable := globalVariables[name]

	// Dot_variable cannot be checked on DefinitionAnalysis(), only on type checking analysis
	if isLocalVariable || isGlobalVariable || isDotVariable {
		if len(v.Symbols) > 1 {
			err := createParseError(&v.Symbols[0], errors.New("Variable cannot have arguments, only function. Remove the extraneous argument, or use a function instead"))
			errs = append(errs, *err)
			v.isError = true

			return errs
		}
	} else if v.Symbols[0].ID == lexer.STRING || v.Symbols[0].ID == lexer.NUMBER {
		if len(v.Symbols) > 1 {
			err := createParseError(&v.Symbols[0], errors.New("'" + id.String() + "' cannot have argument(s)"))
			errs = append(errs, *err)
			v.isError = true

			return errs
		}
	} else {
		v.isFunctionCall = true

		_, isFunction := functionDefinitions[name]
		if !isFunction {
			err := createParseError(&v.Symbols[0], errors.New("Cannot use a '" + id.String() + "' that hasn't been declared"))
			errs = append(errs, *err)
			v.isError = true
			v.isFunctionCall = false
		}
	}

	// 2. check argument validity
	isGlobalVariable = false
	isLocalVariable = false
	isFunction := false
	for _, arg := range v.Symbols[1:] {
		if arg.ID == lexer.STRING || arg.ID == lexer.NUMBER {
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

		_, isFunction = functionDefinitions[name]
		if isFunction {
			continue
		}

		err := createParseError(&arg, errors.New(("argument not declared anywhere")))
		errs = append(errs, *err)

		v.isError = true
	}

	return errs
}

type TemplateStatementNode struct {
	Kind
	Range lexer.Range
	TemplateName	*lexer.Token
	expression	AstNode
}

func (t TemplateStatementNode) GetKind() Kind {
	return t.Kind
}

func (t *TemplateStatementNode) SetKind(val Kind) {
	t.Kind = val
}

func (t TemplateStatementNode) GetRange() *lexer.Range {
	return &t.Range
}

func (v TemplateStatementNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions SymbolDefinition) []ParseError {
	if v.expression != nil {
		return  v.expression.DefinitionAnalysis(globalVariables, localVariables, functionDefinitions)
	}

	// TODO: Do something about checking template name, if it is exist and what not

	return nil
}

type GroupStatementNode struct {
	Kind
	Range	lexer.Range
	// Name	*lexer.Token
	ControlFlow	AstNode
	Statements	[]AstNode
}

func (g GroupStatementNode) GetKind() Kind {
	return g.Kind
}

func (g GroupStatementNode) GetRange() *lexer.Range {
	return &g.Range
}

func (g *GroupStatementNode) SetKind(val Kind) {
	g.Kind = val
}

func (v GroupStatementNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions SymbolDefinition) []ParseError {
	scopedGlobalVariables :=  SymbolDefinition{}
	scopedLocalVariables := SymbolDefinition{}

	maps.Copy(scopedGlobalVariables, globalVariables)

	var errs []ParseError = nil
	var localErr []ParseError = nil
	if v.ControlFlow != nil {
		tmpLocalVariables := SymbolDefinition{}
		tmpGlobalVariables := SymbolDefinition{}
		maps.Copy(tmpGlobalVariables, scopedGlobalVariables)
		maps.Copy(tmpGlobalVariables, localVariables)

		localErr = v.ControlFlow.DefinitionAnalysis(tmpGlobalVariables, tmpLocalVariables, functionDefinitions)
		errs = append(errs, localErr...)

		maps.Copy(scopedLocalVariables, tmpLocalVariables)
	}

	switch v.Kind {
	case KIND_IF, KIND_ELSE, KIND_ELSE_IF:
		// No modification
	case KIND_RANGE_LOOP, KIND_WITH, KIND_ELSE_WITH:
		// Modify only  '.'
		scopedGlobalVariables["."] = v.ControlFlow
	case KIND_DEFINE_TEMPLATE, KIND_BLOCK_TEMPLATE:
		// Modify both '.' and '$'
		scopedGlobalVariables["."] = v.ControlFlow
		scopedGlobalVariables["$"] = v.ControlFlow
	}

	for _, statement := range v.Statements {
		localErr = statement.DefinitionAnalysis(scopedGlobalVariables, scopedLocalVariables, functionDefinitions)
		errs = append(errs, localErr...)
	}

	return errs
}

type CommentNode struct {
	Kind
	Range	lexer.Range
	Value	*lexer.Token
}

func (c CommentNode) GetKind() Kind {
	return c.Kind
}

func (c CommentNode) GetRange() *lexer.Range {
	return &c.Range
}

func (v *CommentNode) SetKind(val Kind) {
	v.Kind = val
}

func (v CommentNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions SymbolDefinition) []ParseError {
	return nil
}
