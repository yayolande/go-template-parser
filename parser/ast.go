package parser

import (
	"log"
	"bytes"
	"errors"

	"github.com/yayolande/gota/lexer"
)

// TODO: enhance the Stringer method for 'GroupStatementNode' and 'CommentNode'
// Since new fields have been added
type SymbolDefinition map[string]AstNode

//go:generate go run ./generate.go
type AstNode interface {
	String()	string
	GetKind()	Kind
	GetRange()	lexer.Range
	SetKind(val Kind)
	DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitionsGlobal, templateDefinitionsLocal SymbolDefinition) []lexer.Error
	// typeAnalysis()
}



// type Kind int
type Kind int

type VariableDeclarationNode struct {
	Kind          Kind
	Range         lexer.Range
	VariableNames []*lexer.Token
	Value         *MultiExpressionNode
}

func (v VariableDeclarationNode) GetKind() Kind {
	return v.Kind
}

func (v VariableDeclarationNode) GetRange() lexer.Range {
	return v.Range
}

func (v *VariableDeclarationNode) SetKind(val Kind) {
	v.Kind = val
}

func (v VariableDeclarationNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitionsGlobal, templateDefinitionsLocal SymbolDefinition) []lexer.Error {
	panic("not useful anymore")
}

type VariableAssignationNode struct {
	Kind         Kind
	Range        lexer.Range
	VariableName *lexer.Token
	// Value	AstNode	// of type expression
	Value *MultiExpressionNode // of type expression
}

func (v VariableAssignationNode) GetKind() Kind {
	return v.Kind
}

func (v VariableAssignationNode) GetRange() lexer.Range {
	return v.Range
}

func (v *VariableAssignationNode) SetKind(val Kind) {
	v.Kind = val
}

func (v VariableAssignationNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitionsGlobal, templateDefinitionsLocal SymbolDefinition) []lexer.Error {
	if v.Kind != KIND_VARIABLE_ASSIGNMENT {
		panic("found value mismatch for 'VariableAssignationNode.Kind' during DefinitionAnalysis()\n" + v.String())
	}

	/*
	if v.Range == getZeroRangeValue() {
		panic("'Range' cannot be empty for VariableAssignationNode.\n" + v.String())
	}
	*/

	if globalVariables == nil || localVariables == nil {
		panic("'localVariables' or 'globalVariables' shouldn't be empty for 'VariableAssignationNode.DefinitionAnalysis()'")
	}

	var errs []lexer.Error

	// 0. Check that 'expression' is valid
	if v.Value != nil {
		errLocal := v.Value.DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitionsGlobal, templateDefinitionsLocal)
		errs = append(errs, errLocal...)
	} else {
		errLocal := NewParseError(nil, errors.New("assignment value cannot be empty"))
		errLocal.Range = v.Range
	}

	// 1. Check at least var is declared
	if v.VariableName == nil {
		errLocal := ParseError{Err: errors.New("empty variable name. syntax should be 'variable = value'"), Range: v.Range}
		errLocal.Range = v.Range

		errs = append(errs, errLocal)
		return errs
	}

	if bytes.ContainsAny(v.VariableName.Value, ".") {
		err := NewParseError(v.VariableName,
			errors.New("variable name cannot contains any special character such '.' while assigning"))

		errs = append(errs, err)
		return errs
	}

	// 2. Check if variable is defined, if not report error
	key := string(v.VariableName.Value)
	_, isLocal := localVariables[key]
	_, isGlobal := globalVariables[key]

	if !(isLocal || isGlobal) {
		err := NewParseError(v.VariableName, errors.New("undefined variable"))
		errs = append(errs, err)
		return errs
	}

	localVariables[key] = &v

	return errs
}

type MultiExpressionNode struct {
	Kind
	Range       lexer.Range
	Expressions []*ExpressionNode
}

func (m MultiExpressionNode) GetKind() Kind {
	return m.Kind
}

func (m MultiExpressionNode) GetRange() lexer.Range {
	return m.Range
}

func (m *MultiExpressionNode) SetKind(val Kind) {
	m.Kind = val
}

func (v *MultiExpressionNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitionsGlobal, templateDefinitionsLocal SymbolDefinition) []lexer.Error {
	panic("not useful anymore")
}

type ExpressionNode struct {
	Kind
	Range   lexer.Range
	Symbols []*lexer.Token
}

func (v ExpressionNode) GetKind() Kind {
	return v.Kind
}

func (v ExpressionNode) GetRange() lexer.Range {
	return v.Range
}

func (v *ExpressionNode) SetKind(val Kind) {
	v.Kind = val
}

func (v ExpressionNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitionsGlobal, templateDefinitionsLocal SymbolDefinition) []lexer.Error {
	panic("not useful anymore")
}

type TemplateStatementNode struct {
	Kind
	Range        lexer.Range
	TemplateName *lexer.Token
	Expression   AstNode
	parent       *GroupStatementNode
}

func (t TemplateStatementNode) GetKind() Kind {
	return t.Kind
}

func (t *TemplateStatementNode) SetKind(val Kind) {
	t.Kind = val
}

func (t TemplateStatementNode) GetRange() lexer.Range {
	return t.Range
}

func (t TemplateStatementNode) Parent() *GroupStatementNode {
	return t.parent
}

func (v TemplateStatementNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitionsGlobal, templateDefinitionsLocal SymbolDefinition) []lexer.Error {
	if templateDefinitionsGlobal == nil || templateDefinitionsLocal == nil {
		panic("templateDefinitionsGlobal/templateDefinitionsLocal shouldn't be empty for 'TemplateStatementNode.DefinitionAnalysis()'")
	}

	if v.TemplateName == nil {
		panic("the template name should never be empty for a template expression. make sure the template has been parsed correctly.\n" + v.String())
	}

	var errs []lexer.Error

	// 1. template name analysis
	switch v.Kind {
	case KIND_USE_TEMPLATE:
		templateName := string(v.TemplateName.Value)

		_, foundGlobal := templateDefinitionsGlobal[templateName]
		_, foundLocal := templateDefinitionsLocal[templateName]

		if  ! foundGlobal && ! foundLocal {
			err := NewParseError(v.TemplateName, errors.New("undefined template"))
			errs = append(errs, err)
		}
	case KIND_DEFINE_TEMPLATE, KIND_BLOCK_TEMPLATE:
		// NOTE: v.parent == TemplateScope, so we need to go deeper to reach the outer scope
		if v.parent.parent != nil && v.parent.parent.isRoot == false {
			err := NewParseError(v.TemplateName, errors.New("template cannot be defined in local scope"))
			errs = append(errs, err)
		}

		templateName := string(v.TemplateName.Value)

		// Make sure that the template haven't already be defined in the local scope (root scope)
		_, found := templateDefinitionsLocal[templateName]
		templateDefinitionsLocal[templateName] = v.parent

		if found {
			err := NewParseError(v.TemplateName, errors.New("template already defined"))
			errs = append(errs, err)
		}

		if v.parent == nil {
			log.Printf("fatal, parent not found on template definition. template = \n %s \n", v)
			panic("'TemplateStatementNode' with unexpected empty parent node. " +
				"the template definition and block template must have a parent which will contain all the statements inside the template")
		}

		if v.parent.Kind != v.Kind {
			panic("value mismatch. 'TemplateStatementNode.Kind' and 'TemplateStatementNode.parent.Kind' must similar")
		}
	default:
		panic("'TemplateStatementNode' do not accept any other type than 'KIND_DEFINE_TEMPLATE, KIND_BLOCK_TEMPLATE, KIND_USE_TEMPLATE'")
	}

	// 2. Expression analysis, if any
	if v.Expression != nil {
		localErr := v.Expression.DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitionsGlobal, templateDefinitionsLocal)
		errs = append(errs, localErr...)
	}

	return errs
}

type GroupStatementNode struct {
	Kind
	Range       lexer.Range
	parent		*GroupStatementNode		// use 'isRoot' to check that the node is the ROOT
	ControlFlow AstNode
	Statements  []AstNode
	isRoot		bool			// only this is consistently enforced to determine whether a node is ROOT or not
}

func (g GroupStatementNode) GetKind() Kind {
	return g.Kind
}

func (g GroupStatementNode) GetRange() lexer.Range {
	return g.Range
}

func (g *GroupStatementNode) SetKind(val Kind) {
	g.Kind = val
}

func (g GroupStatementNode) Parent() *GroupStatementNode {
	return g.parent
}

func (g GroupStatementNode) IsRoot() bool {
	return g.isRoot
}

func (v GroupStatementNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitionsGlobal, templateDefinitionsLocal SymbolDefinition) []lexer.Error {
	panic("not useful anymore")
}

type CommentNode struct {
	Kind
	Range				lexer.Range
	Value				*lexer.Token
	GoCode			[]byte
	// TODO: add those field for 'DefinitionAnalysis()'
}

func (c CommentNode) GetKind() Kind {
	return c.Kind
}

func (c CommentNode) GetRange() lexer.Range {
	return c.Range
}

func (v *CommentNode) SetKind(val Kind) {
	v.Kind = val
}

func (v CommentNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitionsGlobal, templateDefinitionsLocal SymbolDefinition) []lexer.Error {
	panic("not useful anymore")
}

