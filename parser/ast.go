package parser

import (
	"bytes"
	"errors"
	"log"
	"maps"
	"strings"

	"github.com/yayolande/gota/types"
)

//go:generate go run ./generate.go

// type Kind int
type Kind = types.ParserKind

type VariableDeclarationNode struct {
	Kind          Kind
	Range         types.Range
	VariableNames []*types.Token
	Value         *MultiExpressionNode
}

func (v VariableDeclarationNode) GetKind() Kind {
	return v.Kind
}

func (v VariableDeclarationNode) GetRange() types.Range {
	return v.Range
}

func (v *VariableDeclarationNode) SetKind(val Kind) {
	v.Kind = val
}

func (v VariableDeclarationNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitions SymbolDefinition) []types.Error {
	if v.Kind != types.KIND_VARIABLE_DECLARATION {
		panic("found value mismatch for 'VariableDeclarationNode.Kind' during DefinitionAnalysis()")
	}

	if v.Range == getZeroRangeValue() {
		panic("'Range' cannot be empty for VariableAssignationNode.\n" + v.String())
	}

	if localVariables == nil {
		panic("'localVariables' shouldn't be empty for 'VariableDeclarationNode.DefinitionAnalysis()'")
	}

	var errs []types.Error

	// 0. Check that 'expression' is valid
	if v.Value != nil {
		errLocal := v.Value.DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitions)
		errs = append(errs, errLocal...)
	} else {
		errLocal := NewParseError(nil, errors.New("assignment value cannot be empty"))
		errLocal.Range = v.Range
	}

	// 1. Check at least var is declared
	if len(v.VariableNames) == 0 {
		errLocal := ParseError{Err: errors.New("variable name is empty for the declaration"), Range: v.Range}
		errs = append(errs, errLocal)

		return errs
	}

	// 2. Check existance of variable and process without error if 'var' is unique
	for _, variable := range v.VariableNames {
		if bytes.ContainsAny(variable.Value, ".") {
			err := NewParseError(variable,
				errors.New("variable name cannot contains the special character such '.'"))
			err.Range = variable.Range

			errs = append(errs, err)
		}

		// 2. Insert definition into dictionary, since there is no error whether the variable is already declared or not
		key := string(variable.Value)
		localVariables[key] = &v
	}

	return errs
}

type VariableAssignationNode struct {
	Kind         Kind
	Range        types.Range
	VariableName *types.Token
	// Value	AstNode	// of type expression
	Value *MultiExpressionNode // of type expression
}

func (v VariableAssignationNode) GetKind() Kind {
	return v.Kind
}

func (v VariableAssignationNode) GetRange() types.Range {
	return v.Range
}

func (v *VariableAssignationNode) SetKind(val Kind) {
	v.Kind = val
}

func (v VariableAssignationNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitions SymbolDefinition) []types.Error {
	if v.Kind != types.KIND_VARIABLE_ASSIGNMENT {
		panic("found value mismatch for 'VariableAssignationNode.Kind' during DefinitionAnalysis()\n" + v.String())
	}

	if v.Range == getZeroRangeValue() {
		panic("'Range' cannot be empty for VariableAssignationNode.\n" + v.String())
	}

	if globalVariables == nil || localVariables == nil {
		panic("'localVariables' or 'globalVariables' shouldn't be empty for 'VariableAssignationNode.DefinitionAnalysis()'")
	}

	var errs []types.Error

	// 0. Check that 'expression' is valid
	if v.Value != nil {
		errLocal := v.Value.DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitions)
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
	Range       types.Range
	Expressions []*ExpressionNode
}

func (m MultiExpressionNode) GetKind() Kind {
	return m.Kind
}

func (m MultiExpressionNode) GetRange() types.Range {
	return m.Range
}

func (m *MultiExpressionNode) SetKind(val Kind) {
	m.Kind = val
}

func (v *MultiExpressionNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitions SymbolDefinition) []types.Error {
	if v.Kind != types.KIND_MULTI_EXPRESSION {
		panic("found value mismatch for 'MultiExpressionNode.Kind' during DefinitionAnalysis()\n" + v.String())
	}

	if v.Range == getZeroRangeValue() {
		panic("empty 'Range' is unauthorized for MultiExpressionNode.\n" + v.String())
	}

	var errs, localErr []types.Error

	for _, expression := range v.Expressions {
		if expression == nil {
			log.Printf("fatal, nil element within expression list for MultiExpressionNode. \n %s \n", v)
			panic("element within expression list cannot be 'nil' for MultiExpressionNode. instead of inserting the nil value, omit it")
		}

		localErr = expression.DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitions)
		errs = append(errs, localErr...)
	}

	return errs
}

type ExpressionNode struct {
	Kind
	Range   types.Range
	Symbols []*types.Token
	// TODO: Vet them. Not sure of those below
	isError        bool
	isFunctionCall bool
}

func (v ExpressionNode) GetKind() Kind {
	return v.Kind
}

func (v ExpressionNode) GetRange() types.Range {
	return v.Range
}

func (v *ExpressionNode) SetKind(val Kind) {
	v.Kind = val
}

func (v ExpressionNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitions SymbolDefinition) []types.Error {
	if v.Kind != types.KIND_EXPRESSION {
		panic("found value mismatch for 'ExpressionNode.Kind'. expected 'KIND_EXPRESSION' instead. current node \n" + v.String())
	}

	if globalVariables == nil || localVariables == nil || functionDefinitions == nil {
		panic("'globalVariables' or 'localVariables' or 'functionDefinitions' shouldn't be empty for 'ExpressionNode.DefinitionAnalysis()'")
	}

	if v.Range == getZeroRangeValue() {
		panic("empty 'Range' unauthorized for ExpressionNode \n" + v.String())
	}

	// 1. Check FunctionName is legit (var or func)
	var errs []types.Error

	if len(v.Symbols) == 0 {
		err := NewParseError(nil, errors.New("empty expression is not allowed"))
		err.Range = v.Range

		errs = append(errs, err)
		v.isError = true

		return errs
	}

	id := v.Symbols[0].ID

	name := string(v.Symbols[0].Value)

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
		if len(v.Symbols) > 1 {
			err := NewParseError(v.Symbols[0], errors.New("variable cannot have arguments, only function. Remove the extraneous argument, or use a function instead"))
			errs = append(errs, err)
			v.isError = true

			return errs
		}
	} else if isDollarVariable && !isLocalVariable && !isGlobalVariable { // dollar variable not found
		err := NewParseError(v.Symbols[0], errors.New("undefined variable"))
		errs = append(errs, err)

		return errs
	} else if v.Symbols[0].ID == types.STRING || v.Symbols[0].ID == types.NUMBER {
		if len(v.Symbols) > 1 {
			err := NewParseError(v.Symbols[0], errors.New("'"+id.String()+"' cannot have argument(s)"))
			errs = append(errs, err)
			v.isError = true

			return errs
		}
	} else {
		v.isFunctionCall = true

		_, isFunction := functionDefinitions[name]
		if !isFunction {
			err := NewParseError(v.Symbols[0], errors.New("undefined function"))
			errs = append(errs, err)
			v.isError = true
			v.isFunctionCall = false
		}
	}

	// 2. check argument validity
	isGlobalVariable = false
	isLocalVariable = false
	isFunction := false

	for _, arg := range v.Symbols[1:] {
		if arg.ID == types.STRING || arg.ID == types.NUMBER {
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

		err := NewParseError(arg, errors.New(("argument not declared anywhere")))
		errs = append(errs, err)

		v.isError = true
	}

	return errs
}

type TemplateStatementNode struct {
	Kind
	Range        types.Range
	TemplateName *types.Token
	expression   types.AstNode
	parent       *GroupStatementNode
}

func (t TemplateStatementNode) GetKind() Kind {
	return t.Kind
}

func (t *TemplateStatementNode) SetKind(val Kind) {
	t.Kind = val
}

func (t TemplateStatementNode) GetRange() types.Range {
	return t.Range
}

func (v TemplateStatementNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitions SymbolDefinition) []types.Error {
	if templateDefinitions == nil {
		panic("'templateDefinitions' shouldn't be empty for 'TemplateStatementNode.DefinitionAnalysis()'")
	}

	if v.TemplateName == nil {
		panic("the template name should never be empty for a template expression. make sure the template has been parsed correctly.\n" + v.String())
	}

	var errs []types.Error

	// 1. template name analysis
	switch v.Kind {
	case types.KIND_USE_TEMPLATE:
		templateName := string(v.TemplateName.Value)
		_, ok := templateDefinitions[templateName]

		// TODO: remove the 1 lines below
		log.Printf("================ templateName: %s\n", templateName)
		for key := range templateDefinitions {
			log.Println("key (tmpl name def): ", key)
		}
		log.Printf("================\n\ntemplate real def: %#v\n\n", templateDefinitions)

		if !ok {
			// log.Printf("template def: %#v\n", templateDefinitions)
			err := NewParseError(v.TemplateName, errors.New("undefined template"))
			errs = append(errs, err)
		}
	case types.KIND_DEFINE_TEMPLATE, types.KIND_BLOCK_TEMPLATE:
		templateName := string(v.TemplateName.Value)
		templateDefinitions[templateName] = v.parent

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
	if v.expression != nil {
		localErr := v.expression.DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitions)
		errs = append(errs, localErr...)
	}

	return errs
}

type GroupStatementNode struct {
	Kind
	Range       types.Range
	ControlFlow types.AstNode
	Statements  []types.AstNode
}

func (g GroupStatementNode) GetKind() Kind {
	return g.Kind
}

func (g GroupStatementNode) GetRange() types.Range {
	return g.Range
}

func (g *GroupStatementNode) SetKind(val Kind) {
	g.Kind = val
}

func (v GroupStatementNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitions SymbolDefinition) []types.Error {
	if globalVariables == nil || localVariables == nil || functionDefinitions == nil || templateDefinitions == nil {
		panic("arguments global/local/function/template defintion for 'DefinitionAnalysis()' shouldn't be 'nil' for 'GroupStatementNode'")
	}

	// 1. Variables Init
	scopedGlobalVariables := SymbolDefinition{}
	scopedTemplateDefinition := SymbolDefinition{}

	maps.Copy(scopedGlobalVariables, globalVariables)
	maps.Copy(scopedGlobalVariables, localVariables)
	maps.Copy(scopedTemplateDefinition, templateDefinitions)

	localVariables = SymbolDefinition{}

	var errs []types.Error
	var localErr []types.Error

	// 2. ControlFlow analysis
	switch v.Kind {
	case types.KIND_IF, types.KIND_ELSE_IF, types.KIND_RANGE_LOOP, types.KIND_WITH, types.KIND_ELSE_WITH, types.KIND_DEFINE_TEMPLATE, types.KIND_BLOCK_TEMPLATE:
		if v.ControlFlow == nil {
			log.Printf("fatal, 'controlFlow' not found for 'GroupStatementNode'. \n %s \n", v)
			panic("this 'GroupStatementNode' expect a non-nil 'controlFlow' based on its type ('Kind')")
		}

		localErr = v.ControlFlow.DefinitionAnalysis(scopedGlobalVariables, localVariables, functionDefinitions, scopedTemplateDefinition)
		errs = append(errs, localErr...)
	}

	// 3. Variables Scope
	switch v.Kind {
	case types.KIND_IF, types.KIND_ELSE, types.KIND_ELSE_IF, types.KIND_GROUP_STATEMENT, types.KIND_END:

	case types.KIND_RANGE_LOOP, types.KIND_WITH, types.KIND_ELSE_WITH:
		scopedGlobalVariables["."] = v.ControlFlow
	case types.KIND_DEFINE_TEMPLATE, types.KIND_BLOCK_TEMPLATE:
		scopedGlobalVariables["."] = v.ControlFlow
		scopedGlobalVariables["$"] = v.ControlFlow

		control, ok := v.ControlFlow.(*TemplateStatementNode)
		if !ok {
			panic("type mismatch for 'v.ControlFlow'. expected a 'TemplateStatementNode'")
		}

		name := string(control.TemplateName.Value)
		templateDefinitions[name] = &v

		delete(scopedTemplateDefinition, name) // avoid infinite recursive call with 'template' statement
	default:
		panic("found unexpected 'Kind' for 'GroupStatementNode' during 'DefinitionAnalysis()'\n node = " + v.String())
	}

	// 4. Statements analysis
	for _, statement := range v.Statements {
		if statement == nil {
			panic("statement within 'GroupStatementNode' cannot be nil. make to find where this nil value has been introduced and rectify it")
		}

		if v.Kind == types.KIND_GROUP_STATEMENT {
			log.Printf("<--> GroupStatementNode, template definition found: ")
			for key := range templateDefinitions {
				log.Printf("<----> tmpl name key = %s \n", key)
			}
		}

		localErr = statement.DefinitionAnalysis(scopedGlobalVariables, localVariables, functionDefinitions, scopedTemplateDefinition)
		errs = append(errs, localErr...)
	}

	return errs
}

type CommentNode struct {
	Kind
	Range types.Range
	Value *types.Token
}

func (c CommentNode) GetKind() Kind {
	return c.Kind
}

func (c CommentNode) GetRange() types.Range {
	return c.Range
}

func (v *CommentNode) SetKind(val Kind) {
	v.Kind = val
}

func (v CommentNode) DefinitionAnalysis(globalVariables, localVariables, functionDefinitions, templateDefinitions SymbolDefinition) []types.Error {
	if v.Kind != types.KIND_COMMENT {
		panic("found value mismatch for 'CommentNode.Kind' during DefinitionAnalysis().\n " + v.String())
	}

	return nil
}

func getZeroRangeValue() types.Range {
	return types.Range{}
}
