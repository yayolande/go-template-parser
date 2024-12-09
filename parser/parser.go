package parser

import (
	"bytes"
	"errors"
	"log"	// TODO: to remove

	"github.com/yayolande/gota/types"
)

type ParseError struct {
	Err   error
	Range types.Range
	Token *types.Token
}

func (p ParseError) GetError() string {
	return p.Err.Error()
}

func (p ParseError) GetRange() types.Range {
	return p.Range
}

// SymbolDefinition = map[string]types.AstNode
type SymbolDefinition = types.SymbolDefinition

type Parser struct {
	input                 []types.Token
	openedNodeStack       []*GroupStatementNode
	maxRecursionDepth     int
	currentRecursionDepth int
}

func createParser(tokens []types.Token) *Parser {
	if tokens == nil {
		panic("cannot create a parser containing empty tokens")
	}

	input := make([]types.Token, len(tokens))
	copy(input, tokens)

	defaultGroupStatementNode := &GroupStatementNode{
		Kind: types.KIND_GROUP_STATEMENT,
		isRoot: true,
	}

	groupNodeStack := []*GroupStatementNode{}
	groupNodeStack = append(groupNodeStack, defaultGroupStatementNode)

	parser := &Parser{
		input:                 input,
		openedNodeStack:       groupNodeStack,
		maxRecursionDepth:     3,
		currentRecursionDepth: 0,
	}

	return parser
}

func appendStatementToCurrentScope(scopeStack []*GroupStatementNode, statement types.AstNode) {
	if statement == nil {
		panic("cannot add empty statement to 'group'")
	}

	if len(scopeStack) == 0 {
		panic("cannot add statement to empty group. Group must be created before hand")
	}

	currentScope := getLastElement(scopeStack)
	currentScope.Statements = append(currentScope.Statements, statement)
}

func (p *Parser) safeStatementGrouping(node types.AstNode) *ParseError {
	if node == nil {
		return nil
	}

	// change var name to : "openedGroupNodeStack", "activeScopeNodes", "activeScopeStack", "openedScopeStack"
	if len(p.openedNodeStack) == 0 {
		panic("no initial scope available to hold the statements. There must always exist at least one 'scope/group' at any moment")
	}

	if p.openedNodeStack[0].isRoot == false {
		panic("root node has'nt been marked as such. root node, and only the root node, can be marked as 'isRoot'")
	}

	var err *ParseError
	var ROOT_SCOPE *GroupStatementNode = p.openedNodeStack[0]

	newGroup, isScope := node.(*GroupStatementNode)

	if !isScope {
		appendStatementToCurrentScope(p.openedNodeStack, node)
	} else {
		if newGroup.isRoot == true {
			log.Printf("non-root node cannot be flaged as 'root'.\n culprit node = %#v\n", newGroup)
			panic("only the root node, can be marked as 'isRoot', but found it on non-root node")
		}

		stackSize := len(p.openedNodeStack)
		lastInserted := getLastElement(p.openedNodeStack)
		newGroup.parent = lastInserted

		switch newGroup.GetKind() {
		case types.KIND_IF, types.KIND_WITH, types.KIND_RANGE_LOOP, types.KIND_BLOCK_TEMPLATE, types.KIND_DEFINE_TEMPLATE:
			newGroup.parent = getLastElement(p.openedNodeStack)
			appendStatementToCurrentScope(p.openedNodeStack, newGroup)

			p.openedNodeStack = append(p.openedNodeStack, newGroup)

		case types.KIND_ELSE_IF:
			if stackSize > 1 {
				switch lastInserted.GetKind() {
				case types.KIND_IF,types.KIND_ELSE_IF:
					// Remove the last element from the stack and switch it with 'KIND_ELSE_IF' scope
					p.openedNodeStack = p.openedNodeStack[:stackSize-1]

					newGroup.parent = getLastElement(p.openedNodeStack)
					appendStatementToCurrentScope(p.openedNodeStack, newGroup)

					p.openedNodeStack = append(p.openedNodeStack, newGroup)
				default:
					err = &ParseError{Range: lastInserted.GetRange(),
						Err: errors.New("'else if' statement is not compatible with '" + lastInserted.GetKind().String() + "'")}
				}
			} else {
				err = &ParseError{Range: newGroup.GetRange(),
					Err: errors.New("extraneous statement '" + newGroup.GetKind().String() + "'")}
			}
		case types.KIND_ELSE_WITH:
			if stackSize > 1 {
				switch lastInserted.GetKind() {
				case types.KIND_WITH, types.KIND_ELSE_WITH:
					// Remove the last element from the stack and switch it with 'KIND_ELSE_WITH' scope
					p.openedNodeStack = p.openedNodeStack[:stackSize-1]

					newGroup.parent = getLastElement(p.openedNodeStack)
					appendStatementToCurrentScope(p.openedNodeStack, newGroup)

					p.openedNodeStack = append(p.openedNodeStack, newGroup)
				default:
					err = &ParseError{Range: lastInserted.GetRange(),
						Err: errors.New("'else with' statement is not compatible with '" + lastInserted.GetKind().String() + "'")}
				}
			} else {
				err = &ParseError{Range: newGroup.GetRange(),
					Err: errors.New("extraneous statement '" + newGroup.GetKind().String() + "'")}
			}
		case types.KIND_ELSE:
			if stackSize > 1 {
				switch lastInserted.GetKind() {
				case types.KIND_IF, types.KIND_ELSE_IF, types.KIND_WITH, types.KIND_ELSE_WITH, types.KIND_RANGE_LOOP:
					// Remove the last element from the stack and switch it with 'KIND_ELSE' scope
					p.openedNodeStack = p.openedNodeStack[:stackSize-1]

					newGroup.parent = getLastElement(p.openedNodeStack)
					appendStatementToCurrentScope(p.openedNodeStack, newGroup)

					p.openedNodeStack = append(p.openedNodeStack, newGroup)
				default:
					err = &ParseError{Range: newGroup.GetRange(),
						Err: errors.New("'else' statement is not compatible with '" + newGroup.GetKind().String() + "'")}
				}
			} else {
				err = &ParseError{Range: newGroup.GetRange(),
					Err: errors.New("extraneous statement '" + newGroup.GetKind().String() + "'")}
			}
		case types.KIND_END:
			if stackSize > 1 {
				p.openedNodeStack = p.openedNodeStack[:stackSize-1]

				newGroup.parent = getLastElement(p.openedNodeStack)
				appendStatementToCurrentScope(p.openedNodeStack, newGroup)
			} else {
				err = &ParseError{Range: newGroup.GetRange(), Err: errors.New("extraneous 'end' statement detected")}
			}
		default:
			log.Printf("unhandled scope type error\n scope = %#v\n", newGroup)
			panic("scope type '" + newGroup.GetKind().String() + "' is not yet handled for statement grouping\n" + newGroup.String())
		}
	}

	if len(p.openedNodeStack) == 0 {
		panic("'openedNodeStack' cannot be empty ! you have inadvertly close the 'root scope'. You should not interact with it")
	}

	if ROOT_SCOPE != p.openedNodeStack[0] {
		log.Println("root scope change error. new Root = %#v\n", p.openedNodeStack[0])
		panic("error, the root scope have been modified. The root scope should never change under any circumstance")
	}

	return err
}

// Parse tokens into AST and return syntax errors found during the process
func Parse(tokens []types.Token) (*GroupStatementNode, []types.Error) {
	if tokens == nil {
		return nil, nil
	}

	parser := createParser(tokens)

	var errs []types.Error
	var err *ParseError
	var node types.AstNode

	for !parser.isEOF() {
		node, err = parser.StatementParser()

		if err != nil {
			errs = append(errs, err)
			parser.flushInputUntilNextStatement()
		} else {
			err = parser.safeStatementGrouping(node)

			if err != nil {
				errs = append(errs, err)
			}
		}
	}

	if len(parser.openedNodeStack) == 0 {
		panic("fatal error while building the parse tree. Expected at least one scope/group but found nothing")
	}

	defaultGroupStatementNode := parser.openedNodeStack[0]

	if len(parser.openedNodeStack) > 1 {
		lastInserted := getLastElement(parser.openedNodeStack)
		err := ParseError{Range: lastInserted.GetRange(),
			Err: errors.New("not all group statements ('if/else/define/block/with') have been properly claused")}

		errs = append(errs, err)
	}

	return defaultGroupStatementNode, errs
}

// Parse statement form tokens. It is the function that does the real parsing work.
// Whenever parsing fail, you must call 'flushInputUntilNextStatement()' method
// to parse the next statement accurately or else you might be in for a nasty surprise.
// NB: when an error occur, the returned 'ast' can be nil or not. Most of the time,
// the ast will be partially constructed instead of being nil.
// Thus always use the returned error value to deternime whether parsing completed succesfully.
// NB: you should not call this function directly, unless you want to build a costum parsing procedure
// instead it is recommened to use 'Parse()' for regular use cases
func (p *Parser) StatementParser() (ast types.AstNode, er *ParseError) {
	if len(p.input) == 0 {
		return nil, nil
	}

	// 1. Escape infinite recursion
	if p.isRecursionMaxDepth() {
		err := NewParseError(p.peek(), errors.New("parser error, reached the max depth authorized"))
		return nil, err
	}

	p.incRecursionDepth()
	defer p.decRecursionDepth()

	// 2. Helper variable mainly used to easily get end location of the instruction (token.Range.End)
	lastTokenInInstruction := p.peekAtEndCurrentInstruction() // token before 'EOL'
	if lastTokenInInstruction == nil {
		panic("unexpected empty token found at end of the current instruction")
	}

	// 3. Syntax Parser for the Go Template language
	if p.acceptAt(1, types.DECLARATION_ASSIGNEMENT) {
		varDeclarationNode, err := p.declarationAssignmentParser()

		if err != nil {
			return varDeclarationNode, err // partial AST are meant for debugging
		}

		if !p.expect(types.EOL) {
			err = NewParseError(p.peek(), errors.New("syntax for assignment delcaration didn't end properly. extraneous expression"))
			err.Range.End = lastTokenInInstruction.Range.End
			return varDeclarationNode, err
		}

		return varDeclarationNode, nil

	} else if p.acceptAt(1, types.ASSIGNEMENT) {
		varInitialization, err := p.initializationAssignmentParser()

		if err != nil {
			return varInitialization, err // partial AST are meant for debugging
		}

		if !p.expect(types.EOL) {
			err = NewParseError(p.peek(), errors.New("syntax for assignment didn't end properly. extraneous expression"))
			err.Range.End = lastTokenInInstruction.Range.End
			return varInitialization, err
		}

		return varInitialization, nil

	} else if p.accept(types.COMMENT) {
		commentExpression := &CommentNode{Kind: types.KIND_COMMENT, Value: p.peek(), Range: p.peek().Range}

		p.nextToken()

		if !p.expect(types.EOL) {
			err := NewParseError(p.peek(), errors.New("syntax for comment didn't end properly. extraneous expression"))
			err.Range.End = lastTokenInInstruction.Range.End
			return commentExpression, err
		}

		// Check that this comment contains go code to semantically analize
		lookForAndSetGoCodeInComment(commentExpression)

		return commentExpression, nil

	} else if p.accept(types.KEYWORD) {
		keywordToken := p.peek()

		// INFO: most composite statements (else if xxx, etc) do not check for 'EOL' on purpose

		if bytes.Compare(keywordToken.Value, []byte("if")) == 0 {
			ifExpression := &GroupStatementNode{}
			ifExpression.Kind = types.KIND_IF
			ifExpression.Range = keywordToken.Range
			ifExpression.Range.End = lastTokenInInstruction.Range.End

			p.nextToken() // skip keyword "if"

			expression, err := p.StatementParser()
			ifExpression.ControlFlow = expression

			if err != nil {
				return ifExpression, err // partial AST are meant for debugging
			}

			if expression == nil { // because if err == nil, then expression != nil
				panic("returned AST was nil although parsing completed succesfully. can't be added to ControlFlow\n" + ifExpression.String())
			}

			if ifExpression.Range.End != expression.GetRange().End {
				panic("ending location mismatch between 'if' statement and its expression\n" + ifExpression.String())
			}

			switch expression.GetKind() {
			case types.KIND_VARIABLE_ASSIGNMENT, types.KIND_VARIABLE_DECLARATION, types.KIND_MULTI_EXPRESSION, types.KIND_EXPRESSION:

			default:
				err = NewParseError(&types.Token{}, errors.New("'if' do not accept this kind of statement"))
				err.Range = expression.GetRange()
				return ifExpression, err
			}

			return ifExpression, nil

		} else if bytes.Compare(keywordToken.Value, []byte("else")) == 0 {
			elseExpression := &GroupStatementNode{}
			elseExpression.Kind = types.KIND_ELSE
			elseExpression.Range = keywordToken.Range
			elseExpression.Range.End = lastTokenInInstruction.Range.End

			p.nextToken() // skip 'else' token

			if p.expect(types.EOL) {
				return elseExpression, nil
			}

			// past this, 'elseExpression' is not useful anymore, and will be replaced by 'elseCompositeExpression'
			elseExpression = nil
			elseCompositeExpression, err := p.StatementParser()

			if err != nil {
				return elseCompositeExpression, err // partial AST are meant for debugging
			}

			if elseCompositeExpression == nil { // because if err == nil, then expression != nil
				panic("returned AST was nil although parsing completed succesfully. 'else ...' parse error")
			}

			if elseCompositeExpression.GetRange().End != lastTokenInInstruction.Range.End {
				panic("ending location mismatch between 'else if/with/...' statement and its expression\n" + elseCompositeExpression.String())
			}

			switch elseCompositeExpression.GetKind() {
			case types.KIND_IF:
				elseCompositeExpression.SetKind(types.KIND_ELSE_IF)
			case types.KIND_WITH:
				elseCompositeExpression.SetKind(types.KIND_ELSE_WITH)
			default:
				err = NewParseError(keywordToken, errors.New("bad syntax for else statement"))
				err.Range = elseCompositeExpression.GetRange()
				return elseCompositeExpression, err
			}

			return elseCompositeExpression, nil

		} else if bytes.Compare(keywordToken.Value, []byte("end")) == 0 {
			endExpression := &GroupStatementNode{}
			endExpression.Kind = types.KIND_END
			endExpression.Range = keywordToken.Range
			endExpression.Range.End = lastTokenInInstruction.Range.End

			p.nextToken() // skip 'end' token

			if !p.expect(types.EOL) {
				err := NewParseError(keywordToken, errors.New("expression next to 'end' is diasallowed"))
				err.Range.End = lastTokenInInstruction.Range.End
				return endExpression, err
			}

			return endExpression, nil

		} else if bytes.Compare(keywordToken.Value, []byte("range")) == 0 {
			rangeExpression := &GroupStatementNode{}
			rangeExpression.Kind = types.KIND_RANGE_LOOP
			rangeExpression.Range = keywordToken.Range
			rangeExpression.Range.End = lastTokenInInstruction.Range.End

			p.nextToken()

			var expression types.AstNode
			var err *ParseError

			if p.acceptAt(1, types.COMMA) {
				expression, err = p.doubleDeclarationAssignmentParser()

				if !p.expect(types.EOL) {
					err = NewParseError(keywordToken, errors.New("'range' statement has extraneous expression"))
					err.Range.End = lastTokenInInstruction.Range.End
					return rangeExpression, err
				}
			} else {
				expression, err = p.StatementParser()
			}

			rangeExpression.ControlFlow = expression

			if err != nil {
				return rangeExpression, err // partial AST are meant for debugging
			}

			if expression == nil { // because if err == nil, then expression != nil
				panic("returned AST was nil although parsing completed succesfully. can't be added to ControlFlow\n" + rangeExpression.String())
			}

			if rangeExpression.Range.End != expression.GetRange().End {
				panic("ending location mismatch between 'range' statement and its expression\n" + rangeExpression.String())
			}

			switch expression.GetKind() {
			case types.KIND_VARIABLE_ASSIGNMENT, types.KIND_VARIABLE_DECLARATION, types.KIND_MULTI_EXPRESSION, types.KIND_EXPRESSION:

			default:
				err = NewParseError(lastTokenInInstruction, errors.New("'range' do not accept those type of expression"))
				err.Range = expression.GetRange()
				return rangeExpression, err
			}

			return rangeExpression, nil

		} else if bytes.Compare(keywordToken.Value, []byte("with")) == 0 {
			withExpression := &GroupStatementNode{}
			withExpression.Kind = types.KIND_WITH
			withExpression.Range = keywordToken.Range
			withExpression.Range.End = lastTokenInInstruction.Range.End

			p.nextToken() // skip 'with' token

			expression, err := p.StatementParser()
			withExpression.ControlFlow = expression

			if err != nil {
				return withExpression, err // partial AST are meant for debugging
			}

			if expression == nil { // because if err == nil, then expression != nil
				panic("returned AST was nil although parsing completed succesfully. 'with ...' parse error")
			}

			if withExpression.Range.End != expression.GetRange().End {
				panic("ending location mismatch between 'range' statement and its expression\n" + withExpression.String())
			}

			switch expression.GetKind() {
			case types.KIND_VARIABLE_ASSIGNMENT, types.KIND_VARIABLE_DECLARATION, types.KIND_MULTI_EXPRESSION, types.KIND_EXPRESSION:

			default:
				err = NewParseError(lastTokenInInstruction, errors.New("'with' do not accept those type of expression"))
				err.Range = expression.GetRange()
				return withExpression, err
			}

			return withExpression, nil

		} else if bytes.Compare(keywordToken.Value, []byte("block")) == 0 {
			blockExpression := &GroupStatementNode{}
			blockExpression.Kind = types.KIND_BLOCK_TEMPLATE
			blockExpression.Range = keywordToken.Range
			blockExpression.Range.End = lastTokenInInstruction.Range.End

			p.nextToken() // skip 'block' token

			if !p.accept(types.STRING) {
				err := NewParseError(p.peek(), errors.New("'block' expect a string next to it"))
				return blockExpression, err
			}

			templateExpression := &TemplateStatementNode{Kind: types.KIND_BLOCK_TEMPLATE, TemplateName: p.peek(), Range: p.peek().Range}
			templateExpression.parent = blockExpression
			blockExpression.ControlFlow = templateExpression

			p.nextToken()

			expression, err := p.StatementParser()
			templateExpression.expression = expression

			if err != nil {
				return blockExpression, err // partial AST are meant for debugging
			}

			if expression == nil { // because if err == nil, then expression != nil
				panic("returned AST was nil although parsing completed succesfully. can't add expression to ControlFlow\n" + blockExpression.String())
			}

			if blockExpression.Range.End != expression.GetRange().End {
				panic("ending location mismatch between 'range' statement and its expression\n" + blockExpression.String())
			}

			switch expression.GetKind() {
			case types.KIND_VARIABLE_ASSIGNMENT, types.KIND_VARIABLE_DECLARATION, types.KIND_MULTI_EXPRESSION, types.KIND_EXPRESSION:

			default:
				err = NewParseError(templateExpression.TemplateName, errors.New("'block' do not accept those type of expression"))
				err.Range = expression.GetRange()
				return blockExpression, err
			}

			return blockExpression, nil

		} else if bytes.Compare(keywordToken.Value, []byte("define")) == 0 {
			defineExpression := &GroupStatementNode{}
			defineExpression.Kind = types.KIND_DEFINE_TEMPLATE
			defineExpression.Range = keywordToken.Range
			defineExpression.Range.End = lastTokenInInstruction.Range.End

			p.nextToken() // skip 'define' token

			if !p.accept(types.STRING) {
				err := NewParseError(p.peek(), errors.New("'define' expect a string next to it"))
				return defineExpression, err
			}

			templateExpression := &TemplateStatementNode{Kind: types.KIND_DEFINE_TEMPLATE, TemplateName: p.peek(), Range: p.peek().Range}
			templateExpression.parent = defineExpression
			defineExpression.ControlFlow = templateExpression

			p.nextToken()

			if !p.expect(types.EOL) {
				err := NewParseError(p.peek(), errors.New("'define' do not accept any expression after its name"))
				err.Range.End = lastTokenInInstruction.Range.End
				return defineExpression, err
			}

			return defineExpression, nil

		} else if bytes.Compare(keywordToken.Value, []byte("template")) == 0 {
			templateExpression := &TemplateStatementNode{}
			templateExpression.Kind = types.KIND_USE_TEMPLATE
			templateExpression.Range = keywordToken.Range
			templateExpression.Range.End = lastTokenInInstruction.Range.End
			templateExpression.parent = nil

			p.nextToken() // skip 'template' tokens

			if !p.accept(types.STRING) {
				err := NewParseError(p.peek(), errors.New("'template' expect a string next to it"))
				return templateExpression, err
			}

			templateExpression.TemplateName = p.peek()
			p.nextToken()

			expression, err := p.StatementParser()
			templateExpression.expression = expression

			if err != nil {
				return templateExpression, err // partial AST are meant for debugging
			}

			if expression == nil { // because if err == nil, then expression != nil
				panic("returned AST was nil although parsing completed succesfully. can't add expression to ControlFlow\n" + templateExpression.String())
			}

			if templateExpression.Range.End != expression.GetRange().End {
				panic("ending location mismatch between 'range' statement and its expression\n" + templateExpression.String())
			}

			switch expression.GetKind() {
			case types.KIND_VARIABLE_ASSIGNMENT, types.KIND_VARIABLE_DECLARATION, types.KIND_MULTI_EXPRESSION, types.KIND_EXPRESSION:

			default:
				err = NewParseError(templateExpression.TemplateName, errors.New("'template' do not accept those type of expression"))
				err.Range = expression.GetRange()
				return templateExpression, err
			}

			return templateExpression, nil
		}
	}

	// 4. Default parser whenever no other parser have been enabled

	multiExpression, err := p.multiExpressionParser()
	if err != nil {
		return multiExpression, err // partial AST are meant for debugging
	}

	if !p.expect(types.EOL) {
		err = NewParseError(p.peek(), errors.New("expected end of expression, but got extraneous expression"))
		err.Range.End = lastTokenInInstruction.Range.End
		return multiExpression, err
	}

	return multiExpression, nil
}

func (p *Parser) declarationAssignmentParser() (*VariableDeclarationNode, *ParseError) {
	if !p.accept(types.DOLLAR_VARIABLE) {
		err := NewParseError(p.peek(), errors.New("variable name must start by '$' and contains alphanumerical char"))
		return nil, err
	}

	variable := p.peek()
	p.nextToken()

	if !p.expect(types.DECLARATION_ASSIGNEMENT) {
		err := NewParseError(p.peek(), errors.New("expected assignmement '=', but found something else"))
		return nil, err
	}

	lastTokenInInstruction := p.peekAtEndCurrentInstruction() // token before 'EOL'
	if lastTokenInInstruction == nil {
		panic("unexpected empty token found at end of the current instruction")
	}

	varDeclarationNode := &VariableDeclarationNode{}
	varDeclarationNode.Kind = types.KIND_VARIABLE_DECLARATION
	varDeclarationNode.Range = variable.Range
	varDeclarationNode.Range.End = lastTokenInInstruction.Range.End
	varDeclarationNode.VariableNames = append(varDeclarationNode.VariableNames, variable)

	expression, err := p.multiExpressionParser()
	varDeclarationNode.Value = expression

	if err != nil {
		return varDeclarationNode, err // partial AST are meant for debugging
	}

	if expression == nil { // because if err == nil, then expression != nil
		panic("returned AST was nil although parsing completed succesfully. can't be added to ControlFlow\n" + varDeclarationNode.String())
	}

	if varDeclarationNode.Range.End != expression.GetRange().End {
		panic("ending location mismatch between 'if' statement and its expression\n" + varDeclarationNode.String())
	}

	return varDeclarationNode, nil
}

func (p *Parser) doubleDeclarationAssignmentParser() (*VariableDeclarationNode, *ParseError) {
	if !p.accept(types.DOLLAR_VARIABLE) {
		err := NewParseError(p.peek(), errors.New("expected variable begining with '$' but got something else"))
		return nil, err
	}

	firstVariable := p.peek()
	p.nextToken()

	if !p.expect(types.COMMA) {
		err := NewParseError(p.peek(), errors.New("expected ',' to separate variable while declaring them"))
		return nil, err
	}

	varDeclarationNode, err := p.declarationAssignmentParser()
	if err != nil {
		return varDeclarationNode, err // partial AST are meant for debugging
	}

	if varDeclarationNode == nil { // because if err == nil, then expression != nil
		panic("returned AST was nil although parsing completed succesfully. can't be added to ControlFlow\n" + varDeclarationNode.String())
	}

	if len(varDeclarationNode.VariableNames) != 1 {
		err = NewParseError(firstVariable, errors.New("expected one variable after ',' but got something else"))
		err.Range = varDeclarationNode.Range
		return varDeclarationNode, err
	}

	secondVariable := varDeclarationNode.VariableNames[0]
	varDeclarationNode.VariableNames = nil
	varDeclarationNode.VariableNames = append(varDeclarationNode.VariableNames, firstVariable, secondVariable)
	varDeclarationNode.Range.Start = firstVariable.Range.Start

	return varDeclarationNode, nil
}

func (p *Parser) initializationAssignmentParser() (*VariableAssignationNode, *ParseError) {
	if !p.accept(types.DOLLAR_VARIABLE) {
		err := NewParseError(p.peek(), errors.New("the variable name must start by '$' or '.', and only contains alphanumerical char"))
		return nil, err
	}

	variable := p.peek()
	p.nextToken()

	if !p.expect(types.ASSIGNEMENT) {
		err := NewParseError(p.peek(), errors.New("expected assignmement '=', but found something else"))
		return nil, err
	}

	lastTokenInInstruction := p.peekAtEndCurrentInstruction() // token before 'EOL'
	if lastTokenInInstruction == nil {
		panic("unexpected empty token found at end of the current instruction")
	}

	varAssignation := &VariableAssignationNode{}
	varAssignation.Kind = types.KIND_VARIABLE_ASSIGNMENT
	varAssignation.VariableName = variable
	varAssignation.Range = variable.Range
	varAssignation.Range.End = lastTokenInInstruction.Range.End

	expression, err := p.multiExpressionParser()
	varAssignation.Value = expression
	if err != nil {
		return varAssignation, err // partial AST are meant for debugging
	}

	if varAssignation == nil { // because if err == nil, then expression != nil
		panic("returned AST was nil although parsing completed succesfully. can't be added to ControlFlow\n" + varAssignation.String())
	}

	varAssignation.Range.End = expression.Range.End

	return varAssignation, nil
}

func (p *Parser) multiExpressionParser() (*MultiExpressionNode, *ParseError) {
	lastTokenInInstruction := p.peekAtEndCurrentInstruction() // token before 'EOL'
	if lastTokenInInstruction == nil {
		panic("unexpected empty token found at end of the current instruction")
	}

	multiExpression := &MultiExpressionNode{}
	multiExpression.Kind = types.KIND_MULTI_EXPRESSION
	multiExpression.Range.Start = p.peek().Range.Start
	multiExpression.Range.End = lastTokenInInstruction.Range.End

	expression, err := p.expressionParser()
	multiExpression.Expressions = append(multiExpression.Expressions, expression)

	if err != nil {
		err.Range = lastTokenInInstruction.Range
		err.Range.Start = lastTokenInInstruction.Range.End
		err.Range.Start.Character--
		return multiExpression, err
	}

	if expression == nil { // because if err == nil, then expression != nil
		panic("returned AST was nil although parsing completed succesfully. can't be added to ControlFlow\n" + multiExpression.String())
	}

	for p.expect(types.PIPE) {
		expression, err = p.expressionParser()
		multiExpression.Expressions = append(multiExpression.Expressions, expression)

		if err != nil {
			err.Range = lastTokenInInstruction.Range
			err.Range.Start = lastTokenInInstruction.Range.End
			err.Range.Start.Character--
			return multiExpression, err
		}

		if expression == nil { // because if err == nil, then expression != nil
			panic("returned AST was nil although parsing completed succesfully. can't be added to ControlFlow\n" + multiExpression.String())
		}
	}

	multiExpression.Range.End = expression.Range.End

	return multiExpression, nil
}

func (p *Parser) expressionParser() (*ExpressionNode, *ParseError) {
	lastTokenInInstruction := p.peekAtEndCurrentInstruction() // token before 'EOL'
	if lastTokenInInstruction == nil {
		panic("unexpected empty token found at end of the current instruction")
	}

	expression := &ExpressionNode{}
	expression.Kind = types.KIND_EXPRESSION
	expression.Range.Start = p.peek().Range.Start
	expression.Range.Start = lastTokenInInstruction.Range.End

	var currentSymbol *types.Token
	for p.accept(types.FUNCTION) || p.accept(types.DOT_VARIABLE) || p.accept(types.DOLLAR_VARIABLE) || p.accept(types.STRING) || p.accept(types.NUMBER) {
		currentSymbol = p.peek()
		expression.Symbols = append(expression.Symbols, currentSymbol)

		p.nextToken()
	}

	if p.isEOF() {
		panic("'expression' parser reached the end of instructions unexpectedly\n" + expression.String())
	}

	if currentSymbol == nil {
		err := NewParseError(p.peek(), errors.New("expected an expression but got nothing"))
		return expression, err
	}

	expression.Range.End = currentSymbol.Range.End

	return expression, nil
}
func lookForAndSetGoCodeInComment(commentExpression *CommentNode) {
	comment := commentExpression.Value.Value
	comment = bytes.TrimSpace(comment)

	const SEP_COMMENT_GOCODE = "go:code"
	if ! bytes.HasPrefix(comment, []byte(SEP_COMMENT_GOCODE)) {
		log.Printf("SEP not found :: sep = %s :: comment = %q\n", SEP_COMMENT_GOCODE, comment)
		return
	}

	start := len(SEP_COMMENT_GOCODE)
	comment = comment[start:]

	if len(comment) == 0 {
		log.Printf("after SEP, comment too short comment = %q\n", comment)
		return
	}

	first := comment[0]
	if ! (first == ' ' || first == '\n' || first == '\t' || first == '\r') {
		log.Printf("after SEP, no separation 'space' = %q\n", comment)
		return
	}

	comment = bytes.TrimLeft(comment, SEP_COMMENT_GOCODE)
	commentExpression.GoCode = comment
	log.Printf("\nHourray, go:code found : %q\n\n", comment)
}

func (p Parser) peek() *types.Token {
	if len(p.input) == 0 {
		return nil
	}

	return &p.input[0]
}

func (p Parser) peekAt(pos int) *types.Token {
	if pos < len(p.input) {
		return &p.input[pos]
	}

	return nil
}

// Return the last token before 'EOL' from the current instruction
func (p Parser) peekAtEndCurrentInstruction() *types.Token {
	if len(p.input) == 0 {
		return nil
	}

	last := &p.input[0]

	for _, tok := range p.input {
		if tok.ID == types.EOL {
			break
		}
		last = &tok
	}

	return last
}

func (p *Parser) nextToken() {
	if len(p.input) == 0 {
		return
	}

	p.input = p.input[1:]
}

// Whenever a parse error happen for a ligne (series of tokens ending with token 'EOL'),
// the tokens of the statements (line) are not fully read.
// Thus to accurate parse the next statement/line, you must flush tokens of the erroneous statement
func (p *Parser) flushInputUntilNextStatement() {
	if len(p.input) == 0 {
		return
	}

	for index, el := range p.input {
		if el.ID == types.EOL {
			index++
			p.input = p.input[index:]

			return
		}
	}

	panic("tokens malformated used during parsing. missing 'EOL' at the end of instruction tokens")
}

func (p Parser) accept(kind types.LexerKind) bool {
	if len(p.input) == 0 {
		return false
	}
	return p.input[0].ID == kind
}

func (p Parser) acceptAt(pos int, kind types.LexerKind) bool {
	if len(p.input) == 0 {
		return false
	}

	if pos >= len(p.input) {
		return false
	}

	return p.input[pos].ID == kind
}

func (p *Parser) expect(kind types.LexerKind) bool {
	if p.accept(kind) {
		p.nextToken()

		return true
	}

	return false
}

func (p Parser) isEOF() bool {
	return len(p.input) == 0
}

func (p Parser) isEOL() bool {
	if len(p.input) == 0 {
		return false
	}

	return p.input[0].ID == types.EOL
}

func (p *Parser) incRecursionDepth() {
	p.currentRecursionDepth++
}

func (p *Parser) decRecursionDepth() {
	p.currentRecursionDepth--
}

func (p Parser) isRecursionMaxDepth() bool {
	return p.currentRecursionDepth >= p.maxRecursionDepth
}

func NewParseError(token *types.Token, err error) *ParseError {
	e := &ParseError{
		Err:   err,
		Range: token.Range,
		Token: token,
	}

	return e
}

func getLastElement[E any](arr []E) E {
	var last E

	size := len(arr)
	if size <= 0 {
		return last
	}

	last = arr[size-1]

	return last
}
