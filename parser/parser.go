package parser

import (
	"bytes"
	"errors"
	"github.com/yayolande/gota/types"
)

// TODO: change 'Range' to 'interval' or 'reach' ?
// TODO: change this to a linked list ?
type ParseError struct {
	Err	error
	Range	types.Range
	Token	*types.Token
}

func (p ParseError) GetError() string {
	// return "I am da best"
	return p.Err.Error()
}

func (p ParseError) GetRange() *types.Range {
	return &p.Range
}

// SymbolDefinition = map[string]types.AstNode
type SymbolDefinition = types.SymbolDefinition

type Parser struct {
	input	[]types.Token
	openedNodeStack []*GroupStatementNode
	maxRecursionDepth	int
	currentRecursionDepth	int
}

func createParser(tokens []types.Token) *Parser {
	if tokens == nil {
		panic("cannot create a parser containing empty tokens")
	}

	input := make([]types.Token, len(tokens))
	copy(input, tokens)

	defaultGroupStatementNode := &GroupStatementNode{Kind: types.KIND_GROUP_STATEMENT}

	groupNodeStack := []*GroupStatementNode{}
	groupNodeStack = append(groupNodeStack, defaultGroupStatementNode)

	parser := &Parser{
		input: input,
		openedNodeStack: groupNodeStack,
		maxRecursionDepth: 3,
		currentRecursionDepth: 0,
	}

	return parser
}

func addStatementToCurrentScope(statement types.AstNode, scopeStack []*GroupStatementNode) {
	if statement == nil {
		panic("cannot add empty statement to 'group'")
	}

	if scopeStack == nil {
		panic("cannot add statement to empty group. Group must be created before hand")
	}

	currentScope := getLastElement(scopeStack)
	// currentScope, ok := lastElement.(*GroupStatementNode)

	/*
	if !ok {
		panic("only element of type 'GroupStatementNode' are accepted within 'groupNodeStack' slice")
	}
	*/

	currentScope.Statements = append(currentScope.Statements, statement)
}

func (p *Parser) safeStatementGrouping(node types.AstNode) *ParseError {
	// 1. Check for fatal error, where there is no default group assigned
	if node == nil {
		panic("statement to add in the scope cannot be nil")
	}

	// change var name to : "openedGroupNodeStack", "activeScopeNodes", "activeScopeStack"
	if len(p.openedNodeStack) == 0 {
		panic("no initial scope available to hold the statements. There must always exist at least one 'scope/group' at any moment")
	}

	// TODO: remove 'types.AstNode' in favor of '*GroupStatementNode'
	var ROOT_SCOPE types.AstNode = p.openedNodeStack[0]

	var err *ParseError

	// 2. Decide what to do
	newGroup, isScope := node.(*GroupStatementNode)

	if !isScope {
		addStatementToCurrentScope(node, p.openedNodeStack)
	} else {
		switch newGroup.GetKind() {
		case types.KIND_IF, types.KIND_WITH, types.KIND_RANGE_LOOP, types.KIND_BLOCK_TEMPLATE, types.KIND_DEFINE_TEMPLATE:
			addStatementToCurrentScope(newGroup, p.openedNodeStack)
			p.openedNodeStack = append(p.openedNodeStack, newGroup)

		case types.KIND_ELSE_IF:
			size := len(p.openedNodeStack)

			if size > 1 {
				lastInserted := p.openedNodeStack[size - 1]
				lastInsertedKind := p.openedNodeStack[size - 1].GetKind()

				if lastInsertedKind == types.KIND_IF || lastInsertedKind == types.KIND_ELSE_IF {
					p.openedNodeStack = p.openedNodeStack[:size - 1]
					addStatementToCurrentScope(newGroup, p.openedNodeStack)
					p.openedNodeStack = append(p.openedNodeStack, newGroup)
				} else {
					err = &ParseError{Range: *lastInserted.GetRange(), 
						Err: errors.New("'else if' statement is not compatible with '" + lastInsertedKind.String() + "'"), }
				}
			} else {
				// TODO: missing 'token' field to include
				err = &ParseError{Range: *newGroup.GetRange(), 
					Err: errors.New("extraneous statement '" + newGroup.GetKind().String() + "'")}
			}
		case types.KIND_ELSE_WITH:
			size := len(p.openedNodeStack)
			if size > 1 {
				lastInserted := p.openedNodeStack[size - 1]
				lastInsertedKind := p.openedNodeStack[size - 1].GetKind()

				if lastInsertedKind == types.KIND_WITH || lastInsertedKind == types.KIND_ELSE_WITH {
					p.openedNodeStack = p.openedNodeStack[:size - 1]
					addStatementToCurrentScope(newGroup, p.openedNodeStack)
					p.openedNodeStack = append(p.openedNodeStack, newGroup)
				} else {
					err = &ParseError{Range: *lastInserted.GetRange(), 
						Err: errors.New("'else with' statement is not compatible with '" + lastInsertedKind.String() + "'"), }
				}
			} else {
				err = &ParseError{Range: *newGroup.GetRange(), 
					Err: errors.New("extraneous statement '" + newGroup.GetKind().String() + "'")}
			}
		case types.KIND_ELSE:
			size := len(p.openedNodeStack)
			if size > 1 {
				lastInserted := p.openedNodeStack[size - 1]
				switch lastInserted.GetKind() {
				case types.KIND_IF, types.KIND_ELSE_IF, types.KIND_WITH, types.KIND_ELSE_WITH, types.KIND_RANGE_LOOP:
					p.openedNodeStack = p.openedNodeStack[:size - 1]
					addStatementToCurrentScope(newGroup, p.openedNodeStack)
					p.openedNodeStack = append(p.openedNodeStack, newGroup)
				default:
					err = &ParseError{Range: *newGroup.GetRange(), 
						Err: errors.New("'else' statement is not compatible with '" + newGroup.GetKind().String() + "'")}
				}
			} else {
				err = &ParseError{Range: *newGroup.GetRange(), 
					Err: errors.New("extraneous statement '" + newGroup.GetKind().String() + "'")}
			}
		case types.KIND_END:
			size := len(p.openedNodeStack)
			if size > 1 {
				p.openedNodeStack = p.openedNodeStack[:size - 1]
				addStatementToCurrentScope(newGroup, p.openedNodeStack)
			} else {
				err = &ParseError{Range: *newGroup.GetRange(), Err: errors.New("extraneous 'end' statement detected")}
			}
		default:
			panic("'scope' type (" + newGroup.String() + ") is not yet handled for statement grouping")
		}
	}


	if len(p.openedNodeStack) == 0 {
		panic("'openedNodeStack' cannot be empty ! you have inadvertly close the 'root scope'. You should not interact with it")
	}

	if ROOT_SCOPE != p.openedNodeStack[0] {
		panic("error, the root scope have been modified. The root scope should never change under any circumstance")
	}

	return err
}

func Parse(tokens []types.Token) (*GroupStatementNode, []types.Error) {
	if tokens == nil {
		return nil, nil
	}

	var errs []types.Error

	parser := createParser(tokens)
	nodes := []types.AstNode{}

	for ! parser.isEOF() {
		node, err := parser.StatementParser()

		if err != nil {
			errs = append(errs, *err)
			parser.flushInputUntilNextStatement()
		} else {
			err = parser.safeStatementGrouping(node)

			if err != nil {
				errs = append(errs, *err)
			} else {
				nodes = append(nodes, node)
			}
		}
	}
	
	if len(parser.openedNodeStack) == 0 {
		panic("fatal error while building the parse tree. Expected at least one scope/group but found nothing")
	}

	defaultGroupStatementNode := parser.openedNodeStack[0]
	if len(parser.openedNodeStack) > 1 {
		lastInserted := getLastElement(parser.openedNodeStack)
		err := ParseError{Range: *lastInserted.GetRange(), 
			Err: errors.New("not all group statements ('if/else/define/block/with') have been properly claused")}

		errs = append(errs, err)
	}

	return defaultGroupStatementNode, errs
}

func (p *Parser) StatementParser() (types.AstNode, *ParseError) {
	if (p.isRecursionMaxDepth()) {
		err := NewParseError(p.peek(), errors.New("parser error, reached the max depth authorized"))

		return nil, err
	}

	p.incRecursionDepth()
	defer p.decRecursionDepth()

	// ID := expression
	if p.acceptAt(1, types.ASSIGNEMENT_DEFINITION) {
		varDeclarationNode, err := p.declarationAssignmentParser()

		if ! p.expect(types.EOL) {
			err = NewParseError(p.peek(), 
				errors.New("syntax for assignment delcaration did'nt end properly"))
		}
		
		return varDeclarationNode, err

	} else if p.acceptAt(1, types.ASSIGNEMENT) {
		varInitialization, err := p.initializationAssignmentParser()

		if ! p.expect(types.EOL) {
			err = NewParseError(p.peek(), errors.New("syntax for assignment did'nt end properly"))
		}

		return varInitialization, err
	
	} else if p.accept(types.COMMENT) {
		commentExpression := &CommentNode{ Kind: types.KIND_COMMENT, Value: p.peek(), Range: p.peek().Range }
		p.nextToken()

		var err *ParseError
		if ! p.expect(types.EOL) {
			err = NewParseError(p.peek(), errors.New("syntax for comment did'nt end properly"))
		}

		return commentExpression, err

	} else if p.accept(types.KEYWORD) {
		// Order of implementation: if/else/else if, end, range, define, template, block, with
		tokenValue := p.peek().Value

		if bytes.Compare(tokenValue, []byte("if")) == 0 {
			ifExpression := &GroupStatementNode{}
			ifExpression.Range = p.peek().Range

			p.nextToken()	// skip keyword "if"

			expression, err := p.StatementParser()

			if expression != nil {
				switch expression.GetKind() {
				case types.KIND_VARIABLE_ASSIGNMENT, types.KIND_VARIABLE_DECLARATION, types.KIND_MULTI_EXPRESSION, types.KIND_EXPRESSION:
					ifExpression.ControlFlow = expression
					ifExpression.Kind = types.KIND_IF
					ifExpression.Range.End = expression.GetRange().End
				default:
					if err == nil {
						err = NewParseError(&types.Token{}, errors.New("'if' do not accept this kind of statement"))
					}
				}
			}

			return ifExpression, err

		} else if bytes.Compare(tokenValue, []byte("else")) == 0 {
			tmpEpression := &GroupStatementNode{Kind: types.KIND_ELSE, Range: p.peek().Range}
			elseToken := p.peek()

			p.nextToken()

			var elseExpression types.AstNode = tmpEpression
			var err *ParseError
			if ! p.expect(types.EOL) {
				var expr types.AstNode
				expr, err = p.StatementParser()

				elseExpression = expr

				// TODO: produce the accurate else group statement
				if elseExpression != nil {
					switch expr.GetKind() {
					case types.KIND_IF:
						elseExpression.SetKind(types.KIND_ELSE_IF)
					case types.KIND_WITH:
						elseExpression.SetKind(types.KIND_ELSE_WITH)
					default:
						err = NewParseError(elseToken, errors.New("bad syntax for else statement"))
						err.Range = *expr.GetRange()
					}
				}
			}

			return elseExpression, err
		} else if bytes.Compare(tokenValue, []byte("end")) == 0 {
			endExpression := &GroupStatementNode{ Kind: types.KIND_END, Range: p.peek().Range, }

			p.nextToken()	// skip 'end' token

			var err *ParseError
			if !p.expect(types.EOL) {
				err = NewParseError(p.peek() , errors.New("'end' do not accept further expression"))
			}

			return endExpression, err
		} else if bytes.Compare(tokenValue, []byte("range")) == 0 {
			rangeExpression := &GroupStatementNode{Kind: types.KIND_RANGE_LOOP, Range: p.peek().Range}
			token := p.peek()

			p.nextToken()

			var err *ParseError
			if p.acceptAt(1, types.COMMA) {
				var expr *VariableDeclarationNode
				expr, err = p.doubleDeclarationAssignmentParser()
				rangeExpression.ControlFlow = expr

				if ! p.expect(types.EOL) && err == nil {
					err = NewParseError(token, errors.New("'range' statement have missing expression"))
				}
			} else {
				var expr types.AstNode
				expr, err = p.StatementParser()

				if expr != nil {
					switch expr.GetKind() {
					case types.KIND_VARIABLE_ASSIGNMENT, types.KIND_VARIABLE_DECLARATION, types.KIND_MULTI_EXPRESSION, types.KIND_EXPRESSION:
						rangeExpression.ControlFlow = expr
						rangeExpression.Range.End = expr.GetRange().End
					default:
						err = NewParseError(token, errors.New("'range' do not accept those type of expression"))
						err.Range = *expr.GetRange()
					}
				}
			}

			return rangeExpression, err

		} else if bytes.Compare(tokenValue, []byte("with")) == 0 {
			withExpression := &GroupStatementNode{Kind: types.KIND_WITH, Range: p.peek().Range}
			token := p.peek()

			p.nextToken()	// skip 'with' token

			expr, err := p.StatementParser()

			if expr != nil {
				switch expr.GetKind() {
				case types.KIND_VARIABLE_ASSIGNMENT, types.KIND_VARIABLE_DECLARATION, types.KIND_MULTI_EXPRESSION, types.KIND_EXPRESSION:
					withExpression.ControlFlow = expr
					withExpression.Range.End = expr.GetRange().End
				default:
					err = NewParseError(token, errors.New("'with' do not accept those type of expression"))
					err.Range = *expr.GetRange()
				}
			}


			return withExpression, err

		} else if bytes.Compare(tokenValue, []byte("block")) == 0 {
			blockExpression := &GroupStatementNode{ Kind: types.KIND_BLOCK_TEMPLATE, Range: p.peek().Range }

			p.nextToken()	// skip 'block' token

			var err *ParseError
			if p.accept(types.STRING) {
				templateExpression := &TemplateStatementNode{Kind: types.KIND_BLOCK_TEMPLATE, TemplateName: p.peek(), Range: p.peek().Range }
				templateExpression.parent = blockExpression
				blockExpression.ControlFlow = templateExpression
				token := p.peek()

				p.nextToken()

				var expr types.AstNode
				expr, err = p.StatementParser()

				if expr != nil {
					switch expr.GetKind() {
					case types.KIND_VARIABLE_ASSIGNMENT, types.KIND_VARIABLE_DECLARATION, types.KIND_MULTI_EXPRESSION, types.KIND_EXPRESSION:
						templateExpression.expression = expr
						templateExpression.Range.End = expr.GetRange().End
					default:
						err = NewParseError(token, errors.New("'block' do not accept those type of expression"))
						err.Range = *expr.GetRange()
					}
				}
			} else {
				err = NewParseError(p.peek(), errors.New("'block' expect a string next to it"))
			}


			return blockExpression, err
		} else if bytes.Compare(tokenValue, []byte("define")) == 0 {
			defineExpression := &GroupStatementNode{Kind: types.KIND_DEFINE_TEMPLATE, Range: p.peek().Range }

			p.nextToken()	// skip 'define' token

			var err *ParseError
			if p.accept(types.STRING) {
				templateExpression := &TemplateStatementNode{ Kind: types.KIND_DEFINE_TEMPLATE, TemplateName: p.peek(), Range: p.peek().Range }
				templateExpression.parent = defineExpression
				defineExpression.ControlFlow = templateExpression
				defineExpression.Range.End = templateExpression.Range.End

				p.nextToken()
			} else {
				err = NewParseError(p.peek(), errors.New("'define' expect a string next to it"))
			}

			if ! p.expect(types.EOL) && err == nil {
				err = NewParseError(p.peek(), errors.New("'define' do not accept any expression after its name"))
			}

			return defineExpression, err

		} else if bytes.Compare(tokenValue, []byte("template")) == 0 {
			templateExpression := &TemplateStatementNode{Kind: types.KIND_USE_TEMPLATE, Range: p.peek().Range, }
			templateExpression.parent = nil
			p.nextToken()	// skip 'template' token

			var err *ParseError
			if p.accept(types.STRING) {
				templateExpression.TemplateName = p.peek()
				token := p.peek()
				p.nextToken()

				var expr types.AstNode
				expr, err = p.StatementParser()

				if expr != nil {
					switch expr.GetKind() {
					case types.KIND_VARIABLE_ASSIGNMENT, types.KIND_VARIABLE_DECLARATION, types.KIND_MULTI_EXPRESSION, types.KIND_EXPRESSION:
						templateExpression.expression = expr
						templateExpression.Range.End = expr.GetRange().End
					default:
						err = NewParseError(token, errors.New("'template' do not accept those type of expression"))
						err.Range = *expr.GetRange()
					}
				}
			} else {
				err = NewParseError(p.peek(), errors.New("'template' expect a string next to it"))
			}


			return templateExpression, err

		}
	}

	expression, err := p.multiExpressionParser()

	if ! p.expect(types.EOL) {
		err = NewParseError(p.peek(), errors.New("syntax for expression did'nt end properly"))
	}

	return expression, err
}

func (p *Parser) declarationAssignmentParser() (*VariableDeclarationNode, *ParseError) {
	var err *ParseError

	if ! p.accept(types.DOLLAR_VARIABLE) {
		err = NewParseError(p.peek(), 
			errors.New("the variable name must start by '$' or only contains alphanumerical char"))

		return nil, err
	}

	variable := p.peek()
	p.nextToken()

	if ! p.expect(types.ASSIGNEMENT_DEFINITION) {
		err = NewParseError(p.peek(), errors.New("expected assignmement '=', but found something else"))

		return nil, err
	}

	expression, err := p.multiExpressionParser()

	varDeclarationNode := &VariableDeclarationNode {
		Kind: types.KIND_VARIABLE_DECLARATION,
		Value: expression,
		Range: types.Range{ Start: variable.Range.Start, End: expression.Range.End },
	}
	varDeclarationNode.VariableNames = append(varDeclarationNode.VariableNames, *variable)

	return varDeclarationNode, err
}

func (p *Parser) doubleDeclarationAssignmentParser() (*VariableDeclarationNode, *ParseError) {
	var err *ParseError

	if ! p.accept(types.DOLLAR_VARIABLE) {
		err = NewParseError(p.peek(), errors.New("expected variable begining with '$' but got something else"))
		return nil, err
	}

	firstVariable := p.peek()
	p.nextToken()

	if ! p.expect(types.COMMA) {
		err = NewParseError(p.peek(), errors.New("expected ',' to separate variable while declaring them"))
		return nil, err
	}

	varDeclarationNode, err := p.declarationAssignmentParser()
	if len(varDeclarationNode.VariableNames) == 1 {
		secondVariable := varDeclarationNode.VariableNames[0]
		varDeclarationNode.VariableNames = nil

		varDeclarationNode.VariableNames = append(varDeclarationNode.VariableNames, *firstVariable, secondVariable)
	}

	return varDeclarationNode, err
}

func (p *Parser) initializationAssignmentParser() (*VariableAssignationNode, *ParseError) {
	var err *ParseError

	if ! p.accept(types.DOLLAR_VARIABLE) {
		err = NewParseError(p.peek(), 
			errors.New("the variable name must start by '$' or '.', and only contains alphanumerical char"))

		return nil, err
	}

	variable := p.peek()
	p.nextToken()

	if ! p.expect(types.ASSIGNEMENT) {
		err = NewParseError(p.peek(), errors.New("expected assignmement '=', but found something else"))

		return nil, err
	}

	expression, err := p.multiExpressionParser()

	varAssignation := VariableAssignationNode{
		Kind: types.KIND_VARIABLE_ASSIGNMENT,
		VariableName: variable,
		Value: expression,
		Range: types.Range{ Start: variable.Range.Start, End: expression.Range.End },
	}

	return &varAssignation, err
}

func (p *Parser) multiExpressionParser() (*MultiExpressionNode, *ParseError) {
	expression := &MultiExpressionNode{}
	expression.Kind = types.KIND_MULTI_EXPRESSION

	expression.Range.Start = p.peek().Range.Start

	expr, err := p.expressionParser()
	expression.Expressions = append(expression.Expressions, *expr)

	var errLocal *ParseError
	for p.expect(types.PIPE) {
		expr, errLocal = p.expressionParser()
		expression.Expressions = append(expression.Expressions, *expr)

		if err == nil {
			err = errLocal
		}
	}

	expression.Range.End = expr.Range.End

	return expression, err
}

func (p *Parser) expressionParser() (*ExpressionNode, *ParseError) {
	expression := &ExpressionNode{}
	expression.Kind = types.KIND_EXPRESSION

	expression.Range.Start = p.peek().Range.Start

	var symbolName *types.Token
	for p.accept(types.FUNCTION) || p.accept(types.DOT_VARIABLE) || p.accept(types.DOLLAR_VARIABLE) || p.accept(types.STRING) || p.accept(types.NUMBER) {
		symbolName = p.peek()

		expression.Symbols = append(expression.Symbols, *symbolName)
		p.nextToken()
	}

	var err *ParseError
	if symbolName == nil {
		err = NewParseError(p.peek(), 
			errors.New("expected an expression but got '" + p.peek().ID.String() + "'"))
			// errors.New("empty expression not allowed"))

		return expression, err
	}

	expression.Range.End = symbolName.Range.End

	return expression, err
}

func (p Parser) peek() *types.Token {
	if len(p.input) == 0 { return nil }

	return &p.input[0]
}

func (p Parser) peekAt(pos int) *types.Token {
	if pos < len(p.input) {
		return &p.input[pos]
	}

	return nil
}

func (p *Parser) nextToken() {
	if len(p.input) == 0 {
		return 
	}

	p.input = p.input[1:]
}

// TODO: Rename to 'skipTillNextStatement()', 'flushInputUntilNextStatement()', 
// 'skipUntilEOL()'
func (p *Parser) flushInputUntilNextStatement() {
	for index, el := range p.input {
		if el.ID == types.EOL {
			index++
			p.input = p.input[index:]

			break
		}
	}
}

func (p Parser) accept(kind types.LexerKind) bool {
	if len(p.input) == 0 { return false }
	return p.input[0].ID == kind
}

func (p Parser) acceptAt(pos int, kind types.LexerKind) bool {
	if len(p.input) == 0 { return false }

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
	if len(p.input) == 0 { return false }

	return p.input[0].ID == types.EOL
}

func (p *Parser) incRecursionDepth() {
	p.currentRecursionDepth ++
}

func (p *Parser) decRecursionDepth() {
	p.currentRecursionDepth --
}

func (p Parser) isRecursionMaxDepth() bool {
	return p.currentRecursionDepth >= p.maxRecursionDepth
}

// TODO: rename to NewParseError()
func NewParseError(token *types.Token, err error) *ParseError {
	e := &ParseError{
		Err: err,
		Range: token.Range,
		Token: token,
	}

	return e
}

func getLastElement[E any](arr []E) E {
	size := len(arr)

	if size <= 0 {
		panic("cannot obtain the last element of an empty 'slice'")
	}

	return arr[size - 1]
}

