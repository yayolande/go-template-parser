package parser

import (
	"bytes"
	"errors"
	"fmt"
	"go-template-parser/lexer"
)

type ParseError struct {
	Err	error
	Range	lexer.Range
	Token	*lexer.Token
}

type SymbolDefinition map[string]AstNode

type SemanticAnalizer struct {
	rootAstNode	AstNode
	builtinFunctionDefinition	SymbolDefinition
	customFunctionDefinition	SymbolDefinition	// Not sure it is necessary
	globalVariables	SymbolDefinition
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
			"eq": nil,
			"ne": nil,
			"lt": nil,
			"le": nil,
			"gt": nil,
			"ge": nil,
			"true": nil,	// Not sure about this
			"false": nil,	// Not sure about this
			"continue": nil,	// Not sure about this
			"break": nil,	// Not sure about this
		},
		customFunctionDefinition: SymbolDefinition {
		},	// ????????????????????????/
		globalVariables: SymbolDefinition {
			".": nil,
			"$": nil,
		},
	}

	return &analizer
}

func (a *SemanticAnalizer) definitionAnalysis() []ParseError {
	localVariables := SymbolDefinition{}

	root := a.rootAstNode
	errs := root.definitionAnalysis(a.globalVariables, localVariables, a.builtinFunctionDefinition)
	// fmt.Println(lexer.PrettyFormater(errs))

	return errs
}

func SemanticalAnalisis(root AstNode) []ParseError {
	analizer := createSemanticAnalizer(root)
	errs := analizer.definitionAnalysis()

	return errs
}

type Parser struct {
	input	[]lexer.Token
	openedNodeStack []AstNode
	maxRecursionDepth	int
	currentRecursionDepth	int
}

func createParser(tokens []lexer.Token) *Parser {
	input := make([]lexer.Token, len(tokens))
	copy(input, tokens)

	defaultGroupStatementNode := &GroupStatementNode{Kind: KIND_GROUP_STATEMENT}

	groupNodeStack := []AstNode{}
	groupNodeStack = append(groupNodeStack, defaultGroupStatementNode)

	parser := &Parser{
		input: input,
		openedNodeStack: groupNodeStack,
		maxRecursionDepth: 3,
		currentRecursionDepth: 0,
	}

	return parser
}

func appendStatementWithinRecentGroup(node AstNode, nodes []AstNode) *ParseError {
	var err *ParseError

	size := len(nodes)
	lastInserted, ok := nodes[size - 1].(*GroupStatementNode)

	if ok {
		lastInserted.Statements = append(lastInserted.Statements, node)
	} else {
		err = &ParseError{Err: errors.New("parser error, didn't find a statement holder, instead found something else")}
	}

	return err
}

func (p *Parser) safeStatementGrouping(node AstNode) *ParseError {
	var err *ParseError

	// 1. Check for fatal error, where there is no default group assigned
	if len(p.openedNodeStack) == 0 {
		err = &ParseError{Err: errors.New("Parser error, no initial scope available to hold the statements")}
		return err
	}

	// 2. Decide what to do
	switch node.getKind() {
	case KIND_IF, KIND_WITH, KIND_RANGE_LOOP, KIND_BLOCK_TEMPLATE, KIND_DEFINE_TEMPLATE:
		err = appendStatementWithinRecentGroup(node, p.openedNodeStack)
		p.openedNodeStack = append(p.openedNodeStack, node)
	case KIND_ELSE_IF:
		// check that it is only after: if, else if
		size := len(p.openedNodeStack)
		if size > 1 {
			lastInserted := p.openedNodeStack[size - 1]
			lastInsertedKind := p.openedNodeStack[size - 1].getKind()

			if lastInsertedKind == KIND_IF || lastInsertedKind == KIND_ELSE_IF {
				p.openedNodeStack = p.openedNodeStack[:size - 1]
				err = appendStatementWithinRecentGroup(node, p.openedNodeStack)
				p.openedNodeStack = append(p.openedNodeStack, node)
			} else {
				err = &ParseError{Range: *lastInserted.getRange(), 
					Err: errors.New("'else if' statement is not compatible with '" + lastInsertedKind.String() + "'"), }
			}
		} else {
			err = &ParseError{Range: *node.getRange(), 
				Err: errors.New("extraneous statement '" + node.getKind().String() + "'")}
		}
	case KIND_ELSE_WITH:
		// check that it is only after: with, else with
		size := len(p.openedNodeStack)
		if size > 1 {
			lastInserted := p.openedNodeStack[size - 1]
			lastInsertedKind := p.openedNodeStack[size - 1].getKind()

			if lastInsertedKind == KIND_WITH || lastInsertedKind == KIND_ELSE_WITH {
				p.openedNodeStack = p.openedNodeStack[:size - 1]
				err = appendStatementWithinRecentGroup(node, p.openedNodeStack)
				p.openedNodeStack = append(p.openedNodeStack, node)
			} else {
				err = &ParseError{Range: *lastInserted.getRange(), 
					Err: errors.New("'else with' statement is not compatible with '" + lastInsertedKind.String() + "'"), }
			}
		} else {
			err = &ParseError{Range: *node.getRange(), 
				Err: errors.New("extraneous statement '" + node.getKind().String() + "'")}
		}
	case KIND_ELSE:
		// check that it is only after: if, else if, with , else with, range
		size := len(p.openedNodeStack)
		if size > 1 {
			lastInserted := p.openedNodeStack[size - 1]
			switch lastInserted.getKind() {
			case KIND_IF, KIND_ELSE_IF, KIND_WITH, KIND_ELSE_WITH, KIND_RANGE_LOOP:
				p.openedNodeStack = p.openedNodeStack[:size - 1]
				err = appendStatementWithinRecentGroup(node, p.openedNodeStack)
				p.openedNodeStack = append(p.openedNodeStack, node)
			default:
				err = &ParseError{Range: *node.getRange(), 
					Err: errors.New("'else' statement is not compatible with '" + node.getKind().String() + "'")}
			}
		} else {
			err = &ParseError{Range: *node.getRange(), 
				Err: errors.New("extraneous statement '" + node.getKind().String() + "'")}
		}
	case KIND_END:
		size := len(p.openedNodeStack)
		if size > 1 {
			p.openedNodeStack = p.openedNodeStack[:size - 1]
			err = appendStatementWithinRecentGroup(node, p.openedNodeStack)
		} else {
			err = &ParseError{Range: *node.getRange(), Err: errors.New("extraneous 'end' statement detected")}
		}
	default:
		err = appendStatementWithinRecentGroup(node, p.openedNodeStack)
	}

	return err
}

func Parse(tokens []lexer.Token) (AstNode, []ParseError) {
	var errs []ParseError

	parser := createParser(tokens)
	nodes := []AstNode{}

	for ! parser.isEOF() {
		node, err := parser.StatementParser()

		if err != nil {
			errs = append(errs, *err)
			parser.nextStatement()
		} else {
			err = parser.safeStatementGrouping(node)

			if err != nil {
				errs = append(errs, *err)
			} else {
				nodes = append(nodes, node)
			}
		}
	}
	
	var defaultGroupStatementNode AstNode
	if len(parser.openedNodeStack) == 1 {
		defaultGroupStatementNode = parser.openedNodeStack[0]
	} else if len(parser.openedNodeStack) > 1 {
		lastInserted := parser.openedNodeStack[len(parser.openedNodeStack) - 1]
		err := ParseError{Range: *lastInserted.getRange(), 
			Err: errors.New("not all group statements ('if/else/define/block/with') have been properly claused")}

		errs = append(errs, err)
	} else {
		err := ParseError{Err: errors.New("parser fatal error, unable to build a parse tree")}
		errs = append(errs, err)
	}

	fmt.Print("")
	// fmt.Println(errs)
	return defaultGroupStatementNode, errs
}

func (p *Parser) StatementParser() (AstNode, *ParseError) {
	if (p.isRecursionMaxDepth()) {
		err := createParseError(p.peek(), errors.New("parser error, reached the max depth authorized"))

		return nil, err
	}

	p.incRecursionDepth()
	defer p.decRecursionDepth()

	// ID := expression
	if p.acceptAt(1, lexer.ASSIGNEMENT_DEFINITION) {
		varDeclarationNode, err := p.declarationAssignmentParser()

		if ! p.expect(lexer.EOL) {
			err = createParseError(p.peek(), 
				errors.New("syntax for assignment delcaration did'nt end properly"))
		}
		
		return varDeclarationNode, err

	} else if p.acceptAt(1, lexer.ASSIGNEMENT) {
		varInitialization, err := p.initializationAssignmentParser()

		if ! p.expect(lexer.EOL) {
			err = createParseError(p.peek(), errors.New("syntax for assignment did'nt end properly"))
		}

		return varInitialization, err
	
	} else if p.accept(lexer.COMMENT) {
		commentExpression := &CommentNode{ Kind: KIND_COMMENT, Value: p.peek(), Range: p.peek().Range }
		p.nextToken()

		var err *ParseError
		if ! p.expect(lexer.EOL) {
			err = createParseError(p.peek(), errors.New("syntax for comment did'nt end properly"))
		}

		return commentExpression, err

	} else if p.accept(lexer.KEYWORD) {
		// Order of implementation: if/else/else if, end, range, define, template, block, with
		tokenValue := p.peek().Value

		if bytes.Compare(tokenValue, []byte("if")) == 0 {
			ifExpression := &GroupStatementNode{}
			ifExpression.Range = p.peek().Range

			p.nextToken()	// skip keyword "if"

			expression, err := p.StatementParser()

			if expression != nil {
				switch expression.getKind() {
				case KIND_VARIABLE_ASSIGNMENT, KIND_VARIABLE_DECLARATION, KIND_MULTI_EXPRESSION, KIND_EXPRESSION:
					ifExpression.controlFlow = expression
					ifExpression.Kind = KIND_IF
					ifExpression.Range.End = expression.getRange().End
				default:
					if err == nil {
						err = createParseError(&lexer.Token{}, errors.New("'if' do not accept this kind of statement"))
					}
				}
			}

			return ifExpression, err

		} else if bytes.Compare(tokenValue, []byte("else")) == 0 {
			tmpEpression := &GroupStatementNode{Kind: KIND_ELSE, Range: p.peek().Range}
			elseToken := p.peek()

			p.nextToken()

			var elseExpression AstNode = tmpEpression
			var err *ParseError
			if ! p.expect(lexer.EOL) {
				var expr AstNode
				expr, err = p.StatementParser()

				elseExpression = expr

				// TODO: produce the accurate else group statement
				if elseExpression != nil {
					switch expr.getKind() {
					case KIND_IF:
						elseExpression.setKind(KIND_ELSE_IF)
					case KIND_WITH:
						elseExpression.setKind(KIND_ELSE_WITH)
					default:
						err = createParseError(elseToken, errors.New("bad syntax for else statement"))
						err.Range = *expr.getRange()
					}
				}
			}

			return elseExpression, err
		} else if bytes.Compare(tokenValue, []byte("end")) == 0 {
			endExpression := &GroupStatementNode{ Kind: KIND_END, Range: p.peek().Range, }

			p.nextToken()	// skip 'end' token

			var err *ParseError
			if !p.expect(lexer.EOL) {
				err = createParseError(p.peek() , errors.New("'end' do not accept further expression"))
			}

			return endExpression, err
		} else if bytes.Compare(tokenValue, []byte("range")) == 0 {
			rangeExpression := &GroupStatementNode{Kind: KIND_RANGE_LOOP, Range: p.peek().Range}
			token := p.peek()

			p.nextToken()

			var err *ParseError
			if p.acceptAt(1, lexer.COMMA) {
				var expr *VariableDeclarationNode
				expr, err = p.doubleDeclarationAssignmentParser()
				rangeExpression.controlFlow = expr

				if ! p.expect(lexer.EOL) && err == nil {
					err = createParseError(token, errors.New("'range' statement have missing expression"))
				}
			} else {
				var expr AstNode
				expr, err = p.StatementParser()

				if expr != nil {
					switch expr.getKind() {
					case KIND_VARIABLE_ASSIGNMENT, KIND_VARIABLE_DECLARATION, KIND_MULTI_EXPRESSION, KIND_EXPRESSION:
						rangeExpression.controlFlow = expr
						rangeExpression.Range.End = expr.getRange().End
					default:
						err = createParseError(token, errors.New("'range' do not accept those type of expression"))
						err.Range = *expr.getRange()
					}
				}
			}

			return rangeExpression, err

		} else if bytes.Compare(tokenValue, []byte("with")) == 0 {
			withExpression := &GroupStatementNode{Kind: KIND_WITH, Range: p.peek().Range}
			token := p.peek()

			p.nextToken()	// skip 'with' token

			expr, err := p.StatementParser()

			if expr != nil {
				switch expr.getKind() {
				case KIND_VARIABLE_ASSIGNMENT, KIND_VARIABLE_DECLARATION, KIND_MULTI_EXPRESSION, KIND_EXPRESSION:
					withExpression.controlFlow = expr
					withExpression.Range.End = expr.getRange().End
				default:
					err = createParseError(token, errors.New("'with' do not accept those type of expression"))
					err.Range = *expr.getRange()
				}
			}


			return withExpression, err

		} else if bytes.Compare(tokenValue, []byte("block")) == 0 {
			blockExpression := &GroupStatementNode{ Kind: KIND_BLOCK_TEMPLATE, Range: p.peek().Range }

			p.nextToken()	// skip 'block' token

			var err *ParseError
			if p.accept(lexer.STRING) {
				templateExpression := &TemplateStatementNode{Kind: KIND_BLOCK_TEMPLATE, templateName: p.peek(), Range: p.peek().Range }
				blockExpression.controlFlow = templateExpression
				token := p.peek()

				p.nextToken()

				var expr AstNode
				expr, err = p.StatementParser()

				if expr != nil {
					switch expr.getKind() {
					case KIND_VARIABLE_ASSIGNMENT, KIND_VARIABLE_DECLARATION, KIND_MULTI_EXPRESSION, KIND_EXPRESSION:
						templateExpression.expression = expr
						templateExpression.Range.End = expr.getRange().End
					default:
						err = createParseError(token, errors.New("'block' do not accept those type of expression"))
						err.Range = *expr.getRange()
					}
				}
			} else {
				err = createParseError(p.peek(), errors.New("'block' expect a string next to it"))
			}


			return blockExpression, err
		} else if bytes.Compare(tokenValue, []byte("define")) == 0 {
			defineExpression := &GroupStatementNode{Kind: KIND_DEFINE_TEMPLATE, Range: p.peek().Range }

			p.nextToken()	// skip 'define' token

			var err *ParseError
			if p.accept(lexer.STRING) {
				templateExpression := &TemplateStatementNode{ Kind: KIND_DEFINE_TEMPLATE, templateName: p.peek(), Range: p.peek().Range }
				defineExpression.controlFlow = templateExpression
				defineExpression.Range.End = templateExpression.Range.End

				p.nextToken()
			} else {
				err = createParseError(p.peek(), errors.New("'define' expect a string next to it"))
			}

			if ! p.expect(lexer.EOL) && err == nil {
				err = createParseError(p.peek(), errors.New("'define' do not accept any expression after its name"))
			}

			return defineExpression, err

		} else if bytes.Compare(tokenValue, []byte("template")) == 0 {
			templateExpression := &TemplateStatementNode{Kind: KIND_USE_TEMPLATE, Range: p.peek().Range, }
			p.nextToken()	// skip 'template' token

			var err *ParseError
			if p.accept(lexer.STRING) {
				templateExpression.templateName = p.peek()
				token := p.peek()
				p.nextToken()

				var expr AstNode
				expr, err = p.StatementParser()

				if expr != nil {
					switch expr.getKind() {
					case KIND_VARIABLE_ASSIGNMENT, KIND_VARIABLE_DECLARATION, KIND_MULTI_EXPRESSION, KIND_EXPRESSION:
						templateExpression.expression = expr
						templateExpression.Range.End = expr.getRange().End
					default:
						err = createParseError(token, errors.New("'template' do not accept those type of expression"))
						err.Range = *expr.getRange()
					}
				}
			} else {
				err = createParseError(p.peek(), errors.New("'template' expect a string next to it"))
			}


			return templateExpression, err

		}
	}

	expression, err := p.multiExpressionParser()

	if ! p.expect(lexer.EOL) {
		err = createParseError(p.peek(), errors.New("syntax for expression did'nt end properly"))
	}

	return expression, err
}

func (p *Parser) declarationAssignmentParser() (*VariableDeclarationNode, *ParseError) {
	var err *ParseError

	if ! p.accept(lexer.DOLLAR_VARIABLE) {
		err = createParseError(p.peek(), 
			errors.New("the variable name must start by '$' or only contains alphanumerical char"))

		return nil, err
	}

	variable := p.peek()
	p.nextToken()

	if ! p.expect(lexer.ASSIGNEMENT_DEFINITION) {
		err = createParseError(p.peek(), errors.New("expected assignmement '=', but found something else"))

		return nil, err
	}

	expression, err := p.multiExpressionParser()

	varDeclarationNode := &VariableDeclarationNode {
		Kind: KIND_VARIABLE_DECLARATION,
		Value: expression,
		Range: lexer.Range{ Start: variable.Range.Start, End: expression.Range.End },
	}
	varDeclarationNode.VariableNames = append(varDeclarationNode.VariableNames, *variable)

	return varDeclarationNode, err
}

func (p *Parser) doubleDeclarationAssignmentParser() (*VariableDeclarationNode, *ParseError) {
	var err *ParseError

	if ! p.accept(lexer.DOLLAR_VARIABLE) {
		err = createParseError(p.peek(), errors.New("expected variable begining with '$' but got something else"))
		return nil, err
	}

	firstVariable := p.peek()
	p.nextToken()

	if ! p.expect(lexer.COMMA) {
		err = createParseError(p.peek(), errors.New("expected ',' to separate variable while declaring them"))
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

	if ! p.accept(lexer.DOLLAR_VARIABLE) {
		err = createParseError(p.peek(), 
			errors.New("the variable name must start by '$' or '.', and only contains alphanumerical char"))

		return nil, err
	}

	variable := p.peek()
	p.nextToken()

	if ! p.expect(lexer.ASSIGNEMENT) {
		err = createParseError(p.peek(), errors.New("expected assignmement '=', but found something else"))

		return nil, err
	}

	expression, err := p.multiExpressionParser()

	varAssignation := VariableAssignationNode{
		Kind: KIND_VARIABLE_ASSIGNMENT,
		VariableName: variable,
		Value: expression,
		Range: lexer.Range{ Start: variable.Range.Start, End: expression.Range.End },
	}

	return &varAssignation, err
}

func (p *Parser) multiExpressionParser() (*MultiExpressionNode, *ParseError) {
	expression := &MultiExpressionNode{}
	expression.Kind = KIND_MULTI_EXPRESSION

	expression.Range.Start = p.peek().Range.Start

	expr, err := p.expressionParser()
	expression.Expressions = append(expression.Expressions, *expr)

	var errLocal *ParseError
	for p.expect(lexer.PIPE) {
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
	expression.Kind = KIND_EXPRESSION

	expression.Range.Start = p.peek().Range.Start

	var symbolName *lexer.Token
	for p.accept(lexer.FUNCTION) || p.accept(lexer.DOT_VARIABLE) || p.accept(lexer.DOLLAR_VARIABLE) || p.accept(lexer.STRING) || p.accept(lexer.NUMBER) {
		symbolName = p.peek()

		expression.Symbols = append(expression.Symbols, *symbolName)
		p.nextToken()
	}

	var err *ParseError
	if symbolName == nil {
		err = createParseError(p.peek(), 
			errors.New("expected an expression but got '" + p.peek().ID.String() + "'"))
			// errors.New("empty expression not allowed"))

		return expression, err
	}

	expression.Range.End = symbolName.Range.End

	return expression, err
}

func (p Parser) peek() *lexer.Token {
	if len(p.input) == 0 { return nil }

	return &p.input[0]
}

func (p Parser) peekAt(pos int) *lexer.Token {
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

func (p *Parser) nextStatement() {
	for index, el := range p.input {
		if el.ID == lexer.EOL {
			index++
			p.input = p.input[index:]

			break
		}
	}
}

func (p Parser) accept(kind lexer.Kind) bool {
	if len(p.input) == 0 { return false }
	return p.input[0].ID == kind
}

func (p Parser) acceptAt(pos int, kind lexer.Kind) bool {
	if len(p.input) == 0 { return false }

	if pos >= len(p.input) {
		return false
	}

	return p.input[pos].ID == kind
}

func (p *Parser) expect(kind lexer.Kind) bool {
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

	return p.input[0].ID == lexer.EOL
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

func createParseError(token *lexer.Token, err error) *ParseError {
	e := &ParseError{
		Err: err,
		Range: token.Range,
		Token: token,
	}

	return e
}


