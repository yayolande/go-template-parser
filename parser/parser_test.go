package parser

import (
	"strconv"
	"testing"
	// go-template-parser/lexer"
)

func TestSafeStatementGroupingDepth(t *testing.T) {
	type input struct {
		statement	AstNode
	}

	type want struct {
		stackDepth	int
		isReplaceCurrentScope	bool
	}

	type datium struct {
		input
		want
	}

	// TODO: Test the case when we have 'user error' as input
	data := []datium{
		{
			input: input{
				// The type have no incidence on the grouping. Instead, everything is decided by 'statement.Kind'
				statement: &GroupStatementNode{Kind: KIND_IF},	
			},
			want: want{
				stackDepth: 2,
			},
		},
		{
			input: input{
				statement: &GroupStatementNode{Kind: KIND_MULTI_EXPRESSION},
			},
			want: want{
				stackDepth: 2,
			},
		},
		{
			input: input{
				statement: &GroupStatementNode{Kind: KIND_ELSE},
			},
			want: want{
				stackDepth: 2,
				isReplaceCurrentScope: true,
			},
		},
		{
			input: input{
				statement: &GroupStatementNode{Kind: KIND_END},
			},
			want: want{
				stackDepth: 1,
			},
		},
		{
			input: input{
				statement: &GroupStatementNode{Kind: KIND_VARIABLE_ASSIGNMENT},
			},
			want: want{
				stackDepth: 1,
			},
		},
		{
			input: input{
				statement: &GroupStatementNode{Kind: KIND_EXPRESSION},
			},
			want: want{
				stackDepth: 1,
			},
		},
		{
			input: input{
				statement: &GroupStatementNode{Kind: KIND_END},
			},
			want: want{
				stackDepth: 1,
			},
		},
	}

	rootScope := &GroupStatementNode{Kind: KIND_GROUP_STATEMENT}
	initGroup := append([]AstNode{}, AstNode(rootScope))

	parser := Parser{
		openedNodeStack: initGroup,
	}

	var backupStack []AstNode = make([]AstNode, len(parser.openedNodeStack))
	copy(backupStack, parser.openedNodeStack)

	testName := ""
	for count, el := range data {
		testName = "stmtGrouping_" + strconv.Itoa(count)

		t.Run(testName, func(t *testing.T) {
			node := el.input.statement
			parser.safeStatementGrouping(node)

			gotDepth := len(parser.openedNodeStack)
			expectDepth := el.want.stackDepth

			// 1. Check depth level
			if gotDepth != expectDepth {
				t.Errorf("expected stackDepth = %d \ngot stackDepth = %d", expectDepth, gotDepth)
			}

			// 2. Check that rootScope hasn't changed
			if parser.openedNodeStack[0] != rootScope {
				t.Errorf("root scope has changed unadvertly")
			}

			// 3. Check that element at the bottom of the stack hasn't been modified when the top is
			for count, scope := range parser.openedNodeStack {
				if count >= len(backupStack) {
					continue
				}

				if scope != backupStack[count] && ! el.isReplaceCurrentScope {
					t.Errorf("'openedNodeStack' changed between the last run. Particularly, scope at index '%d' is now different", count)
				}
			}

			// Backup the current state, so as to compare it to the next state
			backupStack = make([]AstNode, len(parser.openedNodeStack))
			copy(backupStack, parser.openedNodeStack)

		})
	}
}
