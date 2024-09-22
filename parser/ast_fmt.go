package parser

import (
	"fmt"
)


func (k Kind) String() string {
	val := "NOT FOUND!!!!!!!"

	switch k {
	case KIND_EXPRESSION:
		val = "KIND_EXPRESSION"
	case KIND_MULTI_EXPRESSION:
		val = "KIND_MULTI_EXPRESSION"
	case KIND_VARIABLE_ASSIGNMENT:
		val = "KIND_VARIABLE_ASSIGNMENT"
	case KIND_VARIABLE_DECLARATION:
		val = "KIND_VARIABLE_DECLARATION"
	case KIND_GROUP_STATEMENT:
		val = "KIND_GROUP_STATEMENT"
	}

	return fmt.Sprintf(`"%s"`, val)
}

func (v VariableDeclarationNode) String() string {
	return fmt.Sprintf(`{"Kind": %s, "VariableName": %q, "Value": %s}`, v.Kind, v.VariableName, v.Value)
}

func (v VariableAssignationNode) String() string {
	return fmt.Sprintf(`{"Kind": %s, "VariableName": %q, "Value": %s}`, v.Kind, v.VariableName, v.Value)
}

func (m MultiExpressionNode) String() string {
	str := ""
	
	if len(m.Expressions) == 0 {
		str = "[]"
	} else {
		for _, expression := range m.Expressions {
			str += fmt.Sprintf("%s, ", expression)
		}

		str = "[" + str[:len(str) - 2] + "]"
	}

	return fmt.Sprintf(`{"Kind": %s, "Expressions": %s}`, m.Kind, str)
}

func (e ExpressionNode) String() string {
	str := ""

	if len(e.ArgumentsName) == 0 {
		str = "[]"
	} else {
		for _, argName := range e.ArgumentsName {
			str += fmt.Sprintf("%q, ", argName)
		}

		str = "[" + str[:len(str) - 2] + "]"
	}

	return fmt.Sprintf(`{"Kind": %s, "FunctionName": %q, "ArgumentsName": %s}`, e.Kind, e.FunctionName, str)
}

func (g GroupStatementNode) String() string {
	return PrettyAstNodeFormater(g.Statements)
}

func PrettyAstNodeFormater(nodes []AstNode) string {
	str := ""

	if len(nodes) == 0 {
		str = "[]"
	} else {
		for _, node := range nodes {
			str += fmt.Sprintf("%s, ", node)
		}

		str = "[" + str[:len(str) - 2] + "]"
	}

	return str
}

