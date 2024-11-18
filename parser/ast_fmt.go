package parser

import (
	"fmt"
	"go-template-parser/lexer"
	"strings"
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
	case KIND_COMMENT:
		val = "KIND_COMMENT"
	case KIND_IF:
		val = "KIND_IF"
	case KIND_ELSE_IF:
		val = "KIND_ELSE_IF"
	case KIND_ELSE:
		val = "KIND_ELSE"
	case KIND_WITH:
		val = "KIND_WITH"
	case KIND_ELSE_WITH:
		val = "KIND_ELSE_WITH"
	case KIND_BLOCK_TEMPLATE:
		val = "KIND_BLOCK_TEMPLATE"
	case KIND_RANGE_LOOP:
		val = "KIND_RANGE_LOOP"
	case KIND_DEFINE_TEMPLATE:
		val = "KIND_DEFINE_TEMPLATE"
	case KIND_USE_TEMPLATE:
		val = "KIND_USE_TEMPLATE"
	case KIND_END:
		val = "KIND_END"
	}

	return fmt.Sprintf(`"%s"`, val)
}

func (e ParseError) String() string {
	to := "\"\""
	err := "\"\""

	if e.Err != nil {
		err = e.Err.Error()
		err = strings.ReplaceAll(err, "\"", "'")
	}
	if e.Token != nil {
		to = fmt.Sprint(*e.Token)
	}

	return fmt.Sprintf(`{"Err": "%s", "Range": %s, "Token": %s}`, err, e.Range, to)
}

func (v VariableDeclarationNode) String() string {
	value := `""`
	if v.Value != nil { value = fmt.Sprint(v.Value) }
	str := lexer.PrettyFormater(v.VariableNames)

	return fmt.Sprintf(`{"Kind": %s, "Range": %s, "VariableNames": %s, "Value": %s}`, v.Kind, v.Range, str, value)
}

func (v VariableAssignationNode) String() string {
	variableName := `""`
	value := `""`
	if v.VariableName != nil { variableName = fmt.Sprint(v.VariableName) }
	if v.Value != nil { value = fmt.Sprint(v.Value) }

	return fmt.Sprintf(`{"Kind": %s, "Range": %s, "VariableName": %s, "Value": %s}`, v.Kind, v.Range, variableName, value)
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

	return fmt.Sprintf(`{"Kind": %s, "Range": %s, "Expressions": %s}`, m.Kind, m.Range, str)
}

func (e ExpressionNode) String() string {
	str := ""

	if len(e.Symbols) == 0 {
		str = "[]"
	} else {
		for _, symbol := range e.Symbols {
			str += fmt.Sprintf("%s, ", symbol)
		}

		str = "[" + str[:len(str) - 2] + "]"
	}

	return fmt.Sprintf(`{"Kind": %s, "Range": %s, "Symbols": %s, "isError": "%t", "isFunctionCall": "%t"}`, e.Kind, e.Range, str, e.isError, e.isFunctionCall)
}

func (t TemplateStatementNode) String() string {
	templateName := `""`
	expression := `""`
	if t.TemplateName !=  nil { templateName = fmt.Sprint(t.TemplateName) }
	if t.expression != nil { expression = fmt.Sprint(t.expression) }

	return fmt.Sprintf(`{"Kind": %s, "Range": %s, "templateName": %s, "expression": %s}`, t.Kind, t.Range, templateName, expression)
}

func (g GroupStatementNode) String() string {

	strControlFlow := "{}"
	if g.ControlFlow != nil { strControlFlow = fmt.Sprintf("%s", g.ControlFlow) }
	str :=  PrettyAstNodeFormater(g.Statements)

	return fmt.Sprintf(`{"Kind": %s, "Range": %s, "controlFlow": %s, "Statements": %s}`, g.Kind, g.Range, strControlFlow, str)
}

func (c CommentNode) String() string {
	value := `""`
	if c.Value != nil { value = fmt.Sprint(*c.Value) }

	return fmt.Sprintf(`{"Kind": %s, "Range": %s, "Value": %s}`, c.Kind, c.Range, value)
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

