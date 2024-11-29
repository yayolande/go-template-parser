package parser

import (
	"fmt"
	"strings"

	"github.com/yayolande/gota/types"
)



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
	str := types.PrettyFormater(v.VariableNames)

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

func PrettyAstNodeFormater(nodes []types.AstNode) string {
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

func PrettyFormater[E fmt.Stringer] (nodes []E) string {
	str := ""

	if len(nodes) == 0 {
		str = "[]"
	} else {
		for _, node := range nodes {
			str += fmt.Sprintf("%v, ", node)
			// str += fmt.Sprintf("%#v, ", node)
		}

		str = "[" + str[:len(str) - 2] + "]"
	}

	return str
}

func Print(nodes ...types.AstNode) {
	str := PrettyAstNodeFormater(nodes)
	fmt.Println(str)
}

