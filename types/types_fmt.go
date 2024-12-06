package types

import "fmt"

func (p Position) String() string {
	return fmt.Sprintf("{ \"Line\": %d, \"Character\": %d }", p.Line, p.Character)
}

func (r Range) String() string {
	return fmt.Sprintf("{ \"Start\": %s, \"End\": %s }", r.Start, r.End)
}

func (t Token) String() string {
	return fmt.Sprintf("{ \"ID\": \"%s\", \"Range\": %s, \"Value\": %q }", t.ID, t.Range, t.Value)
}


// TODO: change name of this function -- array_to_string() ?
func PrettyFormater[T fmt.Stringer](arr []T) string {
	if len(arr) == 0 {
		return "[]"
	}

	str := "["
	for _, el := range arr {
		str += fmt.Sprintf("%s,", el)
	}

	str = str[:len(str) - 1]
	str += "]"

	return str
}

func Print(tokens ...Token) {
	str := PrettyFormater(tokens)
	fmt.Println(str)
}

func (k LexerKind) String() string {
	str := "NOT FOUND ID"

	switch k {
	case DOT_VARIABLE:
		str = "DOT_VARIABLE"
	case DOLLAR_VARIABLE:
		str = "DOLLAR_VARIABLE"
	case KEYWORD:
		str = "KEYWORD"
	case FUNCTION:
		str = "FUNCTION"
	case IDENTIFIER:
		str = "IDENTIFIER"
	case ASSIGNEMENT:
		str = "ASSIGNEMENT"
	case DECLARATION_ASSIGNEMENT:
		str = "DECLARATION_ASSIGNEMENT"
	case STRING:
		str = "STRING"
	case NUMBER:
		str = "NUMBER"
	case EQUAL_COMPARISON:
		str = "EQUAL_COMPARISON"
	case PIPE:
		str = "PIPE"
	case LEFT_PAREN:
		str = "LEFT_PAREN"
	case RIGTH_PAREN:
		str = "RIGTH_PAREN"
	case COMMENT:
		str = "COMMENT"
	case EOL:
		str = "EOL"
	case NOT_FOUND:
		str = "NOT_FOUND"
	case UNEXPECTED:
		str = "UNEXPECTED"
	case	COMMA:
		str = "COMMA"
	default:
		str = fmt.Sprintf("stringer() for 'lexer.Kind' type have found an unpected value: %d", k)
		panic(str)
	}

	return fmt.Sprintf(`%s`, str)
}

func (k ParserKind) String() string {
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
