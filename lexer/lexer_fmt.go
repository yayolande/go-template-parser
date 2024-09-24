package lexer

import "fmt"

func (p Position) String() string {
	return fmt.Sprintf("{ \"Line\": %d, \"Character\": %d }", p.Line, p.Character)
}

func (r Range) String() string {
	return fmt.Sprintf("{ \"Start\": %s, \"End\": %s }", r.Start, r.End)
}

func (t Token) String() string {
	return fmt.Sprintf("{ \"ID\": %d, \"Range\": %s, \"Value\": %q }", t.ID, t.Range, t.Value)
}

func (e LexerError) String() string {
	return fmt.Sprintf(`{ "Err": "%s", "Range": %s, "Token": %s }`, e.Err.Error(), e.Range, e.Token)
}

//func PrettyFormater(arr []fmt.Stringer) string {
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


