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
func PrettyTokenFormater (tokens []Token) string {
	str := "["
	for _, tok := range tokens {
		str += fmt.Sprintf("%s,", tok)
	}

	str = str[:len(str) - 1]
	str += "]"

	return str
}
