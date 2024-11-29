package lexer

import "fmt"

func (e LexerError) String() string {
	return fmt.Sprintf(`{ "Err": "%s", "Range": %s, "Token": %s }`, e.Err.Error(), e.Range, e.Token)
}
