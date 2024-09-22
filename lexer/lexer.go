package lexer

import (
	"bytes"
	"fmt"
	"regexp"
)

type Position struct {
	Line	int
	Character	int

}

func (p Position) String() string {
	return fmt.Sprintf("{ \"Line\": %d, \"Character\": %d }", p.Line, p.Character)
}

type Range struct {
	Start	Position
	End	Position
}

func (r Range) String() string {
	return fmt.Sprintf("{ \"Start\": %s, \"End\": %s }", r.Start, r.End)
}

type ParseError struct {
	Err	error
	Range
}

type Kind int

type Token struct {
	ID	Kind
	Range	Range
	Value	[]byte
}

func (t Token) String() string {
	return fmt.Sprintf("{ \"ID\": %d, \"Range\": %s, \"Value\": %q }", t.ID, t.Range, t.Value)
}

const (
	VARIABLE Kind = iota
	IDENTIFIER
	ASSIGNEMENT
	ASSIGNEMENT_DEFINITION
	STRING
	NUMBER
	EQUAL_COMPARISON
	PIPE
	EOL	// End Of Line
	EOF
	NOT_FOUND
)

/*
func main() {
	tokenizer()
}
*/
func PrettyTokenFormater (tokens []Token) string {
	str := "["
	for _, tok := range tokens {
		str += fmt.Sprintf("%s,", tok)
	}

	str = str[:len(str) - 1]
	str += "]"

	// fmt.Printf("%s", str)
	return str
}

func Tokenizer(content []byte) []Token {
	var templateCode [][]byte
	var templateCodePosition []Range

	// content :=  []byte("<p>Hello, {{  \n  .Name \n \n}}</p>")
	// content = append(content, []byte("<p>{{   $rita    \n:=\n.user.friends.favorite \r\t }} -- {{ print .user.friends.best.age }}</p>")...)
	// content = append(content, []byte("\n {{ print \"dummy text\" | convert_to_int | isOkay }}")...)

	// filterTemplateStatementOnly
	captureTemplateStatementOnly := regexp.MustCompile("(?:{{(?:.|[\n\t\r])*?}})")

	count := 0
	currentLine := 0
	currentColumn := 0

	for {
		// TODO: Allocation below is not performant. Change it !
		loc := captureTemplateStatementOnly.FindIndex([]byte(content))
		if loc == nil {
			break
		}

		// alt name: templateDelimitatorPosition
		templatePosition := Range{}
		templatePosition.Start = convertToTextEditorPosition(content, loc[0])
		templatePosition.End = convertToTextEditorPosition(content, loc[1] - 1)

		if templatePosition.End.Line != 0 {
			currentColumn = 0
		} else {
			currentColumn++
		}

		templatePosition.Start.Character += currentColumn
		templatePosition.End.Character += currentColumn

		templatePosition.Start.Line += currentLine
		templatePosition.End.Line += currentLine

		currentLine = templatePosition.End.Line
		currentColumn = templatePosition.End.Character

		templateCodePosition = append(templateCodePosition, templatePosition)

		insideTemplate := content[loc[0] + 2 : loc[1] - 2] // Trim '{{' and '}}'
		templateCode = append(templateCode, insideTemplate)
		count += loc[1]

		content = content[loc[1]:]
	}

	var tokens []Token
	var endOfragment Token

	for _, lineCode := range templateCode {
		fragment, pos := tokenize(lineCode)
		_ = pos

		endOfragment = Token { ID: EOL, Value: []byte("#EOL") }
		fragment = append(fragment, endOfragment)

		tokens = append(tokens, fragment...)
	}

	/*
	str := "["
	for _, tok := range tokens {
		str += fmt.Sprintf("%s,", tok)
	}

	str = str[:len(str) - 1]
	str += "]"

	fmt.Printf("%s", str)
	*/

	return tokens

	// 1. First step, fetch only necessary contain 

	// 2. Tokenize the template extracted from text

	// 3. Repeat Step 1. and 2. until there is no more text read
}

// Alt name: getCursorPosition(data, charPos)

// Find better name
// findPosition (bufferTextEditor, charLocation2D)
func convertToTextEditorPosition (buffer []byte, charIndex int) Position {
	// WARNING: This implementation do not count escaped character (\r\t ...) properly
	var index, characterCount int
	var line int

	for {
		index = bytes.Index(buffer, []byte("\n"))
		if characterCount + index > charIndex || index == -1 {
			break
		}

		line++
		characterCount += index + 1
		buffer = buffer[index + 1 : ]
	}

	charPosition := Position{ Line: (line), Character: (charIndex - characterCount) }

	return charPosition
}

func tokenize(data []byte) ([]Token, []Range) {
	tokens := []Token{}
	tokensPosition := []Range{}

	ignorePattern := []string{`\s+`}

	type PatternToken struct {
		Value	string
		ID	Kind
	}

	// Tokens' meaning: VariableName, ID (function ?), '==' '=' ':='
	patternTokens:= []PatternToken{
		{
			Value: `(?:[.$]\w+)+`,
			ID: VARIABLE,
		},
		{
			Value: `\w+`,
			ID: IDENTIFIER,
		},
		// TODO: Add Number as well
		{
			Value: `"[^"]+"`,
			ID: STRING,
		},
		{
			Value: "==",
			ID: EQUAL_COMPARISON,
		},
		{
			Value: "=",
			ID: ASSIGNEMENT,
		},
		{
			Value: ":=",
			ID: ASSIGNEMENT_DEFINITION,
		},
		{
			Value: "[|]",
			ID: PIPE,
		},
	}

	var lengthDataStart int = -1
	var currentLocalLineNumber, currentLocalColumnNumber int

	for len(data) > 0 && lengthDataStart != len(data) {
		lengthDataStart = len(data)

		for _, pattern := range ignorePattern {
			reg := regexp.MustCompile(pattern)
			loc := reg.FindIndex(data)

			if loc != nil && loc[0] == 0 {
				content := data[loc[0]:loc[1]]

				position := convertToTextEditorPosition(content, loc[1])
				if position.Line != 0 {
					currentLocalColumnNumber = 0
				}

				currentLocalLineNumber += position.Line
				currentLocalColumnNumber += position.Character

				data = data[loc[1]:]
			}
		}

		for _, pattern := range patternTokens {
			reg := regexp.MustCompile(pattern.Value)
			loc := reg.FindIndex(data)

			if loc != nil && loc[0] == 0 {
				pos := Range{
					Start: Position{Line: currentLocalLineNumber, Character: currentLocalColumnNumber},
					End: Position{Line: currentLocalLineNumber, Character: currentLocalColumnNumber + loc[1]},
				}

				currentLocalColumnNumber += loc[1]

				tokensPosition = append(tokensPosition, pos)
				token := Token {
					Value: data[0:loc[1]],
					ID: pattern.ID,
					Range: pos,
				}
				tokens = append(tokens, token)
				data = data[loc[1]:]

				break
			}
		}
	}

	if len(data) > 0 {
		// TODO: This code should return an error in a future iteration
		// error = "unexpected char token in the template statement (??)
		token := Token {
			Value: data,
			ID: NOT_FOUND,
		}

		tokens = append(tokens, token)
		data = nil
	}

	return tokens, tokensPosition
}


