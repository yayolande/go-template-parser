package lexer

import (
	"bytes"
	// "fmt"
	"regexp"
)

type Position struct {
	Line	int
	Character	int

}

type Range struct {
	Start	Position
	End	Position
}

// TODO: Refactor this to 'parser' package
type ParseError struct {
	Err	error
	Range
}

type LexerError struct {
	Err	error
	Range	Range
	Token	*Token
}

type Kind int

type Token struct {
	ID	Kind
	Range	Range
	Value	[]byte
}

const (
	VARIABLE Kind = iota
	DOT_VARIABLE
	DOLLAR_VARIABLE
	FUNCTION
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

func Tokenize(content []byte) []Token {
	content = bytes.Clone(content)

	templateCodes, templatePositions := extractTemplateCode(content)

	var tokens []Token
	var endOfragment Token

	for i := 0; i < len(templateCodes); i++ {
		code := templateCodes[i]
		position := templatePositions[i]

		fragment := tokenizeLine(code, position)

		endOfragment = Token { ID: EOL, Value: []byte("#EOL"), Range: position }
		fragment = append(fragment, endOfragment)

		tokens = append(tokens, fragment...)
	}

	return tokens
}

func extractTemplateCode (content []byte) ([][]byte, []Range) {
	var templateCode [][]byte
	var templatePositions []Range

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


		// Trim '{{' and '}}'
		insideTemplate := content[loc[0] + 2 : loc[1] - 2]

		templatePosition.Start.Character += 2
		templatePosition.End.Character -= 1
		templatePositions = append(templatePositions, templatePosition)

		templateCode = append(templateCode, insideTemplate)
		count += loc[1]

		content = content[loc[1]:]
	}

	return templateCode, templatePositions
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

func tokenizeLine(data []byte, initialPosition Range) []Token  {
	tokens := []Token{}

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
				line := currentLocalLineNumber + initialPosition.Start.Line
				column := currentLocalColumnNumber

				if currentLocalLineNumber == 0 {
					column += initialPosition.Start.Character
				}

				// what if line > initialPosition.End.Line ??

				pos := Range{
					Start: Position{Line: line, Character: column},
					End: Position{Line: line, Character: column + loc[1]},
				}

				currentLocalColumnNumber += loc[1]

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

	return tokens
}


