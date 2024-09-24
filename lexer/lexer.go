package lexer

import (
	"bytes"
	"errors"
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
	UNEXPECTED
)

func Tokenize(content []byte) ([]Token, []Token, []LexerError) {
	content = bytes.Clone(content)

	templateCodes, templatePositions := extractTemplateCode(content)

	// fmt.Printf("templateLines = %q", templateCodes)
	// fmt.Println(PrettyFormater(templatePositions))

	var errs []LexerError
	var tokens []Token
	var failedToken []Token
	var endOfragment Token

	for i := 0; i < len(templateCodes); i++ {
		code := templateCodes[i]
		position := templatePositions[i]

		fragment, tokenErrs := tokenizeLine(code, position)

		endOfragment = Token { ID: EOL, Value: []byte("#EOL"), Range: position }
		fragment = append(fragment, endOfragment)

		if tokenErrs != nil {
			failedToken = append(failedToken, fragment...)
			errs = append(errs, tokenErrs...)

			continue
		}

		tokens = append(tokens, fragment...)
	}

	return tokens, failedToken, errs
}

func extractTemplateCode (content []byte) ([][]byte, []Range) {
	var templateCode [][]byte
	var templatePositions []Range

	captureLonelyTemplateDelimitator := regexp.MustCompile("{{|}}")
	captureTemplateStatementOnly := regexp.MustCompile("(?:{{(?:[^{}]|[\n\r\t])*?}})")

	currentLine := 0
	currentColumn := 0

	var loc, loneLoc []int
	var templatePosition Range

	for {
		// TODO: Allocation below is not performant. Change it !
		loneLoc = captureLonelyTemplateDelimitator.FindIndex(content)
		loc = captureTemplateStatementOnly.FindIndex(content)

		if loc == nil {
			// Do some checking before break out of the loop
			if loneLoc != nil {
				templatePosition = convertRangeIndexToTextEditorPosition(content, loneLoc, currentLine, currentColumn)

				templatePositions = append(templatePositions, templatePosition)
				templateCode = append(templateCode, content[loneLoc[0]:loneLoc[1]])
			}

			break
		}

		// A lone delimitator has been found in the wild, do something
		if loneLoc[0] < loc[0] {
			templatePosition = convertRangeIndexToTextEditorPosition(content, loneLoc, currentLine, currentColumn)

			templatePositions = append(templatePositions, templatePosition)
			templateCode = append(templateCode, content[loneLoc[0]:loneLoc[1]])
		}

		templatePosition = convertRangeIndexToTextEditorPosition(content, loc, currentLine, currentColumn)

		currentLine = templatePosition.End.Line
		currentColumn = templatePosition.End.Character + 1

		// Trim '{{' and '}}'
		insideTemplate := content[loc[0] + 2 : loc[1] - 2]

		templatePosition.Start.Character += 2
		templatePosition.End.Character -= 1

		templatePositions = append(templatePositions, templatePosition)
		templateCode = append(templateCode, insideTemplate)

		content = content[loc[1]:]
	}

	return templateCode, templatePositions
}

func convertSingleIndexToTextEditorPosition (buffer []byte, charIndex int) Position {
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

func convertRangeIndexToTextEditorPosition(editorContent []byte, rangeIndex []int, initialLine, initialColumn int) Range {
	position := Range{}
	position.Start = convertSingleIndexToTextEditorPosition(editorContent, rangeIndex[0])
	position.End = convertSingleIndexToTextEditorPosition(editorContent, rangeIndex[1] - 1)

	if position.Start.Line == 0 {
		position.Start.Character += initialColumn 
	}

	if position.End.Line == 0 {
		position.End.Character += initialColumn 
	}

	position.Start.Line += initialLine
	position.End.Line += initialLine

	return position
}

func tokenizeLine(data []byte, initialPosition Range) ([]Token, []LexerError)  {
	tokens := []Token{}

	ignorePattern := string(`\s+`)
	regIgnore := regexp.MustCompile(ignorePattern)

	type PatternToken struct {
		Value	string
		ID	Kind
		CanBeRightAfterToken bool
	}

	// (tokenRecognizerPattern) Tokens' meaning: VariableName, ID (function ?), '==' '=' ':='
	patternTokens:= []PatternToken{
		/*
		{
			Value: `(?:[.$]\w+)+`,
			ID: VARIABLE,
		},
		*/
		{
			// Value: `"[^"]+"`,
			Value: `"(?:[^"\n\\]|\\.)+"`,
			ID: STRING,
		},
		{
			Value: `[$]\w+(?:[.]\w+)*`,
			ID: NUMBER,
		},
		{
			Value: `[$]\w+(?:[.][a-zA-Z_]\w*)*`,
			ID: DOLLAR_VARIABLE,
		},
		{
			// TODO: Check that this new code is correct
			// Value: `[.]\w+(?:[.]\w+)*`,
			Value: `[.][a-zA-Z_]\w*(?:[.][a-zA-Z_]\w*)*`,
			ID: DOT_VARIABLE,
		},
		{
			Value: `\w+`,
			ID: FUNCTION,
		},
		/*
		{
			Value: `\w+`,
			ID: IDENTIFIER,
		},
		*/
		{
			Value: "==",
			ID: EQUAL_COMPARISON,
			CanBeRightAfterToken: true,
		},
		{
			Value: "=",
			ID: ASSIGNEMENT,
			CanBeRightAfterToken: true,
		},
		{
			Value: ":=",
			ID: ASSIGNEMENT_DEFINITION,
			CanBeRightAfterToken: true,
		},
		{
			Value: "[|]",
			ID: PIPE,
			CanBeRightAfterToken: true,
		},
	}

	var reg *regexp.Regexp
	var loc []int
	var errs []LexerError = nil

	var isCurrentTokenSeparatedFromPrevious bool = true
	var found bool

	var lengthDataStart int = -1
	var currentLocalLineNumber, currentLocalColumnNumber int

	currentLocalLineNumber = initialPosition.Start.Line
	currentLocalColumnNumber = initialPosition.Start.Character

	for len(data) > 0 && lengthDataStart != len(data) {
		lengthDataStart = len(data)
		found = false

		// Ignore White Space
		loc = regIgnore.FindIndex(data)
		if loc != nil && loc[0] == 0 {
			content := data[loc[0]:loc[1]]

			position := convertSingleIndexToTextEditorPosition(content, loc[1])
			if position.Line != 0 {
				currentLocalColumnNumber = 0
			}

			currentLocalLineNumber += position.Line
			currentLocalColumnNumber += position.Character

			isCurrentTokenSeparatedFromPrevious = true
			data = data[loc[1]:]
		}

		// Match a pattern to a token
		for _, pattern := range patternTokens {
			reg = regexp.MustCompile(pattern.Value)
			loc = reg.FindIndex(data)

			isCurrentTokenSeparatedFromPrevious = 
				isCurrentTokenSeparatedFromPrevious || pattern.CanBeRightAfterToken

			if loc != nil && loc[0] == 0 {
				if ! isCurrentTokenSeparatedFromPrevious {
					break
				}

				/*
				line := currentLocalLineNumber + initialPosition.Start.Line
				column := currentLocalColumnNumber

				if currentLocalLineNumber == 0 {
					column += initialPosition.Start.Character
				}

				pos := Range{
					Start: Position{Line: line, Character: column},
					End: Position{Line: line, Character: column + loc[1]},
				}
				*/

				// line := currentLocalLineNumber + initialPosition.Start.Line
				// column := currentLocalColumnNumber

				// loc[0] -= 1
				pos := convertRangeIndexToTextEditorPosition(data, loc, currentLocalLineNumber, currentLocalColumnNumber)

				currentLocalColumnNumber += loc[1]

				token := Token {
					Value: data[0:loc[1]],
					ID: pattern.ID,
					Range: pos,
				}
				tokens = append(tokens, token)

				isCurrentTokenSeparatedFromPrevious = false
				found = true
				data = data[loc[1]:]

				break
			}
		}

		// If no matching token found, add to error list
		if !found && len(data) > 0 {
			loc = regIgnore.FindIndex(data)

			line := currentLocalLineNumber + initialPosition.Start.Line
			column := currentLocalColumnNumber

			if currentLocalLineNumber == 0 {
				column += initialPosition.Start.Character
			}

			var err error
			if isCurrentTokenSeparatedFromPrevious {
				err = errors.New("character(s) not recognized")
			} else {
				err = errors.New("character(s) not recognized, perhaps you should properly separate the word")
			}

			var text []byte
			var pos Range

			if loc == nil {
				pos = Range{
					Start: Position{Line: line, Character: column},
					End: initialPosition.End,
				}

				text = data
				data = nil
			} else {
				pos = Range{
					Start: Position{Line: line, Character: column},
					End: Position{Line: line, Character: column + loc[1]},
				}

				text = data[:loc[0]]
				data = data[loc[0]:]
			}

			kindError := NOT_FOUND

			if bytes.Compare(text, []byte("{{")) == 0 || bytes.Compare(text, []byte("}}")) == 0 {
				err = errors.New("Missing matching template delimitator pair")
				kindError = UNEXPECTED
			}

			token := Token {
				ID: kindError,
				Value: text,
				Range: pos,
			}

			lexErr := LexerError{
				Err: err,
				Range: token.Range,
				Token: &token,
			}

			errs = append(errs, lexErr)
			tokens = append(tokens, token)
		}
	}

	if len(data) > 0 {
		token := Token {
			Value: data,
			ID: UNEXPECTED,
		}

		lexErr := LexerError{
			Err: errors.New("unexpected character(s)"),
			Token: &token,
			Range: initialPosition,
		}

		errs = append(errs, lexErr)
		tokens = append(tokens, token)

		data = nil
	}

	return tokens, errs
}


