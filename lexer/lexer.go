package lexer

import (
	"bytes"
	"errors"
	"log"
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
	DOT_VARIABLE	Kind = iota
	DOLLAR_VARIABLE
	KEYWORD
	FUNCTION
	IDENTIFIER
	ASSIGNEMENT
	ASSIGNEMENT_DEFINITION
	STRING
	NUMBER
	EQUAL_COMPARISON
	PIPE
	COMMA
	LEFT_PAREN
	RIGTH_PAREN
	COMMENT
	SPACE_EATER
	EOL	// End Of Line
	EOF
	NOT_FOUND
	UNEXPECTED
)

func Tokenize(content []byte) ([]Token, []Token, []LexerError) {
	if len(content) == 0 {
		return nil, nil, nil
	}

	templateCodes, templatePositions := extractTemplateCode(content)

	if templateCodes == nil {
		return nil, nil, nil
	}

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
	if len(content) == 0 {
		return nil, nil
	}

	var ORIGINAL_CONTENT = content
	var CLONED_CONTENT = bytes.Clone(content)
	// content = bytes.Clone(content)
	content = CLONED_CONTENT

	var templateCode [][]byte
	var templatePositions []Range
	var insideTemplate []byte

	captureLonelyTemplateDelimitator := regexp.MustCompile("{{|}}")
	// captureTemplateStatementOnly := regexp.MustCompile("(?:{{(?:[^{}]|[\n\r\t])*?}})")
	captureTemplateStatementOnly := regexp.MustCompile("(?:{{(?:[^{}]|[^{}]{|[^{}]}|[^{}]{}|[^{}]}{|[\n\r\t])*?}})")

	// TODO: line shouldn't start at '0' but '1' instead
	currentLine := 0
	currentColumn := 0

	var loc, loneLoc []int
	var templatePosition Range

	for {
		loneLoc = captureLonelyTemplateDelimitator.FindIndex(content)
		loc = captureTemplateStatementOnly.FindIndex(content)

		if loc == nil {
			// Do some checking before break out of the loop
			// TODO: this has some nasty issue. What happend if there is still one or more remaining '{{' left ?
			// Answer: those lone '{{' are not registered, which will biase the lexical analysis
			// TODO: Remove comment above as this issue as been solved
			for {
				if loneLoc != nil {
					templatePosition = convertRangeIndexToTextEditorPosition(content, loneLoc, currentLine, currentColumn)

					templatePositions = append(templatePositions, templatePosition)
					templateCode = append(templateCode, content[loneLoc[0]:loneLoc[1]])

					currentLine = templatePosition.End.Line
					currentColumn = templatePosition.End.Character + 1
					content = content[loneLoc[1]:]

					loneLoc = captureLonelyTemplateDelimitator.FindIndex(content)
					continue
				}

				break
			}

			break
		}

		// A lone delimitator has been found in the wild, do something
		// TODO: this has some nasty issue. What happend if there is still one or more remaining '{{' left ?
		// Answer: those lone '{{' are not registered, which will biase the lexical analysis
		// TODO: Remove comment above as this issue as been solved
		for {
			if loneLoc[0] < loc[0] {
				templatePosition = convertRangeIndexToTextEditorPosition(content, loneLoc, currentLine, currentColumn)

				templatePositions = append(templatePositions, templatePosition)
				templateCode = append(templateCode, content[loneLoc[0]:loneLoc[1]])

				currentLine = templatePosition.End.Line
				currentColumn = templatePosition.End.Character + 1
				content = content[loneLoc[1]:]

				loneLoc = captureLonelyTemplateDelimitator.FindIndex(content)
				loc = captureTemplateStatementOnly.FindIndex(content)
				continue
			}

			break
		}

		templatePosition = convertRangeIndexToTextEditorPosition(content, loc, currentLine, currentColumn)

		currentLine = templatePosition.End.Line
		currentColumn = templatePosition.End.Character + 1

		// Trim '{{' and '}}'
		insideTemplate = content[loc[0] + 2 : loc[1] - 2]

		templatePosition.Start.Character += 2
		templatePosition.End.Character -= 1

		templatePositions = append(templatePositions, templatePosition)
		templateCode = append(templateCode, insideTemplate)

		content = content[loc[1]:]
	}

	// {{ {{ {{ {{ {{ hello }} }} }}
	//	WARNING: This operation is too expensive ! Perhaps it should be changed
	// {{ { hell cat }}
	/*
		jkdf "{{ |ddjfkdfj }}"
{{dd}}
"(?:{{(?:[^{}]|[\n\r\t])*?}})"
(?:{{(?:(?:[^{}]|[\n\r\t])*?|(?:[^{}]|[^{}]{|[\n\r\t])*?)}})
	// {{ { hell cat }}
	// {{ {{ {{ {{ {{ hello }} }} }}
steveen son {{ { necro { nello } beach }{ }}
	*/
	if bytes.Compare(ORIGINAL_CONTENT, CLONED_CONTENT) != 0 {
		log.Printf("ORIGINAL_CONTENT = \n%q\n===================\ncontent = \n%q\n=============", ORIGINAL_CONTENT, CLONED_CONTENT)
		panic("content of the file has changed during lexical analysis (extracting template)." +
			"In a perfect world, it shouldn't change")
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
	if len(data) == 0 {
		return nil, nil
	}

	tokenizer := createTokenizer()
	data, isCommentAllowed, isTrimmed, err := handleExternalWhiteSpaceTrimmer(data, initialPosition) 

	if err != nil {
		tokenizer.Errs = append(tokenizer.Errs, *err)
	}

	if isTrimmed[0] {
		initialPosition.Start.Character ++
	}

	patternTokens := tokenizer.PatternToRecognize
	ignorePattern := tokenizer.PatternToIgnore

	regIgnore := regexp.MustCompile(ignorePattern)

	var reg *regexp.Regexp
	var loc []int

	var isCurrentTokenSeparatedFromPrevious bool = true
	var isPreviousTokenAcceptBindingToken bool = true
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
			isPreviousTokenAcceptBindingToken = true
			data = data[loc[1]:]
		}

		// Match a pattern to a token
		for _, pattern := range patternTokens {
			reg = regexp.MustCompile(pattern.Value)
			loc = reg.FindIndex(data)

			isCurrentTokenSeparatedFromPrevious = isCurrentTokenSeparatedFromPrevious || 
				pattern.CanBeRightAfterToken || isPreviousTokenAcceptBindingToken

			if loc != nil && loc[0] == 0 {
				if pattern.ID == COMMENT && ! isCommentAllowed {
					break
				} else if ! isCurrentTokenSeparatedFromPrevious {
					break
				}

				pos := convertRangeIndexToTextEditorPosition(data, loc, currentLocalLineNumber, currentLocalColumnNumber)
				currentLocalColumnNumber += loc[1]

				tokenizer.appendToken(pattern.ID, pos, data[0:loc[1]])

				isPreviousTokenAcceptBindingToken = pattern.CanBeRightAfterToken
				isCurrentTokenSeparatedFromPrevious = false
				found = true
				data = data[loc[1]:]

				break
			}
		}

		// If no matching token found, add to error list
		if !found && len(data) > 0 {
			loc = regIgnore.FindIndex(data)

			if loc == nil {
				loc = []int{ 0, len(data) }
			} else {
				loc = []int{ 0, loc[0] }
			}

			pos := convertRangeIndexToTextEditorPosition(data, loc, currentLocalLineNumber, currentLocalColumnNumber)
			currentLocalColumnNumber += loc[1]

			var err error
			if isCurrentTokenSeparatedFromPrevious {
				err = errors.New("character(s) not recognized")
			} else {
				err = errors.New("character(s) not recognized, perhaps you should properly separate the word")
			}

			kindError := NOT_FOUND
			if bytes.Compare(data[:loc[1]], []byte("{{")) == 0 || bytes.Compare(data[:loc[1]], []byte("}}")) == 0 {
				err = errors.New("Missing matching template delimitator pair")
				kindError = UNEXPECTED
			}

			tokenizer.appendToken(kindError, pos, data[:loc[1]])
			token := tokenizer.getLastInsertedToken()
			tokenizer.appendError(err, token)

			data = data[loc[1]:]
		}
	}

	if len(data) > 0 {
		tokenizer.appendToken(UNEXPECTED, initialPosition, data)
		token := tokenizer.getLastInsertedToken()
		tokenizer.appendError(errors.New("unexpected character(s)"), token)

		data = nil
	}

	if len(tokenizer.Tokens) == 0 {
		tokenizer.appendError(errors.New("empty tepmlate not allowed"), &Token{ID: NOT_FOUND, Range: initialPosition})
	}

	return tokenizer.Tokens, tokenizer.Errs
}

func handleExternalWhiteSpaceTrimmer(data []byte, pos Range) ([]byte, bool, [2]bool, *LexerError) {
	isLeftCommentAllowed, isRigthCommentAllowed := false, false
	isRigthTrimmed, isLeftTrimmed := false, false

	var err *LexerError = nil

	if len(data) < 2 {
		isCommentAllowed := isLeftCommentAllowed && isRigthCommentAllowed
		isTrimmed := [2]bool{ isLeftTrimmed, isRigthTrimmed }

		return data, isCommentAllowed, isTrimmed, err
	}

	//
	// Simple comment detection : {{/* ... */}}
	//
	if bytes.Compare(data[:1], []byte("/")) == 0 {
		isLeftCommentAllowed = true
	}

	lastElement := len(data) - 1
	if bytes.Compare(data[lastElement:], []byte("/")) == 0 {
		isRigthCommentAllowed = true
	}

	//
	// Advanced Comment detection : {{- /* ... */ -}}
	//
	if bytes.Compare(data[lastElement:], []byte("-")) == 0 {
		isRigthTrimmed = true
		data = data[:lastElement]	// Trim rigth '-'

		lastElement = len(data) - 1
		isOkay := lastElement > 0

		if isOkay && bytes.ContainsAny(data[lastElement:], " \r\n\t\f\v") {
			isOkay = lastElement > 1
			if isOkay && bytes.Compare(data[lastElement - 1:lastElement], []byte("/")) == 0 {
				isRigthCommentAllowed = true
			}
		} else {
			pos.Start.Character = pos.Start.Character - 1

			err = &LexerError{
				Err: errors.New("'-' left operator cannot be next to non-white-space"),
				Range: pos,
				Token: &Token{Value: []byte(".-"), ID: SPACE_EATER, Range: pos},
			}
		}
	}

	if bytes.Compare(data[:1], []byte("-")) == 0 {
		isLeftTrimmed = true
		data = data[1:]	// Trim left '-'

		isOkay := len(data) > 0

		if isOkay && bytes.ContainsAny(data[:1], " \r\n\t\f\v") {
			isOkay = len(data) > 1
			if isOkay && bytes.Compare(data[1:2], []byte("/")) == 0 {
				isLeftCommentAllowed = true
			}
		} else {
			pos.End.Character = pos.Start.Character + 2

			err = &LexerError{
				Err: errors.New("'-' rigth operator cannot be next to non-white-space"),
				Range: pos,
				Token: &Token{Value: []byte("-."), ID: SPACE_EATER, Range: pos},
			}
		}
	}

	isCommentAllowed := isLeftCommentAllowed && isRigthCommentAllowed
	isTrimmed := [2]bool{ isLeftTrimmed, isRigthTrimmed }

	return data, isCommentAllowed, isTrimmed, err
}

type PatternToken struct {
	Value	string
	ID	Kind
	CanBeRightAfterToken bool
}

type Tokenizer struct {
	PatternToIgnore	string
	PatternToRecognize	[]PatternToken
	Tokens	[]Token
	Errs	[]LexerError
}

func (t *Tokenizer) appendToken(id Kind, pos Range, val []byte) {
	to := Token {
		ID: id,
		Range: pos,
		Value: val,
	}

	t.Tokens = append(t.Tokens, to)
}

func (t Tokenizer) getLastInsertedToken() *Token {
	if len(t.Tokens) == 0 {
		return nil
	}

	return &t.Tokens[len(t.Tokens) - 1]
}

func (t *Tokenizer) appendError(err error, token *Token) {
	if err == nil {
		return
	}

	lexErr := LexerError{
		Err: err,
		Token: token,
		Range: token.Range,
	}

	t.Errs = append(t.Errs, lexErr)
}



func createTokenizer() *Tokenizer {
	// (tokenRecognizerPattern) Tokens' meaning: VariableName, ID (function ?), '==' '=' ':='
	tokenPatterns := []PatternToken{
		{
			Value: "if|else|end|range|define|template|block|with",
			ID: KEYWORD,
		},
		{
			// Value: `"[^"]+"`,
			Value: `"(?:[^"\n\\]|\\.)+"`,
			ID: STRING,
		},
		{
			Value: `\d*[.]\d+|\d+`,
			ID: NUMBER,
		},
		{
			Value: `[$]\w+(?:[.][a-zA-Z_]\w*)*|[$]`,
			ID: DOLLAR_VARIABLE,
		},
		{
			// TODO: Check that this new code is correct
			// Value: `[.]\w+(?:[.]\w+)*`,
			// Value: `[.][a-zA-Z_]?\w*(?:[.][a-zA-Z_]\w*)*`,
			Value: `(?:[.][a-zA-Z_]\w*)+|[.]`,
			ID: DOT_VARIABLE,
		},
		{
			Value: `\w+`,
			ID: FUNCTION,
		},
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
		{
			Value: `\(`,
			ID: LEFT_PAREN,
			CanBeRightAfterToken: true,
		},
		{
			Value: `\)`,
			ID: RIGTH_PAREN,
			CanBeRightAfterToken: true,
		},
		{
			Value: `\/\*.*?(?:\*\/)`,
			ID: COMMENT,
		},
		{
			Value: `,`,
			ID: COMMA,
		},
	}

	to := &Tokenizer{
		PatternToIgnore: string(`\s+`),
		PatternToRecognize: tokenPatterns ,
		Tokens: nil,
		Errs: nil,
	}

	return to
}

