package main

import (
	"fmt"
	"testing"
)

func TestComputeFileContentPosition (t *testing.T) {
	type BufferPartition struct {
		Content	[]byte
		Start	int
		End	int
	}
	data := []struct {
		input	BufferPartition
		expect	Range
	} {
		{
			input: BufferPartition{
				Content: []byte("\n\nH\n"),
				Start: 2,
				End: 2,
			},
			expect: Range{
				Start: Position{Line: 2, Character: 0},
				End: Position{Line:2, Character: 0},
			},
		},
		{
			input: BufferPartition{
				Content: []byte("\n\n\n"),
				Start: 3,
				End: 3,
			},
			expect: Range{
				Start: Position{Line: 3, Character: 0},
				End: Position{Line:3, Character: 0},
			},
		},
		{
			input: BufferPartition{
				Content: []byte("H\n\nH\n"),
				Start: 0,
				End: 3,
			},
			expect: Range{
				Start: Position{Line: 0, Character: 0},
				End: Position{Line:2, Character: 0},
			},
		},
		{
			input: BufferPartition{
				Content: []byte("aaaH\n\nH\n"),
				Start: 3,
				End: 6,
			},
			expect: Range{
				Start: Position{Line: 0, Character: 3},
				End: Position{Line:2, Character: 0},
			},
		},
		{
			input: BufferPartition{
				Content: []byte("H\n\naaaH\n"),
				Start: 0,
				End: 6,
			},
			expect: Range{
				Start: Position{Line: 0, Character: 0},
				End: Position{Line:2, Character: 3},
			},
		},
		{
			input: BufferPartition{
				Content: []byte("aaaH\n\ndddddH\n"),
				Start: 3,
				End: 11,
			},
			expect: Range{
				Start: Position{Line: 0, Character: 3},
				End: Position{Line:2, Character: 5},
			},
		},
		{
			input: BufferPartition{
				Content: []byte("aaa H\n\n  dddddH\n"),
				Start: 4,
				End: 14,
			},
			expect: Range{
				Start: Position{Line: 0, Character: 4},
				End: Position{Line:2, Character: 7},
			},
		},
		{
			input: BufferPartition{
				Content: []byte("\n\n\ndd"),
				Start: 5,
				End: 5,
			},
			expect: Range{
				Start: Position{Line: 3, Character: 2},
				End: Position{Line:3, Character: 2},
			},
		},
		{
			input: BufferPartition{
				Content: []byte("\n\n\nddd"),
				Start: 6,
				End: 6,
			},
			expect: Range{
				Start: Position{Line: 3, Character: 3},
				End: Position{Line:3, Character: 3},
			},
		},
	}

	for count, datium := range data {
		testName := fmt.Sprintf("Test %d", count)

		input := datium.input
		expected := datium.expect

		t.Run(testName, func(t *testing.T) {
			// got := computeFileContentPosition(input.Content, input.Start, input.End)
			start := convertToTextEditorPosition(input.Content, input.Start)
			end := convertToTextEditorPosition(input.Content, input.End)

			got := Range{ Start: start, End: end }

			if got != expected {
				t.Errorf("\n Expected : %#v\n But got: %#v\n", expected, got)
			}
		})
	}
}
