package main

import (
	"fmt"
	"log"
	"os"

	"github.com/yayolande/gota"
)

func ddd(files ...int) {
	fmt.Println(files)
}

func main() {
	// 0. Init
	func () {
		file, err := os.Create("log_main.txt")
		if err != nil {
			fmt.Println("error while creating log file; ", err.Error())
			return
		}

		log.SetOutput(file)
		log.SetFlags(log.LstdFlags | log.Llongfile)
		log.SetPrefix("[LOG] ")
	}()

	// 1. Open all files under root directory
	rootDir := "."
	fileExtension := ".html"

	filesContentInWorkspace := gota.OpenProjectFiles(rootDir, fileExtension)
	parsedFilesInWorkspace, parseErrors := gota.ParseFilesInWorkspace(filesContentInWorkspace)
	_ = parsedFilesInWorkspace
	_ = parseErrors

	// gota.DefinitionAnalisisWithinWorkspace(parsedFilesInWorkspace)

	for key, val := range parsedFilesInWorkspace {
		_ = key
		_ = val
		// fmt.Println("key = ", key)
		// fmt.Printf("%q\n\n\n\n", val)
		gota.Print(val)
	}

	// fmt.Println("-------------")
	// fmt.Println("len (parseErrors) = ", len(parseErrors))

	// TODO: rename 'PrettyFormater()' to 'ArrayToString()'
	// str := types.PrettyFormater(parseErrors)
	// fmt.Println(str)
}

