//go:build ignore

package main

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/parser"
	"go/token"
	"go/types"
	"log"
	"os"
	"strconv"
	"strings"
	"text/template"
)

func main() {
	// TODO: Change log output from 'Stdout' to 'file'
	setupLogger()

	// TODO: Make the tool works with proxy structure
	// A proxy struct is a struct that contains all the property for which to produce
	// the getter/setter, and which is embeded inside other struct for which the 
	// getter/setter method will be created on

	fileName := "ast.go"
	interfaceName := "AstNode"
	ifaceNameSuffix := ""
	structNameSuffix := "Node"
	proxyName := "BaseNode"

	_ = interfaceName
	_ = proxyName
	fileName = "./_examples/struct_sample.go"
	interfaceName = "FacePal"

	f := token.NewFileSet()
	file, err := parser.ParseFile(f, fileName, nil, parser.SkipObjectResolution | parser.ParseComments)
	if err != nil {
		panic("fatal error while parsing '" + fileName + "', " + err.Error())
	}

	// 0. Collect all interfaces and structures present in the file ast
	ifaces := extractInterfacesFromAst(ifaceNameSuffix, file.Decls)
	structs := extractStructuresFromAst(structNameSuffix, file.Decls)

	for _, iface := range ifaces {
		for _, method := range iface.Methods {
			methodName := method.Name

			for _, object := range structs {
				// 1. Check that the method we want to generate doesn't exist on source file
				walker := Walker{}
				walker.wantedStructName = object.Name
				walker.wantedMethodName = methodName

				ast.Inspect(file, walker.verifyMethodExistance)

				if walker.found { continue }

				// 2. Generate the method string
				methodCode := stringifyMethodImplementation(method, object)
				if len(methodCode) == 0 {
					continue
				}

				// 3. convert method string to ast
				src := "package main \n" + methodCode
				fileAndMethodAst, err := parser.ParseFile(token.NewFileSet(), "", src, parser.SkipObjectResolution)
				if err != nil {
					log.Println("error parsing expression, ", err.Error())
					continue 
				}

				// Insert method declaration into file ast tree
				if len(fileAndMethodAst.Decls) == 0 {
					log.Println("unexpected error, method not defined")
					continue 
				}
				methodAst := fileAndMethodAst.Decls[0]

				// 4. inject method to code source (ast)
				insertMethodIntoFileAst(object.Name, methodAst, file, f)
			}
		}
	}

	// Show result of the hard work
	code := new(bytes.Buffer)
	err = format.Node(code, f, file)
	if err != nil {
		log.Println("error while converting back ast to code source, ", err.Error())
		return
	}

	fmt.Println("// code completed")
	fmt.Println(code)
	fmt.Println()

	//TODO: Save the newly obtained source code to file

	return
}

func insertMethodIntoFileAst(structName string, methodAst ast.Decl, file *ast.File, f *token.FileSet){
	for indexDeclaration, decl := range file.Decls {
		genDecl, ok := decl.(*ast.GenDecl)
		if !ok {
			continue
		}

		if genDecl.Tok != token.TYPE {
			continue
		}

		for _, spec := range genDecl.Specs {
			typeSepc, ok := spec.(*ast.TypeSpec)
			if !ok {
				continue
			}

			if strings.Compare(typeSepc.Name.Name, structName) != 0 {
				continue
			}

			index := indexDeclaration + 1
			finalDecls := make([]ast.Decl, 0, len(file.Decls))
			finalDecls = append(finalDecls, file.Decls[:index]...)
			finalDecls = append(finalDecls, methodAst)
			finalDecls = append(finalDecls, file.Decls[index:]...)
			file.Decls = finalDecls

			str := new(bytes.Buffer)
			format.Node(str, f, file)
			return
		}
	}
}

type Walker struct {
	wantedStructName	string
	wantedMethodName	string
	found	bool
}

func (w *Walker) Reset() {
	w.found = false
	w.wantedMethodName = ""
	w.wantedStructName = ""
}

func (w *Walker) verifyMethodExistance(node ast.Node) bool {

	switch n := node.(type) {
	case *ast.FuncDecl:
		if n.Recv == nil {
			return false
		}

		if w.wantedMethodName == "" || w.wantedStructName == "" {
			log.Println("warning, empty string for 'wantedStructName' or 'wantedMethodName'.")
		}

		var foundMethodName, foundStructName bool = false, false

		for _, field := range n.Recv.List {
			typ := types.ExprString(field.Type)
			
			if strings.HasSuffix(w.wantedStructName, typ) ||
				strings.HasSuffix(typ, w.wantedStructName) {
				foundStructName = true
				break
			}
		}

		if strings.Compare(w.wantedMethodName, n.Name.Name) == 0 {
			foundMethodName = true
		}

		if foundStructName && foundMethodName {
			w.found = true
		}

		return false
	case *ast.File, ast.Decl, *ast.GenDecl, ast.Spec, *ast.Field:
		return true
	}

	return false
}

func stringifyMethodImplementation (method FunctionStructure, object StructStructure) string {
	methodName := strings.ToLower(method.Name)
		
	for _, field := range object.Fields {
		fieldName := strings.ToLower(field.SymbolName)

		if ! strings.HasSuffix(methodName, fieldName) {
			continue
		}

		src := "UNEXPECTED PREFIX !"
		/*
			src = "func (g {{.structName}}) {{.methodName}} " + 
			"({{range .args}} {{.SymbolName}} {{.Type}}, {{end}}) " + 
			" ({{range .returns}} {{.SymbolName}} {{.Type}}, {{end}}) {" + 
			"return g.{{.property}}" + 
			"}"
		*/

		data := map[string]any{
			"structInitial": strings.ToLower(object.Name)[:1],
			"structName": object.Name,
			"methodName": method.Name,
			"args": method.Parameters,
			"returns": method.Returns,
			"property": field.SymbolName,
		}

		if strings.HasPrefix(methodName, "get") {
			src = "func ({{.structInitial}} {{.structName}}) {{.methodName}} " + 
			" () " + 
			" ({{range .returns}} {{.SymbolName}} {{.Type}}, {{end}}) {" + 
			" return {{.structInitial}}.{{.property}}" + 
			"}"

			if len(method.Parameters) > 0 {
				log.Println("error, getter only accept empty parameter",
					object.Name + "." + field.SymbolName)
				continue
			}

			if len(method.Returns) > 1 {
				log.Println("error, getter connot have more than one return value",
					object.Name + "." + field.SymbolName)
				continue
			} else if len(method.Returns) == 0 {
				log.Println("error, getter cannot have empty return value",
					object.Name + "." + field.SymbolName)
				continue
			}

			if method.Returns[0].Type != field.Type {
				log.Println("error, getter return type should match the type of the structure field --", 
					object.Name + "." + field.SymbolName)
				continue
			}

		} else if strings.HasPrefix(methodName, "set") {
			src = "func ({{.structInitial}} *{{.structName}}) {{.methodName}} " + 
			"({{range .args}} {{.SymbolName}} {{.Type}}, {{end}}) " + 
			" {" + 
			" {{.structInitial}}.{{.property}} = {{ .setterValue }}" + 
			"}"

			if len(method.Parameters) == 0 {
				log.Println("error, setter method cannot have empty parameter list",
					object.Name + "." + field.SymbolName)
				continue
			} else if len(method.Parameters) > 1 {
				log.Println("error, setter method cannot have more than 1 parameter",
					object.Name + "." + field.SymbolName)
				continue
			}

			if method.Parameters[0].Type != field.Type {
				log.Println("error, type mismatch for parameter and return value",
					object.Name + "." + field.SymbolName)
				continue
			}

			data["setterValue"] = method.Parameters[0].SymbolName
		}

		tmpl, err := template.New("method_declaration").Parse(src)
		if err != nil {
			log.Println("error while parsing template to render 'method_declaration'", 
				err.Error())
			continue
		}

		str := new(bytes.Buffer)
		tmpl.Execute(str, data)

		formatedCode, err := format.Source(str.Bytes())
		if err != nil {
			log.Println("error while formating generated go code, ", err.Error())
			log.Println(str.String())
			continue
		}

		return string(formatedCode)
	}

	return ""
}

type StructStructure struct {
	Name	string
	Fields	[]Typing
}

func extractStructuresFromAst(structureNameSuffix string, decls []ast.Decl) []StructStructure {
	if len(structureNameSuffix) == 0 {
		log.Println("warning, structureNameSuffix is empty and might beget unexpected behavior")
	}

	var variable StructStructure
	var variables []StructStructure

	for _, decl := range decls {
		genDecl, ok := decl.(*ast.GenDecl)
		if !ok {
			continue
		}

		if genDecl.Tok != token.TYPE {
			continue
		}

		for _, spec := range genDecl.Specs {
			typeSpec, ok := spec.(*ast.TypeSpec)
			if !ok {
				continue
			}

			structName := typeSpec.Name.Name
			if !strings.HasSuffix(structName, structureNameSuffix) {
				continue
			}

			typeFields, ok := typeSpec.Type.(*ast.StructType)
			if !ok {
				continue
			}

			variable = StructStructure{
				Name: structName,
			}

			for _, list := range typeFields.Fields.List {
				typeString := types.ExprString(list.Type)

				if len(list.Names) == 0 {
					typing := Typing{
						SymbolName: typeString,
						Type: typeString,
					}

					variable.Fields = append(variable.Fields, typing)
					continue
				}

				for _, ident := range list.Names {
					typing := Typing{
						SymbolName: ident.Name,
						Type: typeString,
					}

					variable.Fields = append(variable.Fields, typing)
				}
			}

			variables = append(variables, variable)
		}
	}

	return variables
}

type Typing struct {
	SymbolName	string
	Type	string
}

type FunctionStructure struct {
	Name	string
	Parameters	[]Typing
	Returns	[]Typing
}

type InterfaceStructure struct {
	Name	string
	Methods	[]FunctionStructure
}

func extractInterfacesFromAst(ifaceNameSuffix string, decls []ast.Decl) []InterfaceStructure {
	if len(ifaceNameSuffix) == 0 {
		log.Println("warning, interfaceNameSuffix is empty. It might beget unexpected behavior")
	}

	var interfaces []InterfaceStructure

	for _, decl := range decls {
		genDecl, ok := decl.(*ast.GenDecl)
		if !ok {
			continue
		}

		if genDecl.Tok != token.TYPE {
			continue
		}

		for _, spec := range genDecl.Specs {
			typeSpec, ok := spec.(*ast.TypeSpec)
			if !ok {
				continue
			}

			if ! strings.HasSuffix(typeSpec.Name.Name, ifaceNameSuffix) {
				continue
			}

			infaceType, ok := typeSpec.Type.(*ast.InterfaceType)
			if !ok {
				continue
			}

			inface := InterfaceStructure{
				Name: typeSpec.Name.Name,
				Methods: []FunctionStructure{},
			}

			for _, field := range infaceType.Methods.List {
				funcType, ok := field.Type.(*ast.FuncType)
				if !ok {
					continue
				}

				infaceFunc := FunctionStructure{
					Name: field.Names[0].Name,
				}

				// Parameters
				for idx, param := range funcType.Params.List {
					typeName := types.ExprString(param.Type)

					for _, name := range param.Names {
						typing := Typing {
							SymbolName: name.Name,
							Type: typeName,
						}

						infaceFunc.Parameters = append(infaceFunc.Parameters, typing)
					}

					if len(param.Names) == 0 {
						typing := Typing{
							SymbolName: "val" + strconv.Itoa(idx),
							Type: typeName,
						}

						infaceFunc.Parameters = append(infaceFunc.Parameters, typing)
					}
				}

				// Returns
				if funcType.Results != nil {
					for _, ret := range funcType.Results.List {
						typeName := types.ExprString(ret.Type)

						typing := Typing {
							Type: typeName,
						}

						infaceFunc.Returns = append(infaceFunc.Returns, typing)
					}
				}

				inface.Methods = append(inface.Methods, infaceFunc)
				continue
			}

			interfaces = append(interfaces, inface)
		}
	}

	return interfaces
}

func setupLogger() {
	log.SetOutput(os.Stdout)
	log.SetPrefix("[LOG] ")
	log.SetFlags(log.LstdFlags | log.Lmsgprefix)
}
