package main

import (
	"os"
	"strings"
	"text/template"
)

func main() {
	const source = `Names: {{join . ", "}}{{"\n"}}`
	values := []string{"Gamora", "Groot", "Nebula", "Rocket", "Star-Lord"}
	t := template.Must(template.New("names").Funcs(template.FuncMap{
		"join": strings.Join,
	}).Parse(source))
	if err := t.Execute(os.Stdout, values); err != nil {
		panic(err)
	}
}
