package main

import (
	"strings"
	"text/template"
)

func testTemplate() {
	const (
		master  = `Names:{{block "list" .}}{{"\n"}}{{range .}}{{println "-" .}}{{end}}{{end}}`
		overlay = `{{define "list"}} {{join . ", "}}{{end}} `
	)
	funcs := template.FuncMap{"join": strings.Join}
	guardians := []string{"Gamora", "Groot", "Nebula", "Rocket", "Star-Lord"}
	masterTemplate := template.Must(template.New("master").Funcs(funcs).Parse(master))
	overlayTemplate := template.Must(template.Must(masterTemplate.Clone()).Parse(overlay))

	var masterOutput, overlayOutput strings.Builder
	if err := masterTemplate.Execute(&masterOutput, guardians); err != nil ||
		!strings.Contains(masterOutput.String(), "- Gamora") || !strings.Contains(masterOutput.String(), "- Star-Lord") {
		panic("text/template block")
	}
	if err := overlayTemplate.Execute(&overlayOutput, guardians); err != nil ||
		!strings.Contains(overlayOutput.String(), "Gamora, Groot, Nebula, Rocket, Star-Lord") {
		panic("text/template clone")
	}
}
