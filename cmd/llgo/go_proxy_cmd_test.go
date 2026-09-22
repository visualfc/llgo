//go:build !llgo

package main

import "testing"

func TestGoProxyCommands(t *testing.T) {
	fix := &Cmd_fix{App: new(App)}
	fix.Main("fix")
	fmtCmd := &Cmd_fmt{App: new(App)}
	fmtCmd.Main("fmt")
	generate := &Cmd_generate{App: new(App)}
	generate.Main("generate")
	get := &Cmd_get{App: new(App)}
	get.Main("get")
	vet := &Cmd_vet{App: new(App)}
	vet.Main("vet")
	work := &Cmd_work{App: new(App)}
	work.Main("work")

	commands := []struct {
		name     string
		use      string
		class    string
		disabled bool
		runnable bool
	}{
		{"fix", fix.Command.Command.Use, fix.Classfname(), fix.DisableFlagParsing, fix.Run != nil},
		{"fmt", fmtCmd.Command.Command.Use, fmtCmd.Classfname(), fmtCmd.DisableFlagParsing, fmtCmd.Run != nil},
		{"generate", generate.Command.Command.Use, generate.Classfname(), generate.DisableFlagParsing, generate.Run != nil},
		{"get", get.Command.Command.Use, get.Classfname(), get.DisableFlagParsing, get.Run != nil},
		{"vet", vet.Command.Command.Use, vet.Classfname(), vet.DisableFlagParsing, vet.Run != nil},
		{"work", work.Command.Command.Use, work.Classfname(), work.DisableFlagParsing, work.Run != nil},
	}
	wantUses := map[string]string{
		"fix":      "fix [-target name] [build flags] [packages]",
		"fmt":      "fmt [-n] [-x] [packages]",
		"generate": "generate [-target name] [build flags] [file.go... | packages]",
		"get":      "get [flags] [packages]",
		"vet":      "vet [-target name] [build flags] [packages]",
		"work":     "work <command> [arguments]",
	}
	for _, command := range commands {
		if command.use != wantUses[command.name] || command.class != command.name || !command.disabled || !command.runnable {
			t.Errorf("%s metadata = (%q, %q, flagOff=%v, runnable=%v)", command.name, command.use, command.class, command.disabled, command.runnable)
		}
	}
}
