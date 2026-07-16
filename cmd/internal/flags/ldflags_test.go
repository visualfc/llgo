/*
 * Copyright (c) 2026 The XGo Authors (xgo.dev). All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package flags

import (
	"reflect"
	"testing"

	"github.com/goplus/llgo/internal/build"
)

func TestParseGoLinkFlagsOrder(t *testing.T) {
	opts, err := parseGoLinkFlags([]string{
		"-tags=integration",
		"-ldflags=-s=false -w=true",
		"-ldflags=-s -w=false",
	})
	if err != nil {
		t.Fatal(err)
	}
	if !opts.present || !opts.options.OmitSymbolTable {
		t.Fatalf("options = %+v, want present OmitSymbolTable", opts)
	}
	if opts.options.DWARF != build.DWARFPreserve {
		t.Fatalf("DWARF = %v, want enabled", opts.options.DWARF)
	}
	if opts.options.EffectiveOmitDWARF() {
		t.Fatal("EffectiveOmitDWARF() = true, want false")
	}
}

func TestParseGoLinkFlagsLastArgumentListWins(t *testing.T) {
	opts, err := parseGoLinkFlags([]string{
		"-ldflags=-s",
		"-ldflags=-w=false",
	})
	if err != nil {
		t.Fatal(err)
	}
	if opts.options.OmitSymbolTable {
		t.Fatal("OmitSymbolTable = true, want the final -ldflags list to replace -s")
	}
	if opts.options.DWARF != build.DWARFPreserve || opts.options.EffectiveOmitDWARF() {
		t.Fatalf("options = %+v, want explicitly enabled DWARF", opts.options)
	}
}

func TestParseGoLinkFlagsExplicitFalse(t *testing.T) {
	opts, err := parseGoLinkFlags([]string{"-ldflags=-s=1 -s=f -w=TRUE -w=0"})
	if err != nil {
		t.Fatal(err)
	}
	if opts.options.OmitSymbolTable || opts.options.DWARF != build.DWARFPreserve || opts.options.EffectiveOmitDWARF() {
		t.Fatalf("options = %+v, want no symbol stripping and enabled DWARF", opts.options)
	}
}

func TestParseGoLinkFlagsDoubleDash(t *testing.T) {
	opts, err := parseGoLinkFlags([]string{"-ldflags=--s --w=false"})
	if err != nil {
		t.Fatal(err)
	}
	if !opts.options.OmitSymbolTable || opts.options.EffectiveOmitDWARF() {
		t.Fatalf("options = %+v, want --s with explicit --w=false", opts.options)
	}
}

func TestParseGoLinkFlagsSWithExplicitWFalse(t *testing.T) {
	for _, value := range []string{"-s -w=0", "-w=0 -s"} {
		opts, err := parseGoLinkFlags([]string{"-ldflags=" + value})
		if err != nil {
			t.Fatal(err)
		}
		if !opts.options.OmitSymbolTable || opts.options.EffectiveOmitDWARF() {
			t.Fatalf("%q: options = %+v, want -s with enabled DWARF", value, opts.options)
		}
	}

	opts, err := parseGoLinkFlags([]string{"-ldflags=-s"})
	if err != nil {
		t.Fatal(err)
	}
	if !opts.options.EffectiveOmitDWARF() {
		t.Fatal("EffectiveOmitDWARF() = false, want -s to imply -w")
	}
}

func TestParseGoLinkFlagsQuotesPatternAndIgnored(t *testing.T) {
	opts, err := parseGoLinkFlags([]string{
		`-ldflags=all=-s '-w=false' "-extldflags=-static -pthread" '-w=t' -unknown`,
	})
	if err != nil {
		t.Fatal(err)
	}
	if !opts.options.OmitSymbolTable || !opts.options.EffectiveOmitDWARF() {
		t.Fatalf("options = %+v, want -s and effective -w", opts.options)
	}
	wantIgnored := []string{"-extldflags=-static -pthread", "-unknown"}
	if !reflect.DeepEqual(opts.ignored, wantIgnored) {
		t.Fatalf("ignored = %q, want %q", opts.ignored, wantIgnored)
	}
}

func TestParseGoLinkFlagsBoolSpellings(t *testing.T) {
	trueValues := []string{"1", "t", "T", "true", "TRUE", "True"}
	for _, value := range trueValues {
		t.Run("true_"+value, func(t *testing.T) {
			opts, err := parseGoLinkFlags([]string{"-ldflags=-w=" + value})
			if err != nil {
				t.Fatal(err)
			}
			if !opts.options.EffectiveOmitDWARF() {
				t.Fatalf("-w=%s parsed as false", value)
			}
		})
	}

	falseValues := []string{"0", "f", "F", "false", "FALSE", "False"}
	for _, value := range falseValues {
		t.Run("false_"+value, func(t *testing.T) {
			opts, err := parseGoLinkFlags([]string{"-ldflags=-w=" + value})
			if err != nil {
				t.Fatal(err)
			}
			if opts.options.EffectiveOmitDWARF() {
				t.Fatalf("-w=%s parsed as true", value)
			}
		})
	}
}

func TestParseGoLinkFlagsErrors(t *testing.T) {
	tests := []struct {
		name  string
		flags []string
	}{
		{name: "missing ldflags value", flags: []string{"-ldflags"}},
		{name: "missing pattern separator", flags: []string{"-ldflags=all"}},
		{name: "unsupported package pattern", flags: []string{"-ldflags=example.com/other=-s"}},
		{name: "quoted first parameter", flags: []string{`-ldflags='-s' -w`}},
		{name: "missing s boolean", flags: []string{"-ldflags=-s="}},
		{name: "missing w boolean", flags: []string{"-ldflags=-w="}},
		{name: "invalid s boolean", flags: []string{"-ldflags=-s=maybe"}},
		{name: "invalid w boolean", flags: []string{"-ldflags=-w=yes"}},
		{name: "unterminated quote", flags: []string{`-ldflags=-s '-w=false`}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if _, err := parseGoLinkFlags(tt.flags); err == nil {
				t.Fatalf("parseGoLinkFlags(%q) succeeded, want error", tt.flags)
			}
		})
	}
}
