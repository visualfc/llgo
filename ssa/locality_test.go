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

package ssa

import (
	"go/types"
	"strings"
	"testing"
)

func TestLocalityInfos(t *testing.T) {
	prog := NewProgram(nil)
	pkg := types.NewPackage("example.com/p", "p")
	if prog.PackageSyntaxParsed(pkg) {
		t.Fatal("new package was already marked as parsed")
	}
	prog.MarkPackageSyntaxParsed(pkg)
	if !prog.PackageSyntaxParsed(pkg) {
		t.Fatal("package syntax parsed marker was not retained")
	}
	if prog.PackageSyntaxParsed(types.NewPackage("example.com/p", "p")) {
		t.Fatal("syntax marker was shared by distinct package objects")
	}

	name := "example.com/p.value"
	prog.SetLocalityInfo(name, LocalityInfo{
		Locality:       GoroutineLocal,
		HasInitializer: true,
		InitFunc:       "example.com/p.initLocal",
		InitOrder:      1,
	})
	prog.SetLocalStorage(name, LocalStoragePackage)

	want, ok := prog.VariableLocality(name)
	if !ok || want.Locality != GoroutineLocal || want.LocalStorage != LocalStoragePackage || !want.HasInitializer || want.InitFunc == "" || want.InitOrder != 1 {
		t.Fatalf("VariableLocality(%q) = %+v, %v", name, want, ok)
	}

	prog.SetLocalStorage("example.com/p.ordinary", LocalStorageNativeTLS)
	decls := prog.PackageLocalities("example.com/p")
	if len(decls) != 1 || decls[name] != want {
		t.Fatalf("PackageLocalities = %+v", decls)
	}
	delete(decls, name)
	if _, ok := prog.VariableLocality(name); !ok {
		t.Fatal("mutating PackageLocalities changed program metadata")
	}
}

func TestNeedsLocalContext(t *testing.T) {
	prog := NewProgram(nil)
	if prog.NeedsLocalContext() {
		t.Fatal("empty program needs a local context")
	}
	name := "example.com/p.value"
	prog.SetLocalityInfo(name, LocalityInfo{Locality: ThreadLocal})
	if !prog.NeedsLocalContext() {
		t.Fatal("unknown local storage did not conservatively require a context")
	}
	prog.SetLocalStorage(name, LocalStorageNativeTLS)
	if prog.NeedsLocalContext() {
		t.Fatal("native TLS required a local context")
	}
	prog.SetLocalityInfo(name, LocalityInfo{Locality: ThreadLocal, HasInitializer: true, InitFunc: "example.com/p.initValue", InitOrder: 1})
	if !prog.NeedsLocalContext() {
		t.Fatal("native TLS initializer failure storage did not require a context")
	}
	prog.SetLocalityInfo(name, LocalityInfo{Locality: ThreadLocal})
	prog.SetLocalStorage(name, LocalStoragePackage)
	if !prog.NeedsLocalContext() {
		t.Fatal("context storage was not detected")
	}
}

func TestRejectsLinknameLocality(t *testing.T) {
	prog := NewProgram(nil)
	target := "example.com/target.value"
	alias := "example.com/alias.value"
	prog.SetLocalityInfo(target, LocalityInfo{Locality: ThreadLocal, HasInitializer: true, InitFunc: "example.com/target.initValue", InitOrder: 1})
	prog.SetLocalStorage(target, LocalStoragePackage)
	if canonical, got, ok, err := prog.ResolveLocality(target); err != nil || canonical != target || !ok || got.LocalStorage != LocalStoragePackage {
		t.Fatalf("direct ResolveLocality(%q) = %q, %+v, %v, %v", target, canonical, got, ok, err)
	}

	prog.SetLinkname(alias, target)
	if err := prog.ValidateLocalities("example.com/alias"); err == nil || !strings.Contains(err.Error(), "cannot reference local variable") {
		t.Fatalf("alias-to-local error = %v", err)
	}

	localAlias := "example.com/alias.local"
	prog.SetLocalityInfo(localAlias, LocalityInfo{Locality: GoroutineLocal})
	prog.SetLinkname(localAlias, "example.com/target.ordinary")
	if err := prog.ValidateLocalities("example.com/alias"); err == nil || !strings.Contains(err.Error(), "cannot use go:linkname") {
		t.Fatalf("local-alias error = %v", err)
	}
}

func TestValidateLocalitiesIgnoresOrdinaryLinknameCycle(t *testing.T) {
	prog := NewProgram(nil)
	first := "example.com/p.first"
	second := "example.com/p.second"
	prog.SetLinkname(first, second)
	prog.SetLinkname(second, first)
	if err := prog.ValidateLocalities("example.com/p"); err != nil {
		t.Fatalf("ordinary linkname cycle affected locality validation: %v", err)
	}
	if _, _, _, err := prog.ResolveLocality(first); err == nil || !strings.Contains(err.Error(), "linkname cycle") {
		t.Fatalf("ResolveLocality cycle error = %v", err)
	}
}

func TestValidateLocalitySelfLinkname(t *testing.T) {
	prog := NewProgram(nil)
	name := "example.com/p.value"
	prog.SetLinkname(name, name)
	if err := prog.ValidateLocalities("example.com/p"); err != nil {
		t.Fatal(err)
	}
	if canonical, _, ok, err := prog.ResolveLocality(name); err != nil || canonical != name || ok {
		t.Fatalf("ordinary self-link ResolveLocality(%q) = %q, %v, %v", name, canonical, ok, err)
	}
	prog.SetLocalityInfo(name, LocalityInfo{Locality: ThreadLocal})
	if err := prog.ValidateLocalities("example.com/p"); err == nil || !strings.Contains(err.Error(), "cannot use go:linkname") {
		t.Fatalf("local self-linkname error = %v", err)
	}
}
