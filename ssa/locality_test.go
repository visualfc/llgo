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

func TestResolveLinknameLocality(t *testing.T) {
	prog := NewProgram(nil)
	target := "example.com/target.Value"
	alias := "example.com/alias.Value"
	prog.SetLocalityInfo(target, LocalityInfo{Locality: ThreadLocal, HasInitializer: true, InitFunc: "example.com/target.initValue", InitOrder: 1})
	prog.SetLocalStorage(target, LocalStoragePackage)
	prog.SetLinkname(alias, target)

	_, got, ok, err := prog.ResolveLocality(alias)
	if err != nil {
		t.Fatal(err)
	}
	if !ok || got.Locality != ThreadLocal || got.LocalStorage != LocalStoragePackage || got.InitFunc != "example.com/target.initValue" || got.InitOrder != 1 {
		t.Fatalf("ResolveLocality(%q) = %+v, %v", alias, got, ok)
	}
	if err := prog.ValidateLocalities("example.com/alias"); err != nil {
		t.Fatal(err)
	}
	sameKind := "example.com/alias.SameKind"
	prog.SetLinkname(sameKind, target)
	prog.SetLocalityInfo(sameKind, LocalityInfo{Locality: ThreadLocal})
	if _, got, ok, err := prog.ResolveLocality(sameKind); err != nil || !ok || got.InitFunc != "example.com/target.initValue" {
		t.Fatalf("same-kind ResolveLocality(%q) = %+v, %v", sameKind, got, ok)
	}

	incompatible := "example.com/alias.Incompatible"
	prog.SetLinkname(incompatible, target)
	prog.SetLocalityInfo(incompatible, LocalityInfo{Locality: ThreadLocal, HasInitializer: true, InitFunc: "example.com/alias.initValue", InitOrder: 1})
	if err := prog.ValidateLocalities("example.com/alias"); err == nil || !strings.Contains(err.Error(), "incompatible local initializers") {
		t.Fatalf("initializer mismatch error = %v", err)
	}
	targetDecl, _ := prog.VariableLocality(target)
	prog.SetLocalityInfo(incompatible, targetDecl.Info)

	storageMismatch := "example.com/alias.StorageMismatch"
	prog.SetLinkname(storageMismatch, target)
	prog.SetLocalityInfo(storageMismatch, LocalityInfo{Locality: ThreadLocal})
	prog.SetLocalStorage(storageMismatch, LocalStorageNativeTLS)
	if err := prog.ValidateLocalities("example.com/alias"); err == nil || !strings.Contains(err.Error(), "incompatible local storage") {
		t.Fatalf("storage mismatch error = %v", err)
	}
	prog.SetLocalStorage(storageMismatch, LocalStoragePackage)

	prog.SetLocalityInfo(alias, LocalityInfo{Locality: GoroutineLocal})
	if err := prog.ValidateLocalities("example.com/alias"); err == nil || !strings.Contains(err.Error(), "uses //llgo:gls") {
		t.Fatalf("locality mismatch error = %v", err)
	}
}

func TestValidateLocalityLinknameCycle(t *testing.T) {
	prog := NewProgram(nil)
	prog.SetLinkname("example.com/p.First", "example.com/p.Second")
	prog.SetLinkname("example.com/p.Second", "example.com/p.First")
	prog.SetLocalityInfo("example.com/p.First", LocalityInfo{Locality: ThreadLocal})
	if err := prog.ValidateLocalities("example.com/p"); err == nil || !strings.Contains(err.Error(), "linkname cycle") {
		t.Fatalf("linkname cycle error = %v", err)
	}
}

func TestValidateLocalityAllowsSelfLinkname(t *testing.T) {
	prog := NewProgram(nil)
	name := "example.com/p.Value"
	prog.SetLinkname(name, name)
	prog.SetLocalityInfo(name, LocalityInfo{Locality: ThreadLocal})
	if err := prog.ValidateLocalities("example.com/p"); err != nil {
		t.Fatal(err)
	}
	if _, got, ok, err := prog.ResolveLocality(name); err != nil || !ok || got.Locality != ThreadLocal {
		t.Fatalf("ResolveLocality(%q) = %+v, %v", name, got, ok)
	}
}
