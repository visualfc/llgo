//go:build !llgo

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

package build

import (
	"go/constant"
	"go/token"
	"go/types"
	"os"
	"path/filepath"
	"reflect"
	"testing"
	"time"

	"github.com/xgo-dev/llgo/internal/packages"
)

func llgoFilesTestPackage(t *testing.T, dir, value string) *packages.Package {
	t.Helper()
	goFile := filepath.Join(dir, "package.go")
	if err := os.WriteFile(goFile, []byte("package fixture\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	typesPkg := types.NewPackage("example.com/fixture", "fixture")
	typesPkg.Scope().Insert(types.NewConst(token.NoPos, typesPkg, "LLGoFiles", types.Typ[types.UntypedString], constant.MakeString(value)))
	return &packages.Package{
		ID:         "example.com/fixture",
		PkgPath:    "example.com/fixture",
		GoFiles:    []string{goFile},
		ExportFile: filepath.Join(dir, "fixture.a"),
		Types:      typesPkg,
	}
}

func TestLLGoFileInputsResolvePathsAndFlags(t *testing.T) {
	dir := t.TempDir()
	pkg := llgoFilesTestPackage(t, dir, "$LLGO_TEST_CFLAGS: wrap.c; ; wrap.S")
	ctx := &context{commands: commandEnv{environ: []string{"LLGO_TEST_CFLAGS=-DVALUE=1"}}}
	inputs := llgoPkgFileInputs(ctx, pkg)
	if len(inputs) != 2 {
		t.Fatalf("LLGoFiles inputs = %#v, want two files", inputs)
	}
	for i, name := range []string{"wrap.c", "wrap.S"} {
		if got, want := inputs[i].path, filepath.Join(dir, name); got != want {
			t.Errorf("input %d path = %q, want %q", i, got, want)
		}
		if got, want := inputs[i].compilerArgs, []string{"-DVALUE=1"}; !reflect.DeepEqual(got, want) {
			t.Errorf("input %d compiler args = %q, want %q", i, got, want)
		}
	}
	pkg.GoFiles = nil
	if inputs := llgoPkgFileInputs(&context{}, pkg); len(inputs) != 0 {
		t.Fatalf("LLGoFiles inputs without a package source directory = %#v, want none", inputs)
	}
}

func TestLLGoFilesFingerprintUsesContent(t *testing.T) {
	dir := t.TempDir()
	path := filepath.Join(dir, "wrap.c")
	if err := os.WriteFile(path, []byte("int value = 1;\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	pkg := llgoFilesTestPackage(t, dir, "wrap.c")
	ctx := &context{buildConf: &Config{}}

	fingerprint := func() (string, llgoFileDigest) {
		manifest := newManifestBuilder()
		if err := ctx.collectPackageInputs(manifest, &aPackage{Package: pkg}); err != nil {
			t.Fatal(err)
		}
		if len(manifest.pkg.LLGoFiles) != 1 {
			t.Fatalf("manifest LLGoFiles = %#v, want one file", manifest.pkg.LLGoFiles)
		}
		return manifest.Fingerprint(), manifest.pkg.LLGoFiles[0]
	}

	before, beforeFile := fingerprint()
	info, err := os.Stat(path)
	if err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(path, []byte("int value = 2;\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	// Preserve size and modification time to prove that content, rather than
	// the metadata used for ordinary Go inputs, invalidates the package cache.
	if err := os.Chtimes(path, time.Unix(0, info.ModTime().UnixNano()), time.Unix(0, info.ModTime().UnixNano())); err != nil {
		t.Fatal(err)
	}
	after, afterFile := fingerprint()
	if before == after {
		t.Fatal("LLGoFiles content change did not change the package fingerprint")
	}
	if beforeFile.ContentHash == afterFile.ContentHash {
		t.Fatal("LLGoFiles content change did not change its content hash")
	}
}

func TestLLGoFileOutputsAreProcessPrivate(t *testing.T) {
	first, err := genLLGoFileOutput("wrap.c", ".o")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(first)
	second, err := genLLGoFileOutput("wrap.c", ".o")
	if err != nil {
		t.Fatal(err)
	}
	defer os.Remove(second)
	if first == second {
		t.Fatalf("two LLGoFiles compilations selected the same object path %q", first)
	}
}

func TestDigestLLGoFileInputsEdges(t *testing.T) {
	if digests, err := digestLLGoFileInputs(nil, nil); err != nil || digests != nil {
		t.Fatalf("empty digests = %#v, %v; want nil, nil", digests, err)
	}

	path := filepath.Join(t.TempDir(), "overlay.c")
	digests, err := digestLLGoFileInputs([]llgoFileInput{
		{path: path, compilerArgs: []string{"-DZ=1"}},
		{path: path, compilerArgs: []string{"-DA=1"}},
	}, map[string][]byte{path: []byte("overlay content")})
	if err != nil {
		t.Fatal(err)
	}
	if len(digests) != 2 || !reflect.DeepEqual(digests[0].CompilerArgs, []string{"-DA=1"}) ||
		digests[0].ContentHash != digestBytes([]byte("overlay content")) {
		t.Fatalf("overlay digests = %#v", digests)
	}

	missing := filepath.Join(t.TempDir(), "missing.c")
	if _, err := digestLLGoFileInputs([]llgoFileInput{{path: missing}}, nil); err == nil {
		t.Fatal("digesting a missing LLGoFiles input unexpectedly succeeded")
	}
}
