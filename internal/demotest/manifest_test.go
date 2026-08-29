package demotest

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestLoadManifestRejectsUnknownFields(t *testing.T) {
	path := filepath.Join(t.TempDir(), "manifest.json")
	if err := os.WriteFile(path, []byte(`{"version":1,"profiles":[],"cases":[],"support":[],"workflow_owned":[],"typo":true}`), 0o666); err != nil {
		t.Fatal(err)
	}
	_, err := LoadManifest(path)
	if err == nil || !strings.Contains(err.Error(), "unknown field") {
		t.Fatalf("LoadManifest error = %v, want unknown field", err)
	}
}

func TestValidateChecksCompleteAndUniqueOwnership(t *testing.T) {
	root := newTestRoot(t, "_demo/go/owned", "_demo/go/missing")
	manifest := validTestManifest("_demo/go/owned")
	err := Validate(root, &manifest)
	if err == nil || !strings.Contains(err.Error(), "source directory has no owner: _demo/go/missing") {
		t.Fatalf("Validate error = %v, want missing ownership", err)
	}

	manifest.Support = append(manifest.Support, SupportDirectory{
		Dir:       "_demo/go/owned",
		Owner:     "owner",
		Rationale: "duplicate for test",
	})
	err = Validate(root, &manifest)
	if err == nil || !strings.Contains(err.Error(), "duplicate ownership") {
		t.Fatalf("Validate error = %v, want duplicate ownership", err)
	}
}

func TestValidateAcceptsAllOwnershipKinds(t *testing.T) {
	root := newTestRoot(t, "_demo/go/case", "_demo/c/support", "_demo/embed/workflow")
	manifest := validTestManifest("_demo/go/case")
	manifest.Support = []SupportDirectory{{
		Dir: "_demo/c/support", Owner: "case", Rationale: "imported helper",
	}}
	manifest.Workflow = []WorkflowDirectory{{
		Dir: "_demo/embed/workflow", Workflow: "manual-test", Rationale: "specialized target check",
	}}
	if err := Validate(root, &manifest); err != nil {
		t.Fatal(err)
	}
}

func TestValidateRejectsUnknownSupportOwner(t *testing.T) {
	root := newTestRoot(t, "_demo/go/case", "_demo/c/support")
	manifest := validTestManifest("_demo/go/case")
	manifest.Support = []SupportDirectory{{
		Dir: "_demo/c/support", Owner: "missing", Rationale: "invalid owner for test",
	}}
	err := Validate(root, &manifest)
	if err == nil || !strings.Contains(err.Error(), "unknown case owner missing") {
		t.Fatalf("Validate error = %v, want unknown support owner", err)
	}
}

func TestValidateCheckContracts(t *testing.T) {
	root := newTestRoot(t, "_demo/go/case")
	manifest := validTestManifest("_demo/go/case")
	manifest.Cases[0].Check = Check{Kind: "exit"}
	err := Validate(root, &manifest)
	if err == nil || !strings.Contains(err.Error(), "exit check requires rationale") {
		t.Fatalf("Validate error = %v, want exit rationale error", err)
	}

	manifest.Cases[0].Check = Check{Kind: "stdout", Golden: "_demo/go/case/missing.golden"}
	err = Validate(root, &manifest)
	if err == nil || !strings.Contains(err.Error(), "missing.golden") {
		t.Fatalf("Validate error = %v, want missing golden", err)
	}

	manifest.Cases[0].Check = Check{Kind: "stdout", Golden: ".."}
	err = Validate(root, &manifest)
	if err == nil || !strings.Contains(err.Error(), "clean, slash-separated") {
		t.Fatalf("Validate error = %v, want repository path error", err)
	}
}

func newTestRoot(t *testing.T, sourceDirs ...string) string {
	t.Helper()
	root := t.TempDir()
	for _, base := range []string{"_demo/go", "_demo/c", "_demo/py", "_demo/embed"} {
		if err := os.MkdirAll(filepath.Join(root, filepath.FromSlash(base)), 0o777); err != nil {
			t.Fatal(err)
		}
	}
	for _, dir := range sourceDirs {
		absolute := filepath.Join(root, filepath.FromSlash(dir))
		if err := os.MkdirAll(absolute, 0o777); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join(absolute, "main.go"), []byte("package main\n"), 0o666); err != nil {
			t.Fatal(err)
		}
	}
	return root
}

func validTestManifest(dir string) Manifest {
	return Manifest{
		Version:  ManifestVersion,
		Profiles: []Profile{{Name: "host", LLGOArgs: []string{}}},
		Cases: []Case{{
			ID: "case", Dir: dir, Class: "integration", Capability: "test capability",
			Profiles: []string{"host"}, GOOS: []string{"linux"}, Check: Check{Kind: "self"},
		}},
		Support:  []SupportDirectory{},
		Workflow: []WorkflowDirectory{},
	}
}
