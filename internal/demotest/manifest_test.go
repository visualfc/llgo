package demotest

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestLoadManifestIsStrict(t *testing.T) {
	for _, test := range []struct {
		name string
		data string
		want string
	}{
		{"valid", `{"version":1,"profiles":[],"cases":[],"support":[],"workflow_owned":[]}`, ""},
		{"unknown descriptive field", `{"version":1,"profiles":[],"cases":[],"support":[],"workflow_owned":[],"class":"old"}`, "unknown field"},
		{"multiple values", `{}` + "\n{}", "multiple JSON values"},
		{"malformed", `{`, "decode manifest"},
	} {
		t.Run(test.name, func(t *testing.T) {
			path := filepath.Join(t.TempDir(), "manifest.json")
			if err := os.WriteFile(path, []byte(test.data), 0o666); err != nil {
				t.Fatal(err)
			}
			_, err := LoadManifest(path)
			if test.want == "" && err != nil {
				t.Fatal(err)
			}
			if test.want != "" && (err == nil || !strings.Contains(err.Error(), test.want)) {
				t.Fatalf("LoadManifest error = %v, want %q", err, test.want)
			}
		})
	}
}

func TestValidateOwnershipAndPlanningInvariants(t *testing.T) {
	root := newManifestRoot(t)
	valid := validManifest()
	if err := Validate(root, valid); err != nil {
		t.Fatalf("valid manifest: %v", err)
	}

	for _, test := range []struct {
		name   string
		mutate func(*Manifest)
		want   string
	}{
		{"duplicate profile", func(m *Manifest) { m.Profiles = append(m.Profiles, m.Profiles[0]) }, "duplicate name host"},
		{"unknown case profile", func(m *Manifest) { m.Cases[0].Profiles = []string{"missing"} }, "unknown profile missing"},
		{"model isolation", func(m *Manifest) { m.Cases[0].Profiles = []string{"host", "model"} }, "model profile must be exclusive"},
		{"duplicate case id", func(m *Manifest) { m.Cases = append(m.Cases, m.Cases[0]) }, "duplicate id case"},
		{"duplicate ownership", func(m *Manifest) { m.Support[0].Dir = m.Cases[0].Dir }, "duplicate ownership"},
		{"unknown support owner", func(m *Manifest) { m.Support[0].Owner = "missing" }, "unknown case owner missing"},
		{"unowned source", func(m *Manifest) { m.Support = nil }, "source directory has no owner"},
		{"invalid goos", func(m *Manifest) { m.Cases[0].GOOS = []string{"haiku"} }, "invalid goos haiku"},
	} {
		t.Run(test.name, func(t *testing.T) {
			manifest := validManifest()
			test.mutate(manifest)
			if err := Validate(root, manifest); err == nil || !strings.Contains(err.Error(), test.want) {
				t.Fatalf("Validate error = %v, want %q", err, test.want)
			}
		})
	}
}

func newManifestRoot(t *testing.T) string {
	t.Helper()
	root := t.TempDir()
	for _, dir := range []string{
		"_demo/go/case", "_demo/c/support", "_demo/py", "_demo/embed/workflow", "_demo/workflow",
	} {
		if err := os.MkdirAll(filepath.Join(root, filepath.FromSlash(dir)), 0o777); err != nil {
			t.Fatal(err)
		}
	}
	for _, file := range []string{"_demo/go/case/main.go", "_demo/c/support/support.go", "_demo/embed/workflow/main.go"} {
		if err := os.WriteFile(filepath.Join(root, filepath.FromSlash(file)), []byte("package main\n"), 0o666); err != nil {
			t.Fatal(err)
		}
	}
	return root
}

func validManifest() *Manifest {
	return &Manifest{
		Version: ManifestVersion,
		Profiles: []Profile{
			{Name: "host"},
			{Name: "model"},
		},
		Cases:    []Case{{ID: "case", Dir: "_demo/go/case", Profiles: []string{"host"}, GOOS: []string{"linux"}}},
		Support:  []SupportDirectory{{Dir: "_demo/c/support", Owner: "case"}},
		Workflow: []WorkflowDirectory{{Dir: "_demo/embed/workflow", Workflow: "manual-hardware"}},
	}
}
