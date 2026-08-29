package demotest

import (
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"testing"
)

func TestPlanUsesPositiveProfileAndGOOS(t *testing.T) {
	manifest := Manifest{
		Profiles: []Profile{{Name: "embedded", LLGOArgs: []string{"-lto=full"}, Target: "esp32", Emulator: true}},
		Cases: []Case{
			{ID: "both", Profiles: []string{"embedded"}, GOOS: []string{"linux", "darwin"}},
			{ID: "darwin", Profiles: []string{"embedded"}, GOOS: []string{"darwin"}},
			{ID: "other-profile", Profiles: []string{"host"}, GOOS: []string{"linux"}},
		},
	}
	plan, err := Plan(&manifest, "embedded", "linux")
	if err != nil {
		t.Fatal(err)
	}
	if len(plan) != 1 || plan[0].Case.ID != "both" {
		t.Fatalf("Plan = %#v", plan)
	}
	wantArgs := []string{"run", "-lto=full", "-target=esp32", "-emulator", "."}
	if got := plan[0].LLGOArguments(); !reflect.DeepEqual(got, wantArgs) {
		t.Fatalf("LLGOArguments = %q, want %q", got, wantArgs)
	}
}

func TestPlanRejectsUnknownProfile(t *testing.T) {
	_, err := Plan(&Manifest{}, "missing", "linux")
	if err == nil {
		t.Fatal("Plan unexpectedly accepted missing profile")
	}
}

func TestPlanRejectsEmptyGOOSSelection(t *testing.T) {
	manifest := &Manifest{
		Profiles: []Profile{{Name: "host"}},
		Cases:    []Case{{ID: "linux-only", Profiles: []string{"host"}, GOOS: []string{"linux"}}},
	}
	if _, err := Plan(manifest, "host", "darwin"); err == nil {
		t.Fatal("Plan unexpectedly accepted an empty GOOS selection")
	}
}

func TestFilterPlanAcceptsRepeatedIDAndDirectorySelectors(t *testing.T) {
	plan := []PlannedCase{
		{Case: Case{ID: "first", Dir: "_demo/go/first"}},
		{Case: Case{ID: "second", Dir: "_demo/go/second"}},
		{Case: Case{ID: "third", Dir: "_demo/go/third"}},
	}
	filtered, err := FilterPlan(plan, []string{"third", "_demo/go/first"})
	if err != nil {
		t.Fatal(err)
	}
	if got := []string{filtered[0].Case.ID, filtered[1].Case.ID}; !reflect.DeepEqual(got, []string{"first", "third"}) {
		t.Fatalf("FilterPlan IDs = %q", got)
	}
	if _, err := FilterPlan(plan, []string{"missing"}); err == nil {
		t.Fatal("FilterPlan unexpectedly accepted missing selector")
	}
	if _, err := FilterPlan(plan, []string{"first", "_demo/go/first"}); err == nil {
		t.Fatal("FilterPlan unexpectedly accepted duplicate selection")
	}
}

func TestFilterPlanHandlesIdentityAndAmbiguousSelectors(t *testing.T) {
	plan := []PlannedCase{
		{Case: Case{ID: "first", Dir: "shared"}},
		{Case: Case{ID: "shared", Dir: "_demo/go/second"}},
	}
	got, err := FilterPlan(plan, nil)
	if err != nil || !reflect.DeepEqual(got, plan) {
		t.Fatalf("FilterPlan identity = %#v, %v", got, err)
	}
	if _, err := FilterPlan(plan, []string{"shared"}); err == nil || !strings.Contains(err.Error(), "ambiguous") {
		t.Fatalf("FilterPlan ambiguous error = %v", err)
	}
	if _, err := FilterPlan(plan, []string{"missing", "first", "shared"}); err == nil ||
		!strings.Contains(err.Error(), "not in the plan") || !strings.Contains(err.Error(), "ambiguous") {
		t.Fatalf("FilterPlan combined error = %v", err)
	}
}

func TestRepositoryManifestDefinesFocusedRunSets(t *testing.T) {
	root, err := filepath.Abs(filepath.Join("..", ".."))
	if err != nil {
		t.Fatal(err)
	}
	manifest, err := LoadManifest(filepath.Join(root, "_demo", "manifest.json"))
	if err != nil {
		t.Fatal(err)
	}
	if err := Validate(root, manifest); err != nil {
		t.Fatal(err)
	}
	hostGolden, err := os.ReadFile(filepath.Join("testdata", "focused-host.txt"))
	if err != nil {
		t.Fatal(err)
	}
	hostDirectories := strings.Fields(string(hostGolden))
	assertPlanDirectories(t, manifest, "host", hostDirectories)
	assertPlanDirectories(t, manifest, "host-lto", []string{
		"_demo/c/asmfullcall", "_demo/c/cgofull", "_demo/c/cppintf",
		"_demo/go/cabi", "_demo/go/export", "_demo/go/reflect",
		"_demo/go/stdlib", "_demo/go/sync",
	})
	assertPlanDirectories(t, manifest, "host-deadcodedrop", []string{
		"_demo/c/asmfullcall", "_demo/c/cgofull", "_demo/go/cabi",
		"_demo/go/reflect", "_demo/go/stdlib", "_demo/go/sync",
	})

	windowsDirectories := make([]string, 0, len(hostDirectories)-1)
	for _, dir := range hostDirectories {
		if dir != "_demo/c/thread" {
			windowsDirectories = append(windowsDirectories, dir)
		}
	}
	windows, err := Plan(manifest, "host", "windows")
	if err != nil {
		t.Fatal(err)
	}
	gotWindows := planDirectories(windows)
	sort.Strings(windowsDirectories)
	if !reflect.DeepEqual(gotWindows, windowsDirectories) {
		t.Errorf("Windows host directories:\ngot  %q\nwant %q", gotWindows, windowsDirectories)
	}

	wantProfiles := map[string]Profile{
		"host":              {Name: "host", LLGOArgs: []string{}},
		"host-lto":          {Name: "host-lto", LLGOArgs: []string{"-lto=full", "-globaldce"}},
		"host-deadcodedrop": {Name: "host-deadcodedrop", LLGOArgs: []string{"-deadcodedrop"}},
		"esp32":             {Name: "esp32", LLGOArgs: []string{}, Target: "esp32", Emulator: true},
		"esp32c3-basic":     {Name: "esp32c3-basic", LLGOArgs: []string{}, Target: "esp32c3-basic", Emulator: true},
		"model":             {Name: "model", LLGOArgs: []string{}},
	}
	for _, profile := range manifest.Profiles {
		want, ok := wantProfiles[profile.Name]
		if !ok {
			t.Errorf("unexpected profile %q", profile.Name)
			continue
		}
		if !reflect.DeepEqual(profile, want) {
			t.Errorf("profile %q = %#v, want %#v", profile.Name, profile, want)
		}
		delete(wantProfiles, profile.Name)
	}
	if len(wantProfiles) != 0 {
		t.Errorf("missing profiles: %v", wantProfiles)
	}

	counts := map[string]int{
		"host": 32, "host-lto": 8, "host-deadcodedrop": 6,
		"esp32": 5, "esp32c3-basic": 6, "model": 1,
	}
	for profile, want := range counts {
		plan, err := Plan(manifest, profile, "linux")
		if err != nil {
			t.Fatal(err)
		}
		if got := len(plan); got != want {
			t.Errorf("Plan(%q) has %d cases, want %d", profile, got, want)
		}
	}
	if got, want := len(gotWindows), 31; got != want {
		t.Errorf("Windows host plan has %d cases, want %d", got, want)
	}

	assertPlanDirectories(t, manifest, "esp32", []string{
		"_demo/c/catomic", "_demo/c/hello", "_demo/c/qsort",
		"_demo/go/ifaceconv", "_demo/go/linkname",
	})
	assertPlanDirectories(t, manifest, "esp32c3-basic", []string{
		"_demo/c/hello", "_demo/c/qsort",
		"_demo/go/cabi", "_demo/go/ifaceconv", "_demo/go/issue1538", "_demo/go/linkname",
	})
	assertPlanDirectories(t, manifest, "model", []string{"_demo/workflow/model/llama2-c"})
}

func assertPlanDirectories(t *testing.T, manifest *Manifest, profile string, want []string) {
	t.Helper()
	plan, err := Plan(manifest, profile, "linux")
	if err != nil {
		t.Fatal(err)
	}
	got := planDirectories(plan)
	want = append([]string(nil), want...)
	sort.Strings(want)
	if !reflect.DeepEqual(got, want) {
		t.Errorf("Plan(%q) directories:\ngot  %q\nwant %q", profile, got, want)
	}
}

func planDirectories(plan []PlannedCase) []string {
	got := make([]string, len(plan))
	for i, planned := range plan {
		got[i] = planned.Case.Dir
	}
	sort.Strings(got)
	return got
}
