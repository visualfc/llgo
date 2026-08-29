// Package demotest provides the manifest, planning, and execution support for
// the repository's demo and integration tests.
package demotest

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"io/fs"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

const ManifestVersion = 1

// Manifest is the strictly decoded description of every Go source directory
// below _demo. A directory has exactly one owner: Cases, Support, or Workflow.
type Manifest struct {
	Version  int                 `json:"version"`
	Profiles []Profile           `json:"profiles"`
	Cases    []Case              `json:"cases"`
	Support  []SupportDirectory  `json:"support"`
	Workflow []WorkflowDirectory `json:"workflow_owned"`
}

type Profile struct {
	Name     string   `json:"name"`
	LLGOArgs []string `json:"llgo_args"`
	Target   string   `json:"target,omitempty"`
	Emulator bool     `json:"emulator,omitempty"`
}

type Case struct {
	ID           string   `json:"id"`
	Dir          string   `json:"dir"`
	Class        string   `json:"class"`
	Capability   string   `json:"capability"`
	Profiles     []string `json:"profiles"`
	GOOS         []string `json:"goos"`
	Timeout      string   `json:"timeout,omitempty"`
	Dependencies []string `json:"dependencies,omitempty"`
	History      string   `json:"history,omitempty"`
	Check        Check    `json:"check"`
}

type Check struct {
	Kind           string   `json:"kind"`
	Golden         string   `json:"golden,omitempty"`
	Rationale      string   `json:"rationale,omitempty"`
	StderrContains []string `json:"stderr_contains,omitempty"`
}

type SupportDirectory struct {
	Dir       string `json:"dir"`
	Owner     string `json:"owner"`
	Rationale string `json:"rationale"`
}

type WorkflowDirectory struct {
	Dir       string `json:"dir"`
	Workflow  string `json:"workflow"`
	Rationale string `json:"rationale"`
}

// LoadManifest decodes exactly one JSON value and rejects unknown fields.
func LoadManifest(path string) (*Manifest, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()

	dec := json.NewDecoder(f)
	dec.DisallowUnknownFields()
	var manifest Manifest
	if err := dec.Decode(&manifest); err != nil {
		return nil, fmt.Errorf("decode manifest: %w", err)
	}
	var trailing any
	if err := dec.Decode(&trailing); !errors.Is(err, io.EOF) {
		if err == nil {
			return nil, errors.New("decode manifest: multiple JSON values")
		}
		return nil, fmt.Errorf("decode manifest trailing data: %w", err)
	}
	return &manifest, nil
}

// Validate checks the schema invariants and verifies that every directory
// containing Go source under _demo has exactly one manifest owner.
func Validate(root string, manifest *Manifest) error {
	if manifest == nil {
		return errors.New("nil manifest")
	}
	var problems []string
	if manifest.Version != ManifestVersion {
		problems = append(problems, fmt.Sprintf("version: got %d, want %d", manifest.Version, ManifestVersion))
	}

	profiles := make(map[string]Profile, len(manifest.Profiles))
	for i, profile := range manifest.Profiles {
		where := fmt.Sprintf("profiles[%d]", i)
		if !validName(profile.Name) {
			problems = append(problems, where+": invalid name")
		} else if _, exists := profiles[profile.Name]; exists {
			problems = append(problems, where+": duplicate name "+profile.Name)
		} else {
			profiles[profile.Name] = profile
		}
		if profile.Emulator && profile.Target == "" {
			problems = append(problems, where+": emulator requires target")
		}
		for j, arg := range profile.LLGOArgs {
			if arg == "" || strings.IndexFunc(arg, func(r rune) bool { return r == '\x00' || r == '\n' || r == '\r' }) >= 0 {
				problems = append(problems, fmt.Sprintf("%s.llgo_args[%d]: invalid argument", where, j))
			}
		}
	}
	if len(profiles) == 0 {
		problems = append(problems, "profiles: at least one profile is required")
	}

	owners := make(map[string]string)
	caseIDs := make(map[string]struct{}, len(manifest.Cases))
	for i, c := range manifest.Cases {
		where := fmt.Sprintf("cases[%d]", i)
		if !validName(c.ID) {
			problems = append(problems, where+": invalid id")
		} else if _, exists := caseIDs[c.ID]; exists {
			problems = append(problems, where+": duplicate id "+c.ID)
		} else {
			caseIDs[c.ID] = struct{}{}
		}
		validateOwnedPath(root, c.Dir, where+".dir", "case "+c.ID, owners, &problems)
		if !oneOf(c.Class, "example", "regression", "stdlib", "integration", "target-smoke", "optional") {
			problems = append(problems, where+": invalid class "+c.Class)
		}
		if strings.TrimSpace(c.Capability) == "" {
			problems = append(problems, where+": capability is required")
		}
		if len(c.Profiles) == 0 {
			problems = append(problems, where+": at least one profile is required")
		}
		seenProfiles := make(map[string]struct{}, len(c.Profiles))
		for _, name := range c.Profiles {
			if _, ok := profiles[name]; !ok {
				problems = append(problems, where+": unknown profile "+name)
			}
			if _, exists := seenProfiles[name]; exists {
				problems = append(problems, where+": duplicate profile "+name)
			}
			seenProfiles[name] = struct{}{}
		}
		if len(c.GOOS) == 0 {
			problems = append(problems, where+": goos is required")
		}
		seenGOOS := make(map[string]struct{}, len(c.GOOS))
		for _, goos := range c.GOOS {
			if !oneOf(goos, "aix", "android", "darwin", "dragonfly", "freebsd", "illumos", "ios", "js", "linux", "netbsd", "openbsd", "plan9", "solaris", "wasip1", "windows") {
				problems = append(problems, where+": invalid goos "+goos)
			}
			if _, exists := seenGOOS[goos]; exists {
				problems = append(problems, where+": duplicate goos "+goos)
			}
			seenGOOS[goos] = struct{}{}
		}
		if c.Timeout != "" {
			d, err := time.ParseDuration(c.Timeout)
			if err != nil || d <= 0 {
				problems = append(problems, where+": timeout must be a positive Go duration")
			}
		}
		validateCheck(root, c.Check, where+".check", &problems)
	}

	workflowOwners := make(map[string]struct{}, len(manifest.Workflow))
	for i, owned := range manifest.Workflow {
		where := fmt.Sprintf("workflow_owned[%d]", i)
		validateOwnedPath(root, owned.Dir, where+".dir", "workflow", owners, &problems)
		if strings.TrimSpace(owned.Workflow) == "" {
			problems = append(problems, where+": workflow is required")
		} else if strings.HasPrefix(owned.Workflow, "manual-") {
			workflowOwners[owned.Workflow] = struct{}{}
		} else if err := validateRepoPath(owned.Workflow); err != nil {
			problems = append(problems, where+".workflow: "+err.Error())
		} else {
			workflowOwners[owned.Workflow] = struct{}{}
			info, err := os.Stat(filepath.Join(root, filepath.FromSlash(owned.Workflow)))
			if err != nil {
				problems = append(problems, where+".workflow: "+err.Error())
			} else if !info.Mode().IsRegular() {
				problems = append(problems, where+".workflow: not a regular file")
			}
		}
		if strings.TrimSpace(owned.Rationale) == "" {
			problems = append(problems, where+": rationale is required")
		}
	}
	for i, support := range manifest.Support {
		where := fmt.Sprintf("support[%d]", i)
		validateOwnedPath(root, support.Dir, where+".dir", "support", owners, &problems)
		if workflow, ok := strings.CutPrefix(support.Owner, "workflow:"); ok {
			if _, exists := workflowOwners[workflow]; !exists {
				problems = append(problems, where+": unknown workflow owner "+workflow)
			}
		} else if _, exists := caseIDs[support.Owner]; !exists {
			problems = append(problems, where+": unknown case owner "+support.Owner)
		}
		if strings.TrimSpace(support.Rationale) == "" {
			problems = append(problems, where+": rationale is required")
		}
	}

	sourceDirs, err := findGoSourceDirectories(root)
	if err != nil {
		problems = append(problems, "source audit: "+err.Error())
	} else {
		for dir := range sourceDirs {
			if _, ok := owners[dir]; !ok {
				problems = append(problems, "source directory has no owner: "+dir)
			}
		}
		for dir, owner := range owners {
			if _, ok := sourceDirs[dir]; !ok {
				problems = append(problems, fmt.Sprintf("%s owns a directory without direct Go source: %s", owner, dir))
			}
		}
	}

	if len(problems) != 0 {
		sort.Strings(problems)
		return errors.New(strings.Join(problems, "\n"))
	}
	return nil
}

func validateOwnedPath(root, path, where, owner string, owners map[string]string, problems *[]string) {
	if err := validateRepoPath(path); err != nil {
		*problems = append(*problems, where+": "+err.Error())
		return
	}
	if !strings.HasPrefix(path, "_demo/") {
		*problems = append(*problems, where+": path must be below _demo")
		return
	}
	if previous, exists := owners[path]; exists {
		*problems = append(*problems, fmt.Sprintf("%s: duplicate ownership by %s and %s", where, previous, owner))
		return
	}
	owners[path] = owner
	info, err := os.Stat(filepath.Join(root, filepath.FromSlash(path)))
	if err != nil {
		*problems = append(*problems, where+": "+err.Error())
	} else if !info.IsDir() {
		*problems = append(*problems, where+": not a directory")
	}
}

func validateCheck(root string, check Check, where string, problems *[]string) {
	if !oneOf(check.Kind, "self", "stdout", "exit", "failure") {
		*problems = append(*problems, where+": invalid kind "+check.Kind)
		return
	}
	switch check.Kind {
	case "self":
		if check.Golden != "" || check.Rationale != "" || len(check.StderrContains) != 0 {
			*problems = append(*problems, where+": self check has incompatible fields")
		}
	case "stdout":
		if err := validateRepoPath(check.Golden); err != nil {
			*problems = append(*problems, where+".golden: "+err.Error())
		} else if _, err := os.Stat(filepath.Join(root, filepath.FromSlash(check.Golden))); err != nil {
			*problems = append(*problems, where+".golden: "+err.Error())
		}
		if check.Rationale != "" || len(check.StderrContains) != 0 {
			*problems = append(*problems, where+": stdout check has incompatible fields")
		}
	case "exit":
		if strings.TrimSpace(check.Rationale) == "" {
			*problems = append(*problems, where+": exit check requires rationale")
		}
		if check.Golden != "" || len(check.StderrContains) != 0 {
			*problems = append(*problems, where+": exit check has incompatible fields")
		}
	case "failure":
		if len(check.StderrContains) == 0 {
			*problems = append(*problems, where+": failure check requires stderr_contains")
		}
		for i, fragment := range check.StderrContains {
			if fragment == "" {
				*problems = append(*problems, fmt.Sprintf("%s.stderr_contains[%d]: empty fragment", where, i))
			}
		}
		if check.Golden != "" || check.Rationale != "" {
			*problems = append(*problems, where+": failure check has incompatible fields")
		}
	}
}

func validateRepoPath(path string) error {
	if path == "" {
		return errors.New("path is required")
	}
	if filepath.IsAbs(path) || strings.Contains(path, "\\") || filepath.ToSlash(filepath.Clean(filepath.FromSlash(path))) != path || path == "." || path == ".." || strings.HasPrefix(path, "../") {
		return errors.New("path must be a clean, slash-separated repository-relative path")
	}
	return nil
}

func findGoSourceDirectories(root string) (map[string]struct{}, error) {
	result := make(map[string]struct{})
	for _, base := range []string{"_demo/go", "_demo/c", "_demo/py", "_demo/embed"} {
		absolute := filepath.Join(root, filepath.FromSlash(base))
		err := filepath.WalkDir(absolute, func(path string, entry fs.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if entry.IsDir() || !strings.HasSuffix(entry.Name(), ".go") {
				return nil
			}
			relative, err := filepath.Rel(root, filepath.Dir(path))
			if err != nil {
				return err
			}
			result[filepath.ToSlash(relative)] = struct{}{}
			return nil
		})
		if err != nil {
			return nil, err
		}
	}
	return result, nil
}

func validName(value string) bool {
	if value == "" {
		return false
	}
	for _, r := range value {
		if (r < 'a' || r > 'z') && (r < '0' || r > '9') && r != '-' {
			return false
		}
	}
	return true
}

func oneOf(value string, allowed ...string) bool {
	for _, candidate := range allowed {
		if value == candidate {
			return true
		}
	}
	return false
}
