// Package demotest plans and runs the repository's demo integration cases.
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
)

const ManifestVersion = 1

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
	ID       string   `json:"id"`
	Dir      string   `json:"dir"`
	Profiles []string `json:"profiles"`
	GOOS     []string `json:"goos"`
}

type SupportDirectory struct {
	Dir   string `json:"dir"`
	Owner string `json:"owner"`
}

type WorkflowDirectory struct {
	Dir      string `json:"dir"`
	Workflow string `json:"workflow"`
}

func LoadManifest(path string) (*Manifest, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	decoder := json.NewDecoder(f)
	decoder.DisallowUnknownFields()
	var manifest Manifest
	if err := decoder.Decode(&manifest); err != nil {
		return nil, fmt.Errorf("decode manifest: %w", err)
	}
	var trailing any
	if err := decoder.Decode(&trailing); !errors.Is(err, io.EOF) {
		if err == nil {
			return nil, errors.New("decode manifest: multiple JSON values")
		}
		return nil, fmt.Errorf("decode manifest trailing data: %w", err)
	}
	return &manifest, nil
}

// Validate enforces planning invariants and exact ownership of every Go-source
// directory below _demo. Descriptive capability/history data lives in design
// documentation instead of this runtime schema.
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
			if arg == "" || strings.ContainsAny(arg, "\x00\r\n") {
				problems = append(problems, fmt.Sprintf("%s.llgo_args[%d]: invalid argument", where, j))
			}
		}
	}
	if len(profiles) == 0 {
		problems = append(problems, "profiles: at least one profile is required")
	}

	owners := make(map[string]string)
	caseIDs := make(map[string]struct{}, len(manifest.Cases))
	for i, demoCase := range manifest.Cases {
		where := fmt.Sprintf("cases[%d]", i)
		if !validName(demoCase.ID) {
			problems = append(problems, where+": invalid id")
		} else if _, exists := caseIDs[demoCase.ID]; exists {
			problems = append(problems, where+": duplicate id "+demoCase.ID)
		} else {
			caseIDs[demoCase.ID] = struct{}{}
		}
		validateOwnedPath(root, demoCase.Dir, where+".dir", "case "+demoCase.ID, owners, &problems)
		seenProfiles := make(map[string]struct{}, len(demoCase.Profiles))
		for _, name := range demoCase.Profiles {
			if _, exists := profiles[name]; !exists {
				problems = append(problems, where+": unknown profile "+name)
			}
			if _, exists := seenProfiles[name]; exists {
				problems = append(problems, where+": duplicate profile "+name)
			}
			seenProfiles[name] = struct{}{}
		}
		if len(seenProfiles) == 0 {
			problems = append(problems, where+": at least one profile is required")
		}
		if _, model := seenProfiles["model"]; model && len(seenProfiles) != 1 {
			problems = append(problems, where+": model profile must be exclusive")
		}
		seenGOOS := make(map[string]struct{}, len(demoCase.GOOS))
		for _, goos := range demoCase.GOOS {
			if !validGOOS[goos] {
				problems = append(problems, where+": invalid goos "+goos)
			}
			if _, exists := seenGOOS[goos]; exists {
				problems = append(problems, where+": duplicate goos "+goos)
			}
			seenGOOS[goos] = struct{}{}
		}
		if len(seenGOOS) == 0 {
			problems = append(problems, where+": goos is required")
		}
	}

	workflows := make(map[string]struct{})
	for i, owned := range manifest.Workflow {
		where := fmt.Sprintf("workflow_owned[%d]", i)
		validateOwnedPath(root, owned.Dir, where+".dir", "workflow", owners, &problems)
		if strings.HasPrefix(owned.Workflow, "manual-") {
			workflows[owned.Workflow] = struct{}{}
		} else if err := validateRepoPath(owned.Workflow); err != nil {
			problems = append(problems, where+".workflow: "+err.Error())
		} else if info, err := os.Stat(filepath.Join(root, filepath.FromSlash(owned.Workflow))); err != nil || !info.Mode().IsRegular() {
			problems = append(problems, where+".workflow: not a regular file")
		} else {
			workflows[owned.Workflow] = struct{}{}
		}
	}
	for i, support := range manifest.Support {
		where := fmt.Sprintf("support[%d]", i)
		validateOwnedPath(root, support.Dir, where+".dir", "support", owners, &problems)
		if workflow, ok := strings.CutPrefix(support.Owner, "workflow:"); ok {
			if _, exists := workflows[workflow]; !exists {
				problems = append(problems, where+": unknown workflow owner "+workflow)
			}
		} else if _, exists := caseIDs[support.Owner]; !exists {
			problems = append(problems, where+": unknown case owner "+support.Owner)
		}
	}

	sourceDirs, err := findGoSourceDirectories(root)
	if err != nil {
		problems = append(problems, "source audit: "+err.Error())
	} else {
		for dir := range sourceDirs {
			if _, owned := owners[dir]; !owned {
				problems = append(problems, "source directory has no owner: "+dir)
			}
		}
		for dir, owner := range owners {
			if _, exists := sourceDirs[dir]; !exists {
				problems = append(problems, fmt.Sprintf("%s owns a directory without direct Go source: %s", owner, dir))
			}
		}
	}
	if len(problems) == 0 {
		return nil
	}
	sort.Strings(problems)
	return errors.New(strings.Join(problems, "\n"))
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
	if info, err := os.Stat(filepath.Join(root, filepath.FromSlash(path))); err != nil {
		*problems = append(*problems, where+": "+err.Error())
	} else if !info.IsDir() {
		*problems = append(*problems, where+": not a directory")
	}
}

func validateRepoPath(path string) error {
	if path == "" {
		return errors.New("path is required")
	}
	clean := filepath.ToSlash(filepath.Clean(filepath.FromSlash(path)))
	if filepath.IsAbs(path) || strings.Contains(path, "\\") || clean != path || path == "." || path == ".." || strings.HasPrefix(path, "../") {
		return errors.New("path must be a clean, slash-separated repository-relative path")
	}
	return nil
}

func findGoSourceDirectories(root string) (map[string]struct{}, error) {
	result := make(map[string]struct{})
	for _, base := range []string{"_demo/go", "_demo/c", "_demo/py", "_demo/embed", "_demo/workflow"} {
		err := filepath.WalkDir(filepath.Join(root, base), func(path string, entry fs.DirEntry, err error) error {
			if err != nil || entry.IsDir() || !strings.HasSuffix(entry.Name(), ".go") {
				return err
			}
			relative, err := filepath.Rel(root, filepath.Dir(path))
			if err == nil {
				result[filepath.ToSlash(relative)] = struct{}{}
			}
			return err
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

var validGOOS = map[string]bool{
	"aix": true, "android": true, "darwin": true, "dragonfly": true,
	"freebsd": true, "illumos": true, "ios": true, "js": true,
	"linux": true, "netbsd": true, "openbsd": true, "plan9": true,
	"solaris": true, "wasip1": true, "windows": true,
}
