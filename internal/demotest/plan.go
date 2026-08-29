package demotest

import (
	"errors"
	"fmt"
	"strings"
)

type PlannedCase struct {
	Case    Case
	Profile Profile
}

func Plan(manifest *Manifest, profileName, goos string) ([]PlannedCase, error) {
	var profile *Profile
	for i := range manifest.Profiles {
		if manifest.Profiles[i].Name == profileName {
			profile = &manifest.Profiles[i]
			break
		}
	}
	if profile == nil {
		return nil, fmt.Errorf("unknown profile %q", profileName)
	}
	plan := make([]PlannedCase, 0, len(manifest.Cases))
	for _, demoCase := range manifest.Cases {
		if contains(demoCase.Profiles, profileName) && contains(demoCase.GOOS, goos) {
			plan = append(plan, PlannedCase{Case: demoCase, Profile: *profile})
		}
	}
	if len(plan) == 0 {
		return nil, fmt.Errorf("profile %q has no cases for GOOS %q", profileName, goos)
	}
	return plan, nil
}

func (planned PlannedCase) LLGOArguments() []string {
	args := append([]string{"run"}, planned.Profile.LLGOArgs...)
	if planned.Profile.Target != "" {
		args = append(args, "-target="+planned.Profile.Target)
	}
	if planned.Profile.Emulator {
		args = append(args, "-emulator")
	}
	return append(args, ".")
}

// FilterPlan selects exact IDs or directories while preserving manifest order.
func FilterPlan(plan []PlannedCase, selectors []string) ([]PlannedCase, error) {
	if len(selectors) == 0 {
		return plan, nil
	}
	matches := make(map[string]int, len(plan)*2)
	ambiguous := make(map[string]bool)
	for i, planned := range plan {
		for _, key := range []string{planned.Case.ID, planned.Case.Dir} {
			if previous, exists := matches[key]; exists && previous != i {
				ambiguous[key] = true
			} else {
				matches[key] = i
			}
		}
	}
	selected := make(map[int]string, len(selectors))
	var problems []string
	for _, selector := range selectors {
		index, exists := matches[selector]
		switch {
		case ambiguous[selector]:
			problems = append(problems, fmt.Sprintf("case selector %q is ambiguous", selector))
		case !exists:
			problems = append(problems, fmt.Sprintf("case selector %q is not in the plan", selector))
		case selected[index] != "":
			problems = append(problems, fmt.Sprintf("case selectors %q and %q select the same case", selected[index], selector))
		default:
			selected[index] = selector
		}
	}
	if len(problems) != 0 {
		return nil, errors.New(strings.Join(problems, "\n"))
	}
	filtered := make([]PlannedCase, 0, len(selected))
	for i, planned := range plan {
		if selected[i] != "" {
			filtered = append(filtered, planned)
		}
	}
	return filtered, nil
}

func contains(values []string, value string) bool {
	for _, candidate := range values {
		if candidate == value {
			return true
		}
	}
	return false
}
