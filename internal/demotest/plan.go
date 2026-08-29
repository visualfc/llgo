package demotest

import (
	"errors"
	"fmt"
	"time"
)

const defaultCaseTimeout = 15 * time.Minute

type PlannedCase struct {
	Case    Case
	Profile Profile
	Timeout time.Duration
}

func Plan(manifest *Manifest, profileName, goos string) ([]PlannedCase, error) {
	var profile Profile
	found := false
	for _, candidate := range manifest.Profiles {
		if candidate.Name == profileName {
			profile = candidate
			found = true
			break
		}
	}
	if !found {
		return nil, fmt.Errorf("unknown profile %q", profileName)
	}

	plan := make([]PlannedCase, 0, len(manifest.Cases))
	for _, c := range manifest.Cases {
		if !contains(c.Profiles, profileName) || !contains(c.GOOS, goos) {
			continue
		}
		timeout := defaultCaseTimeout
		if c.Timeout != "" {
			var err error
			timeout, err = time.ParseDuration(c.Timeout)
			if err != nil {
				return nil, fmt.Errorf("case %s timeout: %w", c.ID, err)
			}
		}
		plan = append(plan, PlannedCase{Case: c, Profile: profile, Timeout: timeout})
	}
	if len(plan) == 0 {
		return nil, fmt.Errorf("profile %q has no cases for GOOS %q", profileName, goos)
	}
	return plan, nil
}

func (planned PlannedCase) LLGOArguments() []string {
	args := []string{"run"}
	args = append(args, planned.Profile.LLGOArgs...)
	if planned.Profile.Target != "" {
		args = append(args, "-target="+planned.Profile.Target)
	}
	if planned.Profile.Emulator {
		args = append(args, "-emulator")
	}
	return append(args, ".")
}

// FilterPlan selects cases by exact ID or repository-relative directory while
// preserving manifest order. Every selector must identify one planned case.
func FilterPlan(plan []PlannedCase, selectors []string) ([]PlannedCase, error) {
	if len(selectors) == 0 {
		return plan, nil
	}
	selected := make(map[int]string, len(selectors))
	var problems []string
	for _, selector := range selectors {
		match := -1
		for i, planned := range plan {
			if planned.Case.ID == selector || planned.Case.Dir == selector {
				if match >= 0 {
					problems = append(problems, fmt.Sprintf("case selector %q is ambiguous", selector))
					match = -2
					break
				}
				match = i
			}
		}
		if match == -1 {
			problems = append(problems, fmt.Sprintf("case selector %q is not in the plan", selector))
			continue
		}
		if match < 0 {
			continue
		}
		if previous, exists := selected[match]; exists {
			problems = append(problems, fmt.Sprintf("case selectors %q and %q select the same case", previous, selector))
			continue
		}
		selected[match] = selector
	}
	if len(problems) != 0 {
		return nil, errors.New(joinProblems(problems))
	}
	filtered := make([]PlannedCase, 0, len(selected))
	for i, planned := range plan {
		if _, ok := selected[i]; ok {
			filtered = append(filtered, planned)
		}
	}
	return filtered, nil
}

func joinProblems(problems []string) string {
	result := ""
	for i, problem := range problems {
		if i != 0 {
			result += "\n"
		}
		result += problem
	}
	return result
}

func contains(values []string, value string) bool {
	for _, candidate := range values {
		if candidate == value {
			return true
		}
	}
	return false
}
