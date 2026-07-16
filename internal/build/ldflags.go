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
	"fmt"
	"strconv"
	"strings"
)

// optionalBool records both a boolean value and whether it was explicitly set.
type optionalBool struct {
	value bool
	set   bool
}

// linkFlagOptions contains the Go linker options that LLGo currently handles.
// ignored retains the remaining linker arguments so that parsing them can be
// added incrementally without silently losing their order.
type linkFlagOptions struct {
	stripSymbols bool
	omitDWARF    optionalBool
	ignored      []string
}

// EffectiveOmitDWARF reports the effective -w setting. As in cmd/link, an
// explicit -w value takes precedence; otherwise -s implies -w.
func (o linkFlagOptions) EffectiveOmitDWARF() bool {
	if o.omitDWARF.set {
		return o.omitDWARF.value
	}
	return o.stripSymbols
}

// parseGoLinkFlags extracts the currently supported flags from Go build flags.
// Repeated linker flags are evaluated in command-line order. Each supported
// -ldflags value replaces the previous complete argument list, matching Go's
// last-match-wins behavior for a command-line package. Package-specific
// patterns are rejected until LLGo can select flags separately for each root;
// an unpatterned value and the universal all= pattern are supported.
func parseGoLinkFlags(buildFlags []string) (linkFlagOptions, error) {
	var opts linkFlagOptions
	for _, buildFlag := range buildFlags {
		if buildFlag == "-ldflags" {
			return linkFlagOptions{}, fmt.Errorf("-ldflags requires a value")
		}

		value, ok := strings.CutPrefix(buildFlag, "-ldflags=")
		if !ok {
			continue
		}
		opts = linkFlagOptions{}

		linkFlags, err := splitPerPackageLinkFlags(value)
		if err != nil {
			return linkFlagOptions{}, fmt.Errorf("invalid -ldflags value %q: %w", value, err)
		}
		for _, linkFlag := range linkFlags {
			switch {
			case linkFlag == "-s" || linkFlag == "--s":
				opts.stripSymbols = true
			case hasLinkFlagBoolValue(linkFlag, "s"):
				v, err := parseLinkFlagBool("-s", linkFlagBoolValue(linkFlag, "s"))
				if err != nil {
					return linkFlagOptions{}, err
				}
				opts.stripSymbols = v
			case linkFlag == "-w" || linkFlag == "--w":
				opts.omitDWARF = optionalBool{value: true, set: true}
			case hasLinkFlagBoolValue(linkFlag, "w"):
				v, err := parseLinkFlagBool("-w", linkFlagBoolValue(linkFlag, "w"))
				if err != nil {
					return linkFlagOptions{}, err
				}
				opts.omitDWARF = optionalBool{value: v, set: true}
			default:
				opts.ignored = append(opts.ignored, linkFlag)
			}
		}
	}
	return opts, nil
}

func hasLinkFlagBoolValue(arg, name string) bool {
	return strings.HasPrefix(arg, "-"+name+"=") || strings.HasPrefix(arg, "--"+name+"=")
}

func linkFlagBoolValue(arg, name string) string {
	arg = strings.TrimPrefix(arg, "-")
	arg = strings.TrimPrefix(arg, "-")
	return strings.TrimPrefix(arg, name+"=")
}

func parseLinkFlagBool(name, value string) (bool, error) {
	if value == "" {
		return false, fmt.Errorf("%s requires a boolean value", name)
	}
	v, err := strconv.ParseBool(value)
	if err != nil {
		return false, fmt.Errorf("invalid boolean value %q for %s: %w", value, name, err)
	}
	return v, nil
}

// splitPerPackageLinkFlags implements the currently supported subset of Go's
// per-package build flag syntax: [pattern=]arg-list. LLGo can safely evaluate
// unpatterned values and all= before package loading. Other patterns require
// per-root matching and are rejected instead of being applied globally.
func splitPerPackageLinkFlags(value string) ([]string, error) {
	value = strings.TrimSpace(value)
	if value == "" {
		return nil, nil
	}
	if value[0] != '-' {
		if value[0] == '\'' || value[0] == '"' {
			return nil, fmt.Errorf("parameter may not start with quote character %c", value[0])
		}
		i := strings.IndexByte(value, '=')
		if i < 0 {
			return nil, fmt.Errorf("missing =<value> in <pattern>=<value>")
		}
		if i == 0 {
			return nil, fmt.Errorf("missing <pattern> in <pattern>=<value>")
		}
		pattern := strings.TrimSpace(value[:i])
		if pattern != "all" {
			return nil, fmt.Errorf("package pattern %q is not supported yet; use an unpatterned value or all=", pattern)
		}
		value = value[i+1:]
	}
	return splitQuotedFields(value)
}

// splitQuotedFields follows the quoting rules used by the Go command for
// -ldflags: single or double quotes may surround one argument, with no escape
// processing inside the quoted text.
func splitQuotedFields(value string) ([]string, error) {
	var fields []string
	for len(value) > 0 {
		value = strings.TrimLeft(value, " \t\n\r")
		if value == "" {
			break
		}
		if value[0] == '\'' || value[0] == '"' {
			quote := value[0]
			value = value[1:]
			i := strings.IndexByte(value, quote)
			if i < 0 {
				return nil, fmt.Errorf("unterminated %c string", quote)
			}
			fields = append(fields, value[:i])
			value = value[i+1:]
			continue
		}

		i := strings.IndexAny(value, " \t\n\r")
		if i < 0 {
			fields = append(fields, value)
			break
		}
		fields = append(fields, value[:i])
		value = value[i:]
	}
	return fields, nil
}
