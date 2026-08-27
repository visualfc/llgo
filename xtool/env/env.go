/*
 * Copyright (c) 2024 The XGo Authors (xgo.dev). All rights reserved.
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

package env

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"runtime"
	"strings"

	"github.com/xgo-dev/llgo/internal/quoted"
	"github.com/xgo-dev/llgo/xtool/safesplit"
)

var (
	reSubcmd = regexp.MustCompile(`\$\([^)]+\)`)
	reFlag   = regexp.MustCompile(`[^ \t\n]+`)
)

func ExpandEnvToArgs(s string) []string {
	r, config := expandEnvWithCmd(s, "", nil)
	return expandedArgs(r, config)
}

// ExpandEnvToArgsWith expands variables and supported helper commands using
// the supplied request directory and environment. A non-nil environ prevents
// subprocesses and variable expansion from consulting process-global state.
func ExpandEnvToArgsWith(s, dir string, environ []string) []string {
	r, config := expandEnvWithCmd(s, dir, environ)
	return expandedArgs(r, config)
}

func expandedArgs(r string, config bool) []string {
	if r == "" {
		return nil
	}
	if config {
		return safesplit.SplitPkgConfigFlags(r)
	}
	return []string{r}
}

func ExpandEnv(s string) string {
	r, _ := expandEnvWithCmd(s, "", nil)
	return r
}

func expandEnvWithCmd(s, dir string, environ []string) (string, bool) {
	var config bool
	expanded := reSubcmd.ReplaceAllStringFunc(s, func(m string) string {
		subcmd := strings.TrimSpace(m[2 : len(m)-1])
		args := parseSubcmd(subcmd)
		cmd := args[0]
		if cmd != "pkg-config" && cmd != "llvm-config" {
			fmt.Fprintf(os.Stderr, "expand cmd only support pkg-config and llvm-config: '%s'\n", subcmd)
			return ""
		}
		config = true

		var out []byte
		var err error
		executable := cmd
		if cmd == "pkg-config" {
			executable = PkgConfigCommand(dir, environ)
		} else if environ != nil {
			executable = lookPathInEnvironment(cmd, dir, environ)
		}
		command := exec.Command(executable, args[1:]...)
		command.Dir = dir
		if environ != nil {
			command.Env = append([]string(nil), environ...)
		}
		out, err = command.Output()

		if err != nil {
			// TODO(kindy): log in verbose mode
			return ""
		}

		return strings.Replace(strings.TrimSpace(string(out)), "\n", " ", -1)
	})
	lookup := os.Getenv
	if environ != nil {
		lookup = func(key string) string {
			prefix := key + "="
			for i := len(environ) - 1; i >= 0; i-- {
				if strings.HasPrefix(environ[i], prefix) {
					return strings.TrimPrefix(environ[i], prefix)
				}
			}
			return ""
		}
	}
	return strings.TrimSpace(os.Expand(expanded, lookup)), config
}

// PkgConfigCommand returns the pkg-config executable selected by PKG_CONFIG.
// Like the Go command, it parses the setting with cmd/internal/quoted rules and
// uses its first field as the executable name.
func PkgConfigCommand(dir string, environ []string) string {
	value := environmentValue("PKG_CONFIG", environ)
	if value == "" {
		value = "pkg-config"
	}
	args, err := quoted.Split(value)
	if err != nil {
		panic(fmt.Sprintf("could not parse environment variable PKG_CONFIG with value %q: %v", value, err))
	}
	if len(args) == 0 {
		return "pkg-config"
	}
	if environ != nil {
		return lookPathInEnvironment(args[0], dir, environ)
	}
	return args[0]
}

func environmentValue(name string, environ []string) string {
	if environ == nil {
		return os.Getenv(name)
	}
	for i := len(environ) - 1; i >= 0; i-- {
		key, value, ok := strings.Cut(environ[i], "=")
		if ok && (key == name || runtime.GOOS == "windows" && strings.EqualFold(key, name)) {
			return value
		}
	}
	return ""
}

func lookPathInEnvironment(name, dir string, environ []string) string {
	if strings.ContainsRune(name, filepath.Separator) {
		return name
	}
	extensions := windowsExecutableExtensions(name, environ)
	path := ""
	prefix := "PATH="
	for i := len(environ) - 1; i >= 0; i-- {
		if strings.HasPrefix(environ[i], prefix) {
			path = strings.TrimPrefix(environ[i], prefix)
			break
		}
	}
	for _, entry := range filepath.SplitList(path) {
		if entry == "" {
			entry = "."
		}
		if !filepath.IsAbs(entry) && dir != "" {
			entry = filepath.Join(dir, entry)
		}
		candidate := filepath.Join(entry, name)
		if extensions == nil {
			if isExecutableFile(candidate) {
				return candidate
			}
			continue
		}
		for _, extension := range extensions {
			candidateWithExtension := candidate + extension
			if isExecutableFile(candidateWithExtension) {
				return candidateWithExtension
			}
		}
	}
	return name
}

func windowsExecutableExtensions(name string, environ []string) []string {
	if runtime.GOOS != "windows" || filepath.Ext(name) != "" {
		return nil
	}
	extensions := ".COM;.EXE;.BAT;.CMD"
	for i := len(environ) - 1; i >= 0; i-- {
		key, value, ok := strings.Cut(environ[i], "=")
		if ok && strings.EqualFold(key, "PATHEXT") {
			extensions = value
			break
		}
	}
	exts := make([]string, 0, 4)
	for _, ext := range filepath.SplitList(extensions) {
		if ext != "" {
			exts = append(exts, ext)
		}
	}
	return exts
}

func isExecutableFile(path string) bool {
	info, err := os.Stat(path)
	return err == nil && !info.IsDir() && (runtime.GOOS == "windows" || info.Mode()&0o111 != 0)
}

func parseSubcmd(s string) []string {
	return reFlag.FindAllString(s, -1)
}
