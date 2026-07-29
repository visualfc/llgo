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

package ssa

import (
	"fmt"
	"go/types"
	"sort"
	"strings"
	"sync"

	"github.com/goplus/llgo/internal/locality"
	localitylayout "github.com/goplus/llgo/internal/locality/layout"
)

type Locality = locality.Kind

const (
	LocalityNone   = locality.None
	ThreadLocal    = locality.Thread
	GoroutineLocal = locality.Goroutine
)

type LocalityInfo = locality.Info
type LocalStorage = localitylayout.Storage

const (
	LocalStorageUnknown   = localitylayout.StorageUnknown
	LocalStorageNativeTLS = localitylayout.StorageNativeTLS
	LocalStoragePackage   = localitylayout.StoragePackage
)

// VariableLocality is the locality metadata attached to one package variable.
type VariableLocality struct {
	LocalStorage LocalStorage
	locality.Info
}

type localityInfos struct {
	mu             sync.RWMutex
	entries        map[string]VariableLocality
	parsedPackages map[*types.Package]struct{}
}

func newLocalityInfos() *localityInfos {
	return &localityInfos{
		entries:        make(map[string]VariableLocality),
		parsedPackages: make(map[*types.Package]struct{}),
	}
}

func (p *localityInfos) update(name string, update func(*VariableLocality)) {
	p.mu.Lock()
	info := p.entries[name]
	update(&info)
	p.entries[name] = info
	p.mu.Unlock()
}

func (p Program) SetLocalityInfo(name string, info LocalityInfo) {
	p.localities.update(name, func(current *VariableLocality) { current.Info = info })
}

func (p Program) SetLocalStorage(name string, storage LocalStorage) {
	p.localities.update(name, func(info *VariableLocality) { info.LocalStorage = storage })
}

func (p Program) VariableLocality(name string) (VariableLocality, bool) {
	p.localities.mu.RLock()
	info, ok := p.localities.entries[name]
	p.localities.mu.RUnlock()
	return info, ok
}

// ResolveLocality returns locality metadata for one declaration. Locality
// variables cannot participate in go:linkname alias chains.
func (p Program) ResolveLocality(name string) (string, VariableLocality, bool, error) {
	lookup := func(name string) (VariableLocality, bool) {
		p.localities.mu.RLock()
		info, ok := p.localities.entries[name]
		p.localities.mu.RUnlock()
		return info, ok
	}
	return resolveLocality(lookup, p.Linkname, name)
}

func resolveLocality(lookup func(string) (VariableLocality, bool), linkname func(string) (string, bool), name string) (string, VariableLocality, bool, error) {
	result, ok := lookup(name)
	if !ok {
		result = VariableLocality{}
	}
	var seen map[string]bool
	current := name
	for {
		target, hasLink := linkname(current)
		target = strings.TrimPrefix(target, "go:")
		if !hasLink || target == "" {
			return current, result, ok, nil
		}
		if seen == nil {
			seen = make(map[string]bool)
		}
		if seen[current] {
			return "", VariableLocality{}, false, fmt.Errorf("declaration linkname cycle involving %s", current)
		}
		seen[current] = true
		if currentInfo, exists := lookup(current); exists && currentInfo.Locality != locality.None {
			return "", VariableLocality{}, false, fmt.Errorf("local variable %s cannot use go:linkname", current)
		}
		if targetInfo, exists := lookup(target); exists && targetInfo.Locality != locality.None {
			return "", VariableLocality{}, false, fmt.Errorf("go:linkname alias %s cannot reference local variable %s", name, target)
		}
		if target == current {
			return current, result, ok, nil
		}
		current = target
	}
}

func hasInitialization(info locality.Info) bool {
	return info.HasInitializer || info.InitFunc != "" || info.InitOrder != 0
}

func (p Program) ValidateLocalities(pkgPath string) error {
	prefix := pkgPath + "."
	p.localities.mu.RLock()
	if len(p.localities.entries) == 0 {
		p.localities.mu.RUnlock()
		return nil
	}
	nameSet := make(map[string]bool)
	localNames := make(map[string]bool)
	for name, info := range p.localities.entries {
		if info.Locality != locality.None {
			localNames[name] = true
		}
		if strings.HasPrefix(name, prefix) {
			nameSet[name] = true
		}
	}
	p.localities.mu.RUnlock()
	if len(localNames) == 0 {
		return nil
	}
	p.linknameMu.RLock()
	links := make(map[string]string, len(p.linkname))
	for name, target := range p.linkname {
		links[name] = strings.TrimPrefix(target, "go:")
	}
	p.linknameMu.RUnlock()
	for name := range links {
		if strings.HasPrefix(name, prefix) && linknameReachesLocal(name, links, localNames) {
			nameSet[name] = true
		}
	}
	names := make([]string, 0, len(nameSet))
	for name := range nameSet {
		names = append(names, name)
	}
	sort.Strings(names)
	for _, name := range names {
		if _, _, _, err := p.ResolveLocality(name); err != nil {
			return err
		}
	}
	return nil
}

func linknameReachesLocal(name string, links map[string]string, localNames map[string]bool) bool {
	seen := make(map[string]bool)
	for name != "" && !seen[name] {
		if localNames[name] {
			return true
		}
		seen[name] = true
		name = links[name]
	}
	return false
}

func (p Program) PackageSyntaxParsed(pkg *types.Package) bool {
	p.localities.mu.RLock()
	_, ok := p.localities.parsedPackages[pkg]
	p.localities.mu.RUnlock()
	return ok
}

func (p Program) MarkPackageSyntaxParsed(pkg *types.Package) {
	p.localities.mu.Lock()
	p.localities.parsedPackages[pkg] = struct{}{}
	p.localities.mu.Unlock()
}

func (p Program) PackageLocalities(pkgPath string) map[string]VariableLocality {
	prefix := pkgPath + "."
	ret := make(map[string]VariableLocality)
	p.localities.mu.RLock()
	for name, info := range p.localities.entries {
		if info.Locality != locality.None && strings.HasPrefix(name, prefix) {
			ret[name] = info
		}
	}
	p.localities.mu.RUnlock()
	return ret
}

func (p Program) NeedsLocalContext() bool {
	p.localities.mu.RLock()
	defer p.localities.mu.RUnlock()
	for _, info := range p.localities.entries {
		if info.Locality != locality.None && (info.LocalStorage != LocalStorageNativeTLS || hasInitialization(info.Info)) {
			return true
		}
	}
	return false
}
