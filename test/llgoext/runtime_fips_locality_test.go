//go:build llgo && go1.26 && !baremetal

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

package llgoext

import (
	"testing"
	_ "unsafe"
)

//go:linkname runtimeFIPSSetBypass crypto/fips140.setBypass
func runtimeFIPSSetBypass()

//go:linkname runtimeFIPSUnsetBypass crypto/fips140.unsetBypass
func runtimeFIPSUnsetBypass()

//go:linkname runtimeFIPSIsBypassed crypto/fips140.isBypassed
func runtimeFIPSIsBypassed() bool

type runtimeFIPSBypassResult struct {
	id     int
	before bool
	set    bool
	unset  bool
}

func TestRuntimeFIPSBypassGLSIsolation(t *testing.T) {
	if runtimeFIPSIsBypassed() {
		t.Fatal("main goroutine unexpectedly started with FIPS bypass enabled")
	}
	runtimeFIPSSetBypass()
	runtimeFIPSSetBypass()
	defer func() {
		runtimeFIPSUnsetBypass()
		runtimeFIPSUnsetBypass()
	}()

	ready := make(chan struct{}, 2)
	release := make(chan struct{})
	results := make(chan runtimeFIPSBypassResult, 2)
	for id := 0; id < 2; id++ {
		go func(id int) {
			result := runtimeFIPSBypassResult{id: id, before: runtimeFIPSIsBypassed()}
			runtimeFIPSSetBypass()
			result.set = runtimeFIPSIsBypassed()
			ready <- struct{}{}
			<-release
			runtimeFIPSUnsetBypass()
			result.unset = runtimeFIPSIsBypassed()
			results <- result
		}(id)
	}
	<-ready
	<-ready

	if !runtimeFIPSIsBypassed() {
		t.Fatal("child goroutines changed main FIPS bypass state")
	}
	close(release)
	for range 2 {
		result := <-results
		if result.before || !result.set || result.unset {
			t.Fatalf("child %d FIPS bypass states = before:%v set:%v unset:%v", result.id, result.before, result.set, result.unset)
		}
	}

	runtimeFIPSUnsetBypass()
	if !runtimeFIPSIsBypassed() {
		t.Fatal("nested main FIPS bypass cleared too early")
	}
	runtimeFIPSUnsetBypass()
	if runtimeFIPSIsBypassed() {
		t.Fatal("main FIPS bypass remained enabled")
	}
}
