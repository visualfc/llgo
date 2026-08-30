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

package gotest

import (
	"errors"
	"reflect"
	"testing"
)

func recoverThenDeferPanic(events *[]string) {
	if recovered := recover(); recovered != nil {
		defer panic(recovered)
		*events = append(*events, "recovered")
	}
	*events = append(*events, "returning")
}

func TestRecoverThenDeferredPanic(t *testing.T) {
	sentinel := errors.New("panic in test")
	var events []string
	func() {
		defer func() {
			if recovered := recover(); recovered != sentinel {
				t.Fatalf("outer recover = %v, want %v", recovered, sentinel)
			}
			events = append(events, "repanicked")
		}()
		func() {
			defer recoverThenDeferPanic(&events)
			panic(sentinel)
		}()
		t.Fatal("re-panic unexpectedly returned")
	}()

	want := []string{"recovered", "returning", "repanicked"}
	if !reflect.DeepEqual(events, want) {
		t.Fatalf("defer events = %v, want %v", events, want)
	}
}
