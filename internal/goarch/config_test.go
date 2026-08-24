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

package goarch

import "testing"

func TestResolve386(t *testing.T) {
	for _, test := range []struct {
		value   string
		want    string
		wantErr bool
	}{
		{value: "", want: "sse2"},
		{value: "sse2", want: "sse2"},
		{value: "softfloat", want: "softfloat"},
		{value: "387", want: "sse2", wantErr: true},
		{value: "invalid", want: "sse2", wantErr: true},
	} {
		got, err := Resolve386(test.value)
		if got != test.want || (err != nil) != test.wantErr {
			t.Errorf("Resolve386(%q) = %q, %v; want %q, error=%v", test.value, got, err, test.want, test.wantErr)
		}
	}
	if _, err := Resolve386("387"); err == nil || err.Error() != "unsupported setting GO386=387. Consider using GO386=softfloat instead." {
		t.Fatalf("Resolve386(387) error = %v", err)
	}
}

func TestResolveAMD64(t *testing.T) {
	for _, test := range []struct {
		value   string
		want    string
		wantErr bool
	}{
		{value: "", want: "v1"},
		{value: "v1", want: "v1"},
		{value: "v2", want: "v2"},
		{value: "v3", want: "v3"},
		{value: "v4", want: "v4"},
		{value: "v5", want: "v1", wantErr: true},
	} {
		got, err := ResolveAMD64(test.value)
		if got != test.want || (err != nil) != test.wantErr {
			t.Errorf("ResolveAMD64(%q) = %q, %v; want %q, error=%v", test.value, got, err, test.want, test.wantErr)
		}
	}
}

func TestParseARM(t *testing.T) {
	for _, test := range []struct {
		value    string
		want     string
		wantSoft bool
		wantErr  bool
	}{
		{value: "", want: "7,hardfloat"},
		{value: "5", want: "5,softfloat", wantSoft: true},
		{value: "5,softfloat", want: "5,softfloat", wantSoft: true},
		{value: "5,hardfloat", want: "5,hardfloat"},
		{value: "6", want: "6,hardfloat"},
		{value: "6,softfloat", want: "6,softfloat", wantSoft: true},
		{value: "7,hardfloat", want: "7,hardfloat"},
		{value: "4", want: "7,hardfloat", wantErr: true},
		{value: "7,softfloat,hardfloat", want: "7,hardfloat", wantErr: true},
	} {
		got, err := ParseARM(test.value)
		if got.String() != test.want || got.SoftFloat != test.wantSoft || (err != nil) != test.wantErr {
			t.Errorf("ParseARM(%q) = %q (soft=%v), %v; want %q (soft=%v), error=%v", test.value, got.String(), got.SoftFloat, err, test.want, test.wantSoft, test.wantErr)
		}
	}
}

func TestParseARM64(t *testing.T) {
	for _, test := range []struct {
		value   string
		want    string
		wantErr bool
	}{
		{value: "", want: "v8.0"},
		{value: "v8.0", want: "v8.0"},
		{value: "v8.0,lse", want: "v8.0,lse"},
		{value: "v8.0,crypto", want: "v8.0,crypto"},
		{value: "v8.0,crypto,lse", want: "v8.0,lse,crypto"},
		{value: "v8.0,lse,crypto", want: "v8.0,lse,crypto"},
		{value: "v8.0,lse,lse,crypto", want: "v8.0,lse,crypto"},
		{value: "v8.1", want: "v8.1,lse"},
		{value: "v8.9", want: "v8.9,lse"},
		{value: "v9.0", want: "v9.0,lse"},
		{value: "v9.5,crypto", want: "v9.5,lse,crypto"},
		{value: "v9.6", want: "v8.0", wantErr: true},
		{value: "8.0", want: "v8.0", wantErr: true},
	} {
		got, err := ParseARM64(test.value)
		if got.String() != test.want || (err != nil) != test.wantErr {
			t.Errorf("ParseARM64(%q) = %q, %v; want %q, error=%v", test.value, got.String(), err, test.want, test.wantErr)
		}
	}
}
