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

package gotest

import (
	"github.com/xgo-dev/llgo/test/go/testdata/stringconversion"
	"testing"
)

func checkConvertedRune(t *testing.T, name string, got []rune, want rune) {
	t.Helper()
	if len(got) != 1 || got[0] != want {
		t.Fatalf("%s = %U, want [%U]", name, got, want)
	}
}

func TestStringConversionFromWideIntegers(t *testing.T) {
	checkConvertedRune(t, "int64", stringconversion.RunesFromInt64(0x1F642), '\U0001F642')
	checkConvertedRune(t, "int64-out-of-range", stringconversion.RunesFromInt64(0x110000), '\uFFFD')
	checkConvertedRune(t, "uint64", stringconversion.RunesFromUint64(0x1F642), '\U0001F642')
	checkConvertedRune(t, "uint64-out-of-range", stringconversion.RunesFromUint64(0x110000), '\uFFFD')
}

func checkConvertedBytes(t *testing.T, s []byte, name string) {
	t.Helper()
	if len(s) != 0 {
		t.Fatalf("len(%s) = %d, want 0", name, len(s))
	}
	if s == nil {
		t.Fatalf("%s is nil", name)
	}
}

func checkConvertedRunes(t *testing.T, s []rune, name string) {
	t.Helper()
	if len(s) != 0 {
		t.Fatalf("len(%s) = %d, want 0", name, len(s))
	}
	if s == nil {
		t.Fatalf("%s is nil", name)
	}
}

func TestEmptyStringToByteRuneSlicesNonNil(t *testing.T) {
	checkConvertedBytes(t, stringconversion.BytesFromString(), "[]byte(\"\")")
	checkConvertedBytes(t, stringconversion.BytesFromNamedString(), "[]byte(mystring(\"\"))")
	checkConvertedBytes(t, stringconversion.NamedBytesFromString(), "mybytes(\"\")")
	checkConvertedBytes(t, stringconversion.NamedBytesFromNamedString(), "mybytes(mystring(\"\"))")

	checkConvertedRunes(t, stringconversion.RunesFromString(), "[]rune(\"\")")
	checkConvertedRunes(t, stringconversion.RunesFromNamedString(), "[]rune(mystring(\"\"))")
	checkConvertedRunes(t, stringconversion.NamedRunesFromString(), "myrunes(\"\")")
	checkConvertedRunes(t, stringconversion.NamedRunesFromNamedString(), "myrunes(mystring(\"\"))")
}
