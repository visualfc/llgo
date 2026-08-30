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

package stringconversion

type namedString string
type namedBytes []byte
type namedRunes []rune

func RunesFromInt64(v int64) []rune {
	var out []rune
	for _, r := range string(v) {
		out = append(out, r)
	}
	return out
}

func RunesFromUint64(v uint64) []rune {
	var out []rune
	for _, r := range string(v) {
		out = append(out, r)
	}
	return out
}

func BytesFromString() []byte {
	return []byte("")
}

func BytesFromNamedString() []byte {
	return []byte(namedString(""))
}

func NamedBytesFromString() namedBytes {
	return namedBytes("")
}

func NamedBytesFromNamedString() namedBytes {
	return namedBytes(namedString(""))
}

func RunesFromString() []rune {
	return []rune("")
}

func RunesFromNamedString() []rune {
	return []rune(namedString(""))
}

func NamedRunesFromString() namedRunes {
	return namedRunes("")
}

func NamedRunesFromNamedString() namedRunes {
	return namedRunes(namedString(""))
}
