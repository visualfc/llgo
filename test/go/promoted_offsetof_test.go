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
	"reflect"
	"testing"
	"unsafe"
)

type promotedEmbedded struct {
	B int
}

type promotedGenericOuter[K any] struct {
	A K
	promotedEmbedded
}

func promotedOffsets[K any](v *promotedGenericOuter[K]) (uintptr, uintptr) {
	return unsafe.Offsetof(v.B), unsafe.Offsetof(v.promotedEmbedded)
}

func TestUnsafeOffsetofGenericPromotedFieldIssue53137(t *testing.T) {
	got, want := promotedOffsets(new(promotedGenericOuter[int]))
	if got != want {
		t.Fatalf("unsafe.Offsetof(v.B) = %d, want embedded field offset %d", got, want)
	}
}

func offsetofGenericCompositeField[T int](v T) uintptr {
	return unsafe.Offsetof(struct {
		Prefix byte
		Value  T
	}{0, func(T) T { return v }(v)}.Value)
}

func TestUnsafeOffsetofGenericCompositeFieldMatchesGoLayout(t *testing.T) {
	typ := reflect.TypeOf(struct {
		Prefix byte
		Value  int
	}{})
	field, ok := typ.FieldByName("Value")
	if !ok {
		t.Fatal("reflect.Type.FieldByName(Value) failed")
	}
	if got, want := offsetofGenericCompositeField(1), field.Offset; got != want {
		t.Fatalf("unsafe.Offsetof(composite.Value) = %d, want Go layout offset %d", got, want)
	}
}
