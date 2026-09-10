//go:build llgo

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

	l "github.com/xgo-dev/llgo/runtime/_test/locality"
)

func TestTLSAndGLSIsolation(t *testing.T)              { l.TestTLSAndGLSIsolation(t) }
func TestLocalPackageDirectCaches(t *testing.T)        { l.TestLocalPackageDirectCaches(t) }
func TestPanickingInitializerIsSticky(t *testing.T)    { l.TestPanickingInitializerIsSticky(t) }
func TestNilPanickingInitializerIsSticky(t *testing.T) { l.TestNilPanickingInitializerIsSticky(t) }
func TestRecursiveInitializerObservesPartialValue(t *testing.T) {
	l.TestRecursiveInitializerObservesPartialValue(t)
}
func TestLateSortedInitializer(t *testing.T) { l.TestLateSortedInitializer(t) }
func TestLocalPointerIsGCRoot(t *testing.T)  { l.TestLocalPointerIsGCRoot(t) }
func TestLocalContextCleanupAfterThreadExit(t *testing.T) {
	l.TestLocalContextCleanupAfterThreadExit(t)
}
func TestLocalAddressAndAtomicSemantics(t *testing.T) { l.TestLocalAddressAndAtomicSemantics(t) }
func TestClosureUsesInvocationContext(t *testing.T)   { l.TestClosureUsesInvocationContext(t) }
func TestEscapedPackageBlockAddressSurvivesOwnerExit(t *testing.T) {
	l.TestEscapedPackageBlockAddressSurvivesOwnerExit(t)
}
func TestEscapedPackageBlockAddressSurvivesGoexit(t *testing.T) {
	l.TestEscapedPackageBlockAddressSurvivesGoexit(t)
}
func TestZeroSizedNativeLocalAddressIsStable(t *testing.T) {
	l.TestZeroSizedNativeLocalAddressIsStable(t)
}
func TestInitializerScopeRunsOncePerPackageKind(t *testing.T) {
	l.TestInitializerScopeRunsOncePerPackageKind(t)
}
func TestMultiValueInitializerUsesOneGroup(t *testing.T) { l.TestMultiValueInitializerUsesOneGroup(t) }
func TestCrossPackageMixedInitializerGroup(t *testing.T) { l.TestCrossPackageMixedInitializerGroup(t) }
func TestComparableLocalityReads(t *testing.T)           { l.TestComparableLocalityReads(t) }

func BenchmarkOrdinaryGlobal(b *testing.B)               { l.BenchmarkOrdinaryGlobal(b) }
func BenchmarkNativeTLS(b *testing.B)                    { l.BenchmarkNativeTLS(b) }
func BenchmarkNativeGLS(b *testing.B)                    { l.BenchmarkNativeGLS(b) }
func BenchmarkTLSPackageBlock(b *testing.B)              { l.BenchmarkTLSPackageBlock(b) }
func BenchmarkGLSPackageBlock(b *testing.B)              { l.BenchmarkGLSPackageBlock(b) }
func BenchmarkComparableOrdinaryGlobalRead(b *testing.B) { l.BenchmarkComparableOrdinaryGlobalRead(b) }
func BenchmarkComparableNativeTLSRead(b *testing.B)      { l.BenchmarkComparableNativeTLSRead(b) }
func BenchmarkComparableGLSPackageRead(b *testing.B)     { l.BenchmarkComparableGLSPackageRead(b) }
func BenchmarkAlternatingGLSPackageRead(b *testing.B)    { l.BenchmarkAlternatingGLSPackageRead(b) }
func BenchmarkGoroutineEntry(b *testing.B)               { l.BenchmarkGoroutineEntry(b) }
func BenchmarkGoroutinePackageBlockFirstTouch(b *testing.B) {
	l.BenchmarkGoroutinePackageBlockFirstTouch(b)
}
