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

package localityscope

var firstCalls int
var secondCalls int

func initFirst() int {
	firstCalls++
	return 100 + firstCalls
}

func initSecond() int {
	secondCalls++
	return 200 + secondCalls
}

//llgo:gls
var first = initFirst()

//llgo:gls
var second = initSecond()

func FirstCalls() int  { return firstCalls }
func SecondCalls() int { return secondCalls }
func First() int       { return first }
func Second() int      { return second }

func SetFirst(value int) { first = value }

func IncrementFirst() int {
	first++
	return first
}

var pairCalls int

func initPair() (int, int) {
	pairCalls++
	return 300 + pairCalls, 400 + pairCalls
}

//llgo:gls
var pairFirst, pairSecond = initPair()

func PairCalls() int { return pairCalls }

func PairFirst() int  { return pairFirst }
func PairSecond() int { return pairSecond }

var mixedCalls int
var mixedBacking = 500

func initMixed() (int, *int) {
	mixedCalls++
	return 600 + mixedCalls, &mixedBacking
}

//llgo:tls
var mixedScalar, mixedPointer = initMixed()

func MixedCalls() int { return mixedCalls }

func MixedScalar() int   { return mixedScalar }
func MixedPointer() *int { return mixedPointer }

func MixedScalarAddress() *int { return &mixedScalar }
