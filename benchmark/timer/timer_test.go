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

package timer

import (
	"testing"
	"time"
)

const (
	farFuture     = time.Hour
	timerHeapSize = 1024
)

func timerNoop() {}

func BenchmarkTimerCreateStop(b *testing.B) {
	b.Run(timerImplementation, func(b *testing.B) {
		warm := time.AfterFunc(farFuture, timerNoop)
		if !warm.Stop() {
			b.Fatal("warm far-future timer was not active")
		}
		b.ResetTimer()
		for range b.N {
			timer := time.AfterFunc(farFuture, timerNoop)
			if !timer.Stop() {
				b.Fatal("new far-future timer was not active")
			}
		}
	})
}

func BenchmarkTimerResetActive(b *testing.B) {
	b.Run(timerImplementation, func(b *testing.B) {
		b.ReportAllocs()
		timer := time.AfterFunc(farFuture, timerNoop)
		b.ResetTimer()
		for i := range b.N {
			delay := farFuture + time.Duration(i&1)*time.Second
			if !timer.Reset(delay) {
				b.Fatal("far-future timer became inactive")
			}
		}
		b.StopTimer()
		if !timer.Stop() {
			b.Fatal("reset timer was not active during cleanup")
		}
	})
}

func BenchmarkTimerRearmStopped(b *testing.B) {
	b.Run(timerImplementation, func(b *testing.B) {
		b.ReportAllocs()
		timer := time.AfterFunc(farFuture, timerNoop)
		if !timer.Stop() {
			b.Fatal("new far-future timer was not active")
		}
		b.ResetTimer()
		for range b.N {
			if timer.Reset(farFuture) {
				b.Fatal("stopped timer was reported active")
			}
			if !timer.Stop() {
				b.Fatal("rearmed timer was not active")
			}
		}
	})
}

func BenchmarkTimerResetHeap1024(b *testing.B) {
	b.Run(timerImplementation, func(b *testing.B) {
		b.ReportAllocs()
		timers := make([]*time.Timer, timerHeapSize)
		for i := range timers {
			timers[i] = time.AfterFunc(
				farFuture+time.Duration(i)*time.Millisecond,
				timerNoop,
			)
		}
		target := timers[timerHeapSize/2]

		b.ResetTimer()
		for i := range b.N {
			// 641 is coprime to 1024, so the target moves through the full
			// deadline range instead of remaining near one heap position.
			delay := farFuture + time.Duration((i*641)&(timerHeapSize-1))*time.Millisecond
			if !target.Reset(delay) {
				b.Fatal("heap timer became inactive")
			}
		}
		b.StopTimer()
		for _, timer := range timers {
			if !timer.Stop() {
				b.Fatal("heap timer was not active during cleanup")
			}
		}
	})
}

func BenchmarkTimerAfterFuncZeroDelivery(b *testing.B) {
	b.Run(timerImplementation, func(b *testing.B) {
		delivered := make(chan struct{}, 1)
		callback := func() {
			delivered <- struct{}{}
		}
		time.AfterFunc(0, callback)
		<-delivered
		b.ResetTimer()
		for range b.N {
			time.AfterFunc(0, callback)
			<-delivered
		}
	})
}
