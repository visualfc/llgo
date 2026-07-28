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
	"sync/atomic"
	"testing"
)

var (
	benchmarkGlobal int64

	//llgo:tls
	benchmarkTLS int64

	//llgo:gls
	benchmarkGLS int64

	benchmarkIntSink int
	benchmarkI64Sink int64
)

//go:noinline
func benchmarkReadGlobal() int64 {
	return atomic.LoadInt64(&benchmarkGlobal)
}

//go:noinline
func benchmarkReadTLS() int64 {
	return atomic.LoadInt64(&benchmarkTLS)
}

//go:noinline
func benchmarkReadGLS() int64 {
	return atomic.LoadInt64(&benchmarkGLS)
}

//go:noinline
func benchmarkWriteGlobal(v int64) {
	atomic.StoreInt64(&benchmarkGlobal, v)
}

//go:noinline
func benchmarkWriteTLS(v int64) {
	atomic.StoreInt64(&benchmarkTLS, v)
}

//go:noinline
func benchmarkWriteGLS(v int64) {
	atomic.StoreInt64(&benchmarkGLS, v)
}

func BenchmarkGlobalRead(b *testing.B) {
	atomic.StoreInt64(&benchmarkGlobal, 1)
	var value int64
	for i := 0; i < b.N; i++ {
		value += benchmarkReadGlobal()
	}
	benchmarkI64Sink = value
}

func BenchmarkTLSRead(b *testing.B) {
	atomic.StoreInt64(&benchmarkTLS, 1)
	var value int64
	for i := 0; i < b.N; i++ {
		value += benchmarkReadTLS()
	}
	benchmarkI64Sink = value
}

func BenchmarkGLSRead(b *testing.B) {
	atomic.StoreInt64(&benchmarkGLS, 1)
	var value int64
	for i := 0; i < b.N; i++ {
		value += benchmarkReadGLS()
	}
	benchmarkI64Sink = value
}

func BenchmarkGlobalWrite(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmarkWriteGlobal(int64(i))
	}
}

func BenchmarkTLSWrite(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmarkWriteTLS(int64(i))
	}
}

func BenchmarkGLSWrite(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmarkWriteGLS(int64(i))
	}
}

//go:noinline
func benchmarkDirectCall(v int) int {
	return v + 1
}

type benchmarkCaller interface {
	call(int) int
}

type benchmarkCallImpl struct{}

//go:noinline
func (benchmarkCallImpl) call(v int) int {
	return v + 1
}

func BenchmarkDirectCall(b *testing.B) {
	value := 0
	for i := 0; i < b.N; i++ {
		value = benchmarkDirectCall(value)
	}
	benchmarkIntSink = value
}

func BenchmarkInterfaceCall(b *testing.B) {
	var caller benchmarkCaller = benchmarkCallImpl{}
	value := 0
	for i := 0; i < b.N; i++ {
		value = caller.call(value)
	}
	benchmarkIntSink = value
}

//go:noinline
func benchmarkDefer() {
	defer func() {}()
}

func BenchmarkDefer(b *testing.B) {
	for i := 0; i < b.N; i++ {
		benchmarkDefer()
	}
}

func BenchmarkGoroutine(b *testing.B) {
	done := make(chan struct{})
	for i := 0; i < b.N; i++ {
		go func() {
			done <- struct{}{}
		}()
		<-done
	}
}

func BenchmarkChannelBuffered(b *testing.B) {
	values := make(chan int, 1)
	for i := 0; i < b.N; i++ {
		values <- i
		benchmarkIntSink = <-values
	}
}

func BenchmarkChannelHandoff(b *testing.B) {
	values := make(chan int)
	acks := make(chan int)
	done := make(chan struct{})
	go func() {
		for value := range values {
			acks <- value
		}
		close(done)
	}()
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		values <- i
		benchmarkIntSink = <-acks
	}
	b.StopTimer()
	close(values)
	<-done
}
