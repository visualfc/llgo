//go:build windows

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

package syscall

type Errno uintptr

func Error(e Errno) string {
	idx := int(e - APPLICATION_ERROR)
	if 0 <= idx && idx < len(errors) {
		return errors[idx]
	}
	return "winapi error #" + utoa(uint64(e))
}

const (
	ERROR_FILE_NOT_FOUND Errno = 2
	ERROR_PATH_NOT_FOUND Errno = 3
)

// A Signal is a number describing a process signal.
// It implements the [os.Signal] interface.
type Signal int

func (s Signal) Signal() {}

func (s Signal) String() string {
	if 0 <= s && int(s) < len(signals) {
		str := signals[s]
		if str != "" {
			return str
		}
	}
	return "signal " + itoa(int64(s))
}

var signals = [...]string{
	1:  "hangup",
	2:  "interrupt",
	3:  "quit",
	4:  "illegal instruction",
	5:  "trace/breakpoint trap",
	6:  "aborted",
	7:  "bus error",
	8:  "floating point exception",
	9:  "killed",
	10: "user defined signal 1",
	11: "segmentation fault",
	12: "user defined signal 2",
	13: "broken pipe",
	14: "alarm clock",
	15: "terminated",
}

// Timespec is an invented structure on Windows, retained for consistency with
// the syscall package on other operating systems.
type Timespec struct {
	Sec  int64
	Nsec int64
}

func TimespecToNsec(ts Timespec) int64 { return ts.Nano() }

func NsecToTimespec(nsec int64) (ts Timespec) {
	ts.Sec = nsec / 1e9
	ts.Nsec = nsec % 1e9
	return
}

// Timeval is an invented structure on Windows, retained for consistency with
// the syscall package on other operating systems.
type Timeval struct {
	Sec  int32
	Usec int32
}

func TimevalToNsec(tv Timeval) int64 { return tv.Nano() }

func NsecToTimeval(nsec int64) (tv Timeval) {
	tv.Sec = int32(nsec / 1e9)
	tv.Usec = int32(nsec % 1e9 / 1e3)
	return
}
