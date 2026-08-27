//go:build windows

package windowstesting

import (
	"context"
	"os"
	"os/exec"
	"testing"
	"time"
)

const exitDuringThreadCreateHelper = "LLGO_WINDOWS_EXIT_THREAD_CREATE_HELPER"

func TestRepeatedProcessLifecycle(t *testing.T) {
	const iterations = 32
	for i := 0; i < iterations; i++ {
		cmd := exec.Command(`C:\Windows\System32\cmd.exe`, "/c", "exit", "0")
		if err := cmd.Run(); err != nil {
			t.Fatalf("iteration %d: %v", i, err)
		}
	}
}

func TestExitDuringThreadCreation(t *testing.T) {
	if mode := os.Getenv(exitDuringThreadCreateHelper); mode != "" {
		runExitThreadSieve()
		if mode == "os-exit" {
			os.Exit(0)
		}
		return
	}

	const iterations = 64
	for _, mode := range []string{"return", "os-exit"} {
		for i := 0; i < iterations; i++ {
			ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
			cmd := exec.CommandContext(ctx, os.Args[0], "-test.run=^TestExitDuringThreadCreation$")
			cmd.Env = append(os.Environ(), exitDuringThreadCreateHelper+"="+mode)
			output, err := cmd.CombinedOutput()
			timedOut := ctx.Err() != nil
			cancel()
			if timedOut {
				t.Fatalf("%s iteration %d: child process did not exit\n%s", mode, i, output)
			}
			if err != nil {
				t.Fatalf("%s iteration %d: %v\n%s", mode, i, err, output)
			}
		}
	}
}

func runExitThreadSieve() {
	primes := make(chan int)
	go func() {
		ch := make(chan int)
		go func() {
			for value := 2; ; value++ {
				ch <- value
			}
		}()
		for {
			prime := <-ch
			primes <- prime
			next := make(chan int)
			go func(input <-chan int, output chan<- int, divisor int) {
				for value := range input {
					if value%divisor != 0 {
						output <- value
					}
				}
			}(ch, next, prime)
			ch = next
		}
	}()
	for range 25 {
		<-primes
	}
}
