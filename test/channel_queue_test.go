package test

import (
	"fmt"
	"strings"
	"testing"
	"time"
)

func TestClosedChannelSendPanicDoesNotKeepChannelLocked(t *testing.T) {
	ch := make(chan int)
	close(ch)

	expectChannelPanicContaining(t, "send on closed channel", func() {
		ch <- 1
	})

	select {
	case _, ok := <-ch:
		if ok {
			t.Fatal("receive from closed channel returned ok=true")
		}
	default:
		t.Fatal("receive from closed channel did not select")
	}
}

func TestSelectDuplicateChannelDoesNotDeadlock(t *testing.T) {
	ch := make(chan int)
	done := make(chan int, 1)

	go func() {
		select {
		case v := <-ch:
			done <- v
		case v := <-ch:
			done <- v
		}
	}()

	ch <- 42
	select {
	case got := <-done:
		if got != 42 {
			t.Fatalf("select receive = %d, want 42", got)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("select with duplicate channel did not finish")
	}
}

func TestConcurrentSelectProposeReplyStress(t *testing.T) {
	const clients = 64
	const perClient = 64

	type proposal struct {
		ack chan struct{}
	}

	propose := make(chan proposal)
	stop := make(chan struct{})
	done := make(chan struct{}, clients)

	go func() {
		for {
			select {
			case p := <-propose:
				p.ack <- struct{}{}
			case <-stop:
				return
			}
		}
	}()

	for c := 0; c < clients; c++ {
		go func() {
			for i := 0; i < perClient; i++ {
				ack := make(chan struct{})
				select {
				case propose <- proposal{ack: ack}:
				case <-stop:
					return
				}
				select {
				case <-ack:
				case <-stop:
					return
				}
			}
			done <- struct{}{}
		}()
	}

	timeout := time.After(10 * time.Second)
	for c := 0; c < clients; c++ {
		select {
		case <-done:
		case <-timeout:
			close(stop)
			t.Fatal("concurrent select request/reply stress timed out")
		}
	}
	close(stop)
}

func expectChannelPanicContaining(t *testing.T, want string, f func()) {
	t.Helper()
	defer func() {
		err := recover()
		if err == nil {
			t.Fatalf("expected panic containing %q", want)
		}
		if got := fmt.Sprint(err); !strings.Contains(got, want) {
			t.Fatalf("panic = %q, want contains %q", got, want)
		}
	}()
	f()
}
