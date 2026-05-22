package gotest

import (
	"fmt"
	"strings"
	"testing"
)

func TestClosedChannelReceiveYieldsZeroAndOKFalse(t *testing.T) {
	ch := make(chan int, 1)
	ch <- 7
	close(ch)

	if got, ok := <-ch; got != 7 || !ok {
		t.Fatalf("buffered receive from closed channel = %d, %v, want 7, true", got, ok)
	}
	if got, ok := <-ch; got != 0 || ok {
		t.Fatalf("empty receive from closed channel = %d, %v, want 0, false", got, ok)
	}

	got := 99
	select {
	case got = <-ch:
	default:
		t.Fatal("receive from closed channel did not select")
	}
	if got != 0 {
		t.Fatalf("select receive from closed channel stored %d, want 0", got)
	}
}

func TestSendOnClosedChannelPanics(t *testing.T) {
	ch := make(chan int)
	close(ch)

	expectChannelPanicContaining(t, "send on closed channel", func() {
		ch <- 1
	})
}

func TestSelectSendOnClosedChannelPanics(t *testing.T) {
	ch := make(chan int)
	close(ch)

	expectChannelPanicContaining(t, "send on closed channel", func() {
		select {
		case ch <- 1:
			t.Fatal("send on closed channel selected without panic")
		default:
			t.Fatal("default selected for send on closed channel")
		}
	})

	expectChannelPanicContaining(t, "send on closed channel", func() {
		var never chan int
		select {
		case ch <- 1:
			t.Fatal("send on closed channel selected without panic")
		case <-never:
			t.Fatal("nil channel receive selected")
		}
	})
}

func TestCloseClosedChannelPanics(t *testing.T) {
	expectChannelPanicContaining(t, "close of nil channel", func() {
		var ch chan int
		close(ch)
	})

	ch := make(chan int)
	close(ch)
	expectChannelPanicContaining(t, "close of closed channel", func() {
		close(ch)
	})
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
