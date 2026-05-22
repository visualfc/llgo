package gotest

import (
	"fmt"
	"strings"
	"testing"
	"time"
)

func TestNilChannelSendRecvBlock(t *testing.T) {
	done := make(chan string, 2)
	var ch chan int

	go func() {
		ch <- 1
		done <- "send"
	}()
	go func() {
		<-ch
		done <- "recv"
	}()

	time.Sleep(20 * time.Millisecond)
	select {
	case op := <-done:
		t.Fatalf("nil channel %s completed, want block", op)
	default:
	}
}

func TestNilChannelSelectCasesUseDefault(t *testing.T) {
	var ch chan int

	select {
	case ch <- 1:
		t.Fatal("nil channel send case selected")
	case <-ch:
		t.Fatal("nil channel receive case selected")
	default:
	}
}

func TestClosedChannelSendPanics(t *testing.T) {
	ch := make(chan int)
	close(ch)

	expectChannelPanicContaining(t, "send on closed channel", func() {
		ch <- 1
	})
}

func TestClosedChannelSelectSendPanics(t *testing.T) {
	ch := make(chan int)
	close(ch)

	expectChannelPanicContaining(t, "send on closed channel", func() {
		select {
		case ch <- 1:
		default:
			t.Fatal("closed channel send selected default")
		}
	})
}

func TestClosedChannelSelectRecvReturnsZero(t *testing.T) {
	ch := make(chan int)
	close(ch)

	select {
	case v, ok := <-ch:
		if v != 0 || ok {
			t.Fatalf("closed channel receive = %d, %v; want 0, false", v, ok)
		}
	default:
		t.Fatal("closed channel receive did not select")
	}
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
