package main

import (
	"errors"
	"io"
	"net"
	"os"
	"time"

	_ "github.com/xgo-dev/llgo/runtime/internal/runtime"
)

const operationTimeout = 5 * time.Second

func mustTCPListener() *net.TCPListener {
	listener, err := net.ListenTCP("tcp4", &net.TCPAddr{IP: net.IPv4(127, 0, 0, 1)})
	if err != nil {
		panic("net.ListenTCP failed: " + err.Error())
	}
	return listener
}

func testTCPRoundTrip() {
	listener := mustTCPListener()
	defer listener.Close()

	serverErr := make(chan error, 1)
	go func() {
		conn, err := listener.AcceptTCP()
		if err != nil {
			serverErr <- err
			return
		}
		defer conn.Close()
		if err := conn.SetDeadline(time.Now().Add(operationTimeout)); err != nil {
			serverErr <- err
			return
		}
		buf := make([]byte, 4)
		if _, err := io.ReadFull(conn, buf); err != nil {
			serverErr <- err
			return
		}
		if string(buf) != "ping" {
			serverErr <- errors.New("TCP server received the wrong payload")
			return
		}
		_, err = conn.Write([]byte("pong"))
		serverErr <- err
	}()

	conn, err := net.DialTimeout("tcp4", listener.Addr().String(), operationTimeout)
	if err != nil {
		panic("net.DialTimeout failed: " + err.Error())
	}
	defer conn.Close()
	if err := conn.SetDeadline(time.Now().Add(operationTimeout)); err != nil {
		panic("TCP SetDeadline failed: " + err.Error())
	}
	if _, err := conn.Write([]byte("ping")); err != nil {
		panic("TCP Write failed: " + err.Error())
	}
	buf := make([]byte, 4)
	if _, err := io.ReadFull(conn, buf); err != nil {
		panic("TCP Read failed: " + err.Error())
	}
	if string(buf) != "pong" {
		panic("TCP client received the wrong payload")
	}
	if err := <-serverErr; err != nil {
		panic("TCP server failed: " + err.Error())
	}
}

func testTCPReadDeadline() {
	listener := mustTCPListener()
	defer listener.Close()

	accepted := make(chan *net.TCPConn, 1)
	acceptErr := make(chan error, 1)
	go func() {
		conn, err := listener.AcceptTCP()
		if err != nil {
			acceptErr <- err
			return
		}
		accepted <- conn
	}()

	client, err := net.DialTimeout("tcp4", listener.Addr().String(), operationTimeout)
	if err != nil {
		panic("deadline TCP dial failed: " + err.Error())
	}
	defer client.Close()

	var server *net.TCPConn
	select {
	case server = <-accepted:
		defer server.Close()
	case err := <-acceptErr:
		panic("deadline TCP accept failed: " + err.Error())
	case <-time.After(operationTimeout):
		panic("deadline TCP accept timed out")
	}

	if err := client.SetReadDeadline(time.Now().Add(100 * time.Millisecond)); err != nil {
		panic("TCP SetReadDeadline failed: " + err.Error())
	}
	var buf [1]byte
	_, err = client.Read(buf[:])
	if err == nil {
		panic("TCP read unexpectedly succeeded before its deadline")
	}
	if !errors.Is(err, os.ErrDeadlineExceeded) {
		panic("TCP read returned the wrong deadline error: " + err.Error())
	}
	if netErr, ok := err.(net.Error); !ok || !netErr.Timeout() {
		panic("TCP read deadline error does not report Timeout")
	}
}

func testTCPListenerClose() {
	listener := mustTCPListener()
	acceptErr := make(chan error, 1)
	started := make(chan struct{})
	go func() {
		close(started)
		conn, err := listener.AcceptTCP()
		if conn != nil {
			conn.Close()
		}
		acceptErr <- err
	}()
	<-started
	if err := listener.Close(); err != nil {
		panic("TCP listener Close failed: " + err.Error())
	}
	select {
	case err := <-acceptErr:
		if err == nil {
			panic("closing a TCP listener did not fail its blocked Accept")
		}
	case <-time.After(operationTimeout):
		panic("closing a TCP listener did not unblock Accept")
	}
}

func testUDP() {
	server, err := net.ListenUDP("udp4", &net.UDPAddr{IP: net.IPv4(127, 0, 0, 1)})
	if err != nil {
		panic("net.ListenUDP failed: " + err.Error())
	}
	defer server.Close()
	if err := server.SetDeadline(time.Now().Add(operationTimeout)); err != nil {
		panic("UDP server SetDeadline failed: " + err.Error())
	}

	client, err := net.DialUDP("udp4", nil, server.LocalAddr().(*net.UDPAddr))
	if err != nil {
		panic("net.DialUDP failed: " + err.Error())
	}
	defer client.Close()
	if err := client.SetDeadline(time.Now().Add(operationTimeout)); err != nil {
		panic("UDP client SetDeadline failed: " + err.Error())
	}

	if _, err := client.Write([]byte("ping")); err != nil {
		panic("UDP Write failed: " + err.Error())
	}
	buf := make([]byte, 4)
	n, addr, err := server.ReadFromUDP(buf)
	if err != nil {
		panic("UDP ReadFrom failed: " + err.Error())
	}
	if n != len(buf) || string(buf) != "ping" {
		panic("UDP server received the wrong payload")
	}
	if _, err := server.WriteToUDP([]byte("pong"), addr); err != nil {
		panic("UDP WriteTo failed: " + err.Error())
	}
	n, err = client.Read(buf)
	if err != nil {
		panic("UDP Read failed: " + err.Error())
	}
	if n != len(buf) || string(buf) != "pong" {
		panic("UDP client received the wrong payload")
	}
}

func testLocalhostLookup() {
	addrs, err := net.LookupHost("localhost")
	if err != nil {
		panic("net.LookupHost(localhost) failed: " + err.Error())
	}
	if len(addrs) == 0 {
		panic("net.LookupHost(localhost) returned no addresses")
	}
}

func main() {
	testTCPRoundTrip()
	testTCPReadDeadline()
	testTCPListenerClose()
	testUDP()
	testLocalhostLookup()
	println("windows network smoke: ok")
}
