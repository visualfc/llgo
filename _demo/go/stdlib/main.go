package main

import (
	"bytes"
	cryptorand "crypto/rand"
	"io"
	"log"
	"math"
	"math/cmplx"
	mathrand "math/rand"
	"net/netip"
	"net/textproto"
	"runtime"
)

// A bounded ecosystem smoke keeps the distinct API calls from ten former tiny
// demos in one executable. Package-specific semantics remain owned by test/std;
// this case checks that an LLGo program can link and execute the combined API
// surface without one independently linked command per package.
func main() {
	if math.Sqrt(81) != 9 || math.Abs(-1.2) != 1.2 || math.Ldexp(1.2, 3) != 9.6 || cmplx.Abs(3+4i) != 5 {
		panic("math")
	}

	var logOutput bytes.Buffer
	log.New(&logOutput, "llgo: ", 0).Print("ok")
	if logOutput.String() != "llgo: ok\n" {
		panic("log")
	}
	logWriter := log.Writer()
	log.SetOutput(io.Discard)
	log.Println("Hello")
	log.SetOutput(logWriter)
	testMapHash()

	header := make(textproto.MIMEHeader)
	header.Add("Content-Type", "text/plain")
	header.Set("host", "www.example.com")
	if header.Get("content-type") != "text/plain" || len(header.Values("Content-Type")) != 1 {
		panic("MIMEHeader")
	}
	if header.Get("Host") != "www.example.com" {
		panic("MIMEHeader Set")
	}
	if addr, err := netip.ParseAddr("127.0.0.1"); err != nil || !addr.IsLoopback() {
		panic("netip")
	}
	if endpoint := netip.MustParseAddrPort("127.0.0.1:80"); endpoint.Port() != 80 {
		panic("netip AddrPort")
	}

	var entropy [8]byte
	if n, err := cryptorand.Read(entropy[:]); err != nil || n != len(entropy) {
		panic("crypto/rand")
	}
	rng := mathrand.New(mathrand.NewSource(1))
	if rng.Int63() != 5577006791947779410 {
		panic("math/rand")
	}
	for i := 0; i < 3; i++ {
		if value := mathrand.Intn(100); value < 0 || value >= 100 {
			panic("math/rand Intn")
		}
	}
	if runtime.GOROOT() == "" {
		panic("runtime.GOROOT")
	}

	testTemplate()
	println("stdlib ecosystem ok")
}
