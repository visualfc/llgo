package main

import (
	"bytes"
	cryptorand "crypto/rand"
	"hash/maphash"
	"log"
	"math"
	"math/cmplx"
	mathrand "math/rand"
	"net/netip"
	"net/textproto"
	"runtime"
	"strings"
	"text/template"
)

// A bounded ecosystem smoke keeps representative calls from ten former tiny
// demos. Package-specific behavior remains owned by test/std; this case checks
// that an LLGo program can link and execute the combined API surface.
func main() {
	if math.Sqrt(81) != 9 || cmplx.Abs(3+4i) != 5 {
		panic("math")
	}

	var logOutput bytes.Buffer
	log.New(&logOutput, "llgo: ", 0).Print("ok")
	if logOutput.String() != "llgo: ok\n" {
		panic("log")
	}

	var hash maphash.Hash
	hash.SetSeed(maphash.MakeSeed())
	_, _ = hash.WriteString("llgo")
	wantHash := hash.Sum64()
	hash.Reset()
	_, _ = hash.WriteString("llgo")
	if hash.Sum64() != wantHash {
		panic("maphash reset")
	}

	header := make(textproto.MIMEHeader)
	header.Add("Content-Type", "text/plain")
	if header.Get("content-type") != "text/plain" || len(header.Values("Content-Type")) != 1 {
		panic("MIMEHeader")
	}
	if addr, err := netip.ParseAddr("127.0.0.1"); err != nil || !addr.IsLoopback() {
		panic("netip")
	}

	var entropy [8]byte
	if n, err := cryptorand.Read(entropy[:]); err != nil || n != len(entropy) {
		panic("crypto/rand")
	}
	rng := mathrand.New(mathrand.NewSource(1))
	if rng.Int63() != 5577006791947779410 {
		panic("math/rand")
	}
	if runtime.GOROOT() == "" {
		panic("runtime.GOROOT")
	}

	tmpl := template.Must(template.New("names").Funcs(template.FuncMap{"join": strings.Join}).Parse(`{{join . ","}}`))
	var rendered strings.Builder
	if err := tmpl.Execute(&rendered, []string{"Go", "LLGo"}); err != nil || rendered.String() != "Go,LLGo" {
		panic("text/template")
	}
	println("stdlib ecosystem ok")
}
