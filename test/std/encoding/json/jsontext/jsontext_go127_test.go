//go:build go1.27

package jsontext_test

import (
	"bytes"
	"encoding/json/jsontext"
	"testing"
)

func TestValueFormattingAndValidation(t *testing.T) {
	v := jsontext.Value(` { "answer" : 42 } `)
	if !v.IsValid() {
		t.Fatal("Value is not valid JSON")
	}
	if err := v.Compact(); err != nil {
		t.Fatal(err)
	}
	if got, want := v.String(), `{"answer":42}`; got != want {
		t.Fatalf("Compact = %q, want %q", got, want)
	}
	if jsontext.Value(`{"x":}`).IsValid() {
		t.Fatal("invalid Value reported valid")
	}
}

func TestEncoderDecoder(t *testing.T) {
	var buf bytes.Buffer
	enc := jsontext.NewEncoder(&buf)
	if err := enc.WriteValue(jsontext.Value(`{"ok":true}`)); err != nil {
		t.Fatal(err)
	}
	dec := jsontext.NewDecoder(&buf)
	v, err := dec.ReadValue()
	if err != nil || v.String() != `{"ok":true}` {
		t.Fatalf("ReadValue = %q, %v", v, err)
	}
}
