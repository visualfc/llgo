//go:build go1.27

package uuid_test

import (
	"testing"
	"uuid"
)

func TestParseAndTextRoundTrip(t *testing.T) {
	want := uuid.MustParse("123e4567-e89b-12d3-a456-426614174000")
	got, err := uuid.Parse(want.String())
	if err != nil || got != want {
		t.Fatalf("Parse = %v, %v; want %v, nil", got, err, want)
	}
	if _, err := uuid.Parse("not-a-uuid"); err == nil {
		t.Fatal("Parse accepted malformed UUID")
	}
	text, err := want.MarshalText()
	if err != nil {
		t.Fatal(err)
	}
	var parsed uuid.UUID
	if err := parsed.UnmarshalText(text); err != nil || parsed != want {
		t.Fatalf("UnmarshalText = %v, %v; want %v, nil", parsed, err, want)
	}
}

func TestNewUUIDs(t *testing.T) {
	if got := uuid.NewV4(); got == uuid.Nil() {
		t.Fatal("NewV4 returned Nil")
	}
	if got := uuid.NewV7(); got == uuid.Nil() {
		t.Fatal("NewV7 returned Nil")
	}
	if uuid.Nil().Compare(uuid.Max()) >= 0 {
		t.Fatal("Nil must sort before Max")
	}
}
