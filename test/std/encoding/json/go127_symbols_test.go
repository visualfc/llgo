//go:build go1.27

package json_test

import (
	"bytes"
	"encoding/json"
	"encoding/json/jsontext"
	jsonv2 "encoding/json/v2"
	"errors"
	"strings"
	"testing"
)

func TestNumberJSONV2Methods(t *testing.T) {
	var output bytes.Buffer
	if err := json.Number("12.5").MarshalJSONTo(jsontext.NewEncoder(&output)); err != nil {
		t.Fatal(err)
	}
	if got := strings.TrimSpace(output.String()); got != "12.5" {
		t.Fatalf("MarshalJSONTo output = %q", got)
	}

	var number json.Number
	if err := number.UnmarshalJSONFrom(jsontext.NewDecoder(strings.NewReader("42"))); err != nil {
		t.Fatal(err)
	}
	if number != "42" {
		t.Fatalf("UnmarshalJSONFrom result = %q", number)
	}

	var options json.Options = json.DefaultOptionsV1()
	encoded, err := jsonv2.Marshal(number, options)
	if err != nil {
		t.Fatal(err)
	}
	if string(encoded) != "42" {
		t.Fatalf("v2 Marshal with v1 options = %q", encoded)
	}
}

func TestUnmarshalTypeErrorUnwrap(t *testing.T) {
	want := errors.New("detail")
	err := &json.UnmarshalTypeError{Err: want}
	if got := err.Unwrap(); got != want || !errors.Is(err, want) {
		t.Fatalf("Unwrap = %v, want %v", got, want)
	}
}
