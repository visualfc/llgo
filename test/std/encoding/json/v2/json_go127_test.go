//go:build go1.27

package json_test

import (
	json "encoding/json/v2"
	"testing"
)

type jsonV2Person struct {
	Name string `json:"name"`
	Age  int    `json:"age"`
}

func TestMarshalAndUnmarshal(t *testing.T) {
	want := jsonV2Person{Name: "Ada", Age: 37}
	data, err := json.Marshal(want)
	if err != nil {
		t.Fatal(err)
	}
	if got := string(data); got != `{"name":"Ada","age":37}` {
		t.Fatalf("Marshal = %q", got)
	}
	var got jsonV2Person
	if err := json.Unmarshal(data, &got); err != nil || got != want {
		t.Fatalf("Unmarshal = %#v, %v; want %#v, nil", got, err, want)
	}
	if err := json.Unmarshal([]byte(`{"name":}`), &got); err == nil {
		t.Fatal("Unmarshal accepted invalid JSON")
	}
}

func TestRejectUnknownMembers(t *testing.T) {
	var got jsonV2Person
	err := json.Unmarshal([]byte(`{"name":"Ada","extra":true}`), &got, json.RejectUnknownMembers(true))
	if err == nil {
		t.Fatal("RejectUnknownMembers accepted unknown field")
	}
}
