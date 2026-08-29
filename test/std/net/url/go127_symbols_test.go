//go:build go1.27

package url_test

import (
	"net/url"
	"testing"
)

func TestClone(t *testing.T) {
	original, err := url.Parse("https://user:pass@example.com/old")
	if err != nil {
		t.Fatal(err)
	}
	clone := original.Clone()
	if clone == original || clone.User == original.User {
		t.Fatal("URL.Clone did not copy the URL and its Userinfo")
	}
	clone.Path = "/new"
	if original.Path != "/old" {
		t.Fatalf("mutating URL clone changed original path to %q", original.Path)
	}

	values := url.Values{"key": {"one", "two"}}
	valuesClone := values.Clone()
	valuesClone["key"][0] = "changed"
	if values.Get("key") != "one" {
		t.Fatalf("mutating Values clone changed original to %q", values.Get("key"))
	}
}
