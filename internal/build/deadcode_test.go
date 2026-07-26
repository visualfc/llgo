package build

import (
	"reflect"
	"testing"

	"github.com/goplus/llgo/internal/packages"
	llssa "github.com/goplus/llgo/ssa"
)

func TestDCEEntryRootCandidates(t *testing.T) {
	want := []string{"pkg.init", "pkg.main"}
	if got := dceEntryRootCandidates(&packages.Package{PkgPath: "pkg"}, false); !reflect.DeepEqual(got, want) {
		t.Fatalf("dceEntryRootCandidates(false) = %v, want %v", got, want)
	}

	want = append(want, llssa.PkgRuntime+".init")
	if got := dceEntryRootCandidates(&packages.Package{PkgPath: "pkg"}, true); !reflect.DeepEqual(got, want) {
		t.Fatalf("dceEntryRootCandidates(true) = %v, want %v", got, want)
	}
}

func TestDCEEntryRootCandidatesNil(t *testing.T) {
	if got := dceEntryRootCandidates(nil, true); got != nil {
		t.Fatalf("dceEntryRootCandidates(nil) = %v, want nil", got)
	}
}
