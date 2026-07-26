package build

import (
	"github.com/goplus/llgo/internal/deadcode"
	"github.com/goplus/llgo/internal/meta"
	"github.com/goplus/llgo/internal/packages"
	llssa "github.com/goplus/llgo/ssa"
)

func linkedPackageMetas(pkgs []Package) []*meta.PackageMeta {
	metas := make([]*meta.PackageMeta, 0, len(pkgs))
	for _, pkg := range pkgs {
		if pkg == nil || pkg.Meta == nil {
			continue
		}
		metas = append(metas, pkg.Meta)
	}
	return metas
}

func dceEntryRootCandidates(pkg *packages.Package, needRuntime bool) []string {
	if pkg == nil || pkg.PkgPath == "" {
		return nil
	}
	roots := []string{pkg.PkgPath + ".init", pkg.PkgPath + ".main"}
	if needRuntime {
		roots = append(roots, llssa.PkgRuntime+".init")
	}
	return roots
}

func (c *context) analyzeDeadcode(pkgs []Package, mainPkg *packages.Package, needRuntime bool) (map[string][]int, error) {
	metas := linkedPackageMetas(pkgs)
	if len(metas) == 0 {
		return nil, nil
	}
	summary, err := meta.NewGlobalSummary(metas)
	if err != nil {
		return nil, err
	}
	return deadcode.Analyze(summary, dceEntryRootCandidates(mainPkg, needRuntime)), nil
}
