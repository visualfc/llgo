package build

import (
	"errors"
	"strings"

	"github.com/xgo-dev/llgo/internal/packages"
)

// These settings affect package code, not just the final link. Independent
// programs may share a backend/cache entry only when these settings agree.
type initialBuildFeatures struct {
	localContext    bool
	reflectBridges  bool
	funcInfoEntries bool
}

type initialBuildGroup struct {
	features initialBuildFeatures
	pkgs     []*packages.Package
}

func groupInitialBuilds(ctx *context, alts []*packages.Package) []initialBuildGroup {
	if len(ctx.initial) < 2 || ctx.mode == ModeGen {
		return nil
	}
	var groups []initialBuildGroup
	indexes := make(map[initialBuildFeatures]int)
	target := ctx.prog.Target()
	for _, pkg := range ctx.initial {
		features := initialBuildFeatures{
			localContext: ctx.prog.NeedsLocalContextForPackages(activeLocalityPackages([]*packages.Package{pkg}, alts)),
		}
		if target.GOARCH == "wasm" {
			roots := wasmReflectRoots(&context{progSSA: ctx.progSSA, initial: []*packages.Package{pkg}})
			use := analyzeWasmProgramUse(ctx.progSSA, roots)
			features.reflectBridges = target.WasmProvider == "wasi" && use.usesWasmReflectBridges()
			features.funcInfoEntries = ctx.buildConf.BuildMode != BuildModeExe || use.usesRuntimeFuncForPC()
		}
		index, ok := indexes[features]
		if !ok {
			index = len(groups)
			indexes[features] = index
			groups = append(groups, initialBuildGroup{features: features})
		}
		groups[index].pkgs = append(groups[index].pkgs, pkg)
	}
	return groups
}

func buildInitialGroups(inv Invocation, ctx *context, groups []initialBuildGroup) ([]Package, error) {
	var result []Package
	var failures []error
	// Keep the original package-worker budget. Ordinary test batches generally
	// form a single group and retain the shared frontend and native test DAG.
	for _, group := range groups {
		conf := ctx.buildConf.clone()
		args := make([]string, len(group.pkgs))
		for i, pkg := range group.pkgs {
			args[i] = pkg.PkgPath
			if ctx.mode == ModeTest {
				args[i] = strings.TrimSuffix(args[i], ".test")
			}
		}
		child := inv
		child.Args, child.Config, child.Dir = args, conf, ctx.commands.dir
		child.multipleInitials = true
		child.initialFeatures = &group.features
		child.parentBuildTrace = ctx.buildTrace
		pkgs, err := Build(child)
		result = append(result, pkgs...)
		if err != nil {
			failures = append(failures, err)
		}
	}
	return result, errors.Join(failures...)
}
