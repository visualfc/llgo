package build

import (
	"github.com/xgo-dev/llgo/internal/abi"
	"github.com/xgo-dev/llvm"
)

func lowerWasmAggregateCopies(_ string, td llvm.TargetData, mod llvm.Module, config abi.AggregateLoweringConfig) int {
	return abi.LowerWasmAggregateCopies(td, mod, config)
}
