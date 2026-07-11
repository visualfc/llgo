#ifndef LLGO_LTO_PASSES_H
#define LLGO_LTO_PASSES_H

#include "llvm/IR/PassManager.h"

namespace llgo {

inline constexpr char LLGOPreGlobalDCEPassName[] = "llgo-lto-pre-globaldce";

void addLLGOReflectMethodByNamePass(llvm::ModulePassManager &MPM);

inline void addLLGOPreGlobalDCEPipeline(llvm::ModulePassManager &MPM) {
  addLLGOReflectMethodByNamePass(MPM);
}

} // namespace llgo

#endif // LLGO_LTO_PASSES_H
