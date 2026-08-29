#include "LLGOLTOPasses.h"

#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/GlobalAlias.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Support/raw_ostream.h"

#include <cstdlib>

using namespace llvm;

namespace {

static constexpr char ExactTypeIDExportPrefix[] = "__typeid_go.method.i.";

class LLGOTypeIDExportCleanupPass
    : public PassInfoMixin<LLGOTypeIDExportCleanupPass> {
public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &) {
    // LowerTypeTests exports string type IDs as named aliases even in the
    // monolithic Full LTO pipeline. At the last Full LTO extension point all
    // exact LLGo checks have already been lowered, so aliases with no IR uses
    // are only symbol-table residue. Removing them here leaves the allocated
    // type-test data and lowered checks untouched.
    SmallVector<GlobalAlias *, 32> DeadAliases;
    for (GlobalAlias &Alias : M.aliases()) {
      if (Alias.getName().starts_with(ExactTypeIDExportPrefix) &&
          Alias.use_empty())
        DeadAliases.push_back(&Alias);
    }
    for (GlobalAlias *Alias : DeadAliases)
      Alias->eraseFromParent();
    if (std::getenv("LLGO_LTO_PLUGIN_VERBOSE"))
      errs() << "llgo-lto-plugin: removed " << DeadAliases.size()
             << " unused exact type-id export symbols\n";
    return DeadAliases.empty() ? PreservedAnalyses::all()
                               : PreservedAnalyses::none();
  }
};

} // namespace

namespace llgo {

void addLLGOTypeIDExportCleanupPass(ModulePassManager &MPM) {
  MPM.addPass(LLGOTypeIDExportCleanupPass());
}

} // namespace llgo
