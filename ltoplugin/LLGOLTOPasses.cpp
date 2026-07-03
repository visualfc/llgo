#include "llvm/Config/llvm-config.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"

#include <cstdlib>

using namespace llvm;

namespace {

static constexpr char LLGOPreGlobalDCEPassName[] = "llgo-lto-pre-globaldce";

class LLGOLTOPreGlobalDCEPass
    : public PassInfoMixin<LLGOLTOPreGlobalDCEPass> {
public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &) {
    if (std::getenv("LLGO_LTO_PLUGIN_VERBOSE")) {
      errs() << "llgo-lto-plugin: running " << LLGOPreGlobalDCEPassName
             << " on " << M.getModuleIdentifier() << "\n";
    }

    return PreservedAnalyses::all();
  }
};

} // namespace

PassPluginLibraryInfo getLLGOLTOPluginInfo() {
  return {LLVM_PLUGIN_API_VERSION, "llgo-lto-plugin", LLVM_VERSION_STRING,
          [](PassBuilder &PB) {
            PB.registerPipelineParsingCallback(
                [](StringRef Name, ModulePassManager &MPM,
                   ArrayRef<PassBuilder::PipelineElement>) {
                  if (Name != LLGOPreGlobalDCEPassName)
                    return false;
                  MPM.addPass(LLGOLTOPreGlobalDCEPass());
                  return true;
                });

            PB.registerFullLinkTimeOptimizationEarlyEPCallback(
                [](ModulePassManager &MPM, OptimizationLevel) {
                  MPM.addPass(LLGOLTOPreGlobalDCEPass());
                });
          }};
}

extern "C" LLVM_ATTRIBUTE_WEAK PassPluginLibraryInfo
llvmGetPassPluginInfo() {
  return getLLGOLTOPluginInfo();
}
