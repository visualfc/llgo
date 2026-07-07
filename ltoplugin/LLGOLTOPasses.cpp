#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Analysis/ValueTracking.h"
#include "llvm/Config/llvm-config.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/Support/Compiler.h"
#include "llvm/Support/raw_ostream.h"

#include <cstdlib>
#include <optional>
#include <string>

using namespace llvm;

namespace {

static constexpr char LLGOPreGlobalDCEPassName[] = "llgo-lto-pre-globaldce";
static constexpr char ReflectMethodByNameCallAttr[] =
    "llgo.reflect.methodbyname";
static constexpr char ReflectMethodByNameArgAttr[] =
    "llgo.reflect.methodbyname.name";
static constexpr char ReflectMethodByNameValueKind[] = "value";
static constexpr char ReflectMethodByNameTypeKind[] = "type";
static constexpr char ReflectValueMethodTypeID[] = "go.method.value.reflect";
static constexpr char ReflectTypeMethodTypeID[] = "go.method.type.reflect";
static constexpr char ReflectValueMethodTypeIDPrefix[] =
    "go.method.value.reflect.";
static constexpr char ReflectTypeMethodTypeIDPrefix[] =
    "go.method.type.reflect.";
static constexpr unsigned MaxReflectMethodNames = 32;
static constexpr unsigned MaxStringAnalysisDepth = 12;

// Keep the replacement bounded. The pass is only meant to refine cases where
// optimization has exposed a small finite set of names. If the set grows too
// large we leave the original generic marker intact, which preserves the old
// conservative GlobalDCE behavior.
bool addName(SmallVectorImpl<std::string> &Names, StringRef Name) {
  for (const std::string &Existing : Names) {
    if (Existing == Name)
      return true;
  }
  if (Names.size() >= MaxReflectMethodNames)
    return false;
  Names.push_back(Name.str());
  return true;
}

// LLGo strings are lowered as (ptr, len). After LTO optimizations the pointer
// usually still points into a private global string literal, possibly with a
// constant GEP offset. Reading through the initializer lets the pass recover
// names that are no longer visible to the Go SSA builder, for example after
// inlining and scalar replacement.
std::optional<std::string> readConstantString(Value *Ptr, Value *Len,
                                              const DataLayout &DL) {
  auto *LenC = dyn_cast<ConstantInt>(Len);
  if (!LenC || LenC->isNegative())
    return std::nullopt;
  uint64_t Size = LenC->getZExtValue();

  int64_t Offset = 0;
  Value *Base = GetPointerBaseWithConstantOffset(Ptr, Offset, DL);
  if (!Base || Offset < 0)
    return std::nullopt;

  auto *GV = dyn_cast<GlobalVariable>(Base->stripPointerCasts());
  if (!GV || !GV->hasInitializer())
    return std::nullopt;

  auto *Data = dyn_cast<ConstantDataArray>(GV->getInitializer());
  if (!Data || !Data->isString())
    return std::nullopt;

  StringRef Bytes = Data->getAsString();
  uint64_t Start = static_cast<uint64_t>(Offset);
  if (Start > Bytes.size() || Size > Bytes.size() - Start)
    return std::nullopt;
  return Bytes.substr(Start, Size).str();
}

bool collectStringSet(Value *Ptr, Value *Len, const DataLayout &DL,
                      SmallVectorImpl<std::string> &Names,
                      unsigned Depth = 0);

bool collectStringSetFromStringValue(Value *StringValue, const DataLayout &DL,
                                     SmallVectorImpl<std::string> &Names,
                                     unsigned Depth = 0);

std::optional<unsigned> singleExtractValueIndex(ExtractValueInst *Extract) {
  if (!Extract || Extract->getNumIndices() != 1)
    return std::nullopt;
  return Extract->getIndices()[0];
}

std::optional<unsigned> singleInsertValueIndex(InsertValueInst *Insert) {
  if (!Insert || Insert->getNumIndices() != 1)
    return std::nullopt;
  return Insert->getIndices()[0];
}

Value *findInsertedValue(Value *Aggregate, unsigned Index) {
  for (Value *Cur = Aggregate;;) {
    auto *Insert = dyn_cast<InsertValueInst>(Cur);
    if (!Insert)
      return nullptr;
    auto InsertIndex = singleInsertValueIndex(Insert);
    if (InsertIndex && *InsertIndex == Index)
      return Insert->getInsertedValueOperand();
    Cur = Insert->getAggregateOperand();
  }
}

bool collectStringSetFromStringConstant(Constant *C, const DataLayout &DL,
                                        SmallVectorImpl<std::string> &Names,
                                        unsigned Depth) {
  if (isa<ConstantAggregateZero>(C))
    return addName(Names, "");

  Constant *Ptr = C->getAggregateElement(0U);
  Constant *Len = C->getAggregateElement(1U);
  if (!Ptr || !Len)
    return false;
  return collectStringSet(Ptr, Len, DL, Names, Depth + 1);
}

bool collectStringSetFromStringValue(Value *StringValue, const DataLayout &DL,
                                     SmallVectorImpl<std::string> &Names,
                                     unsigned Depth) {
  if (Depth > MaxStringAnalysisDepth)
    return false;

  if (auto *C = dyn_cast<Constant>(StringValue))
    return collectStringSetFromStringConstant(C, DL, Names, Depth);

  if (auto *Insert = dyn_cast<InsertValueInst>(StringValue)) {
    Value *Ptr = findInsertedValue(Insert, 0);
    Value *Len = findInsertedValue(Insert, 1);
    if (!Ptr || !Len)
      return false;
    return collectStringSet(Ptr, Len, DL, Names, Depth + 1);
  }

  if (auto *Sel = dyn_cast<SelectInst>(StringValue)) {
    return collectStringSetFromStringValue(Sel->getTrueValue(), DL, Names,
                                           Depth + 1) &&
           collectStringSetFromStringValue(Sel->getFalseValue(), DL, Names,
                                           Depth + 1);
  }

  if (auto *Phi = dyn_cast<PHINode>(StringValue)) {
    for (Value *Incoming : Phi->incoming_values()) {
      if (!collectStringSetFromStringValue(Incoming, DL, Names, Depth + 1))
        return false;
    }
    return true;
  }

  return false;
}

bool collectExtractedStringSet(Value *Ptr, Value *Len, const DataLayout &DL,
                               SmallVectorImpl<std::string> &Names,
                               unsigned Depth) {
  auto *PtrExtract = dyn_cast<ExtractValueInst>(Ptr);
  auto *LenExtract = dyn_cast<ExtractValueInst>(Len);
  auto PtrIndex = singleExtractValueIndex(PtrExtract);
  auto LenIndex = singleExtractValueIndex(LenExtract);
  if (!PtrIndex || !LenIndex || *PtrIndex != 0 || *LenIndex != 1 ||
      PtrExtract->getAggregateOperand() != LenExtract->getAggregateOperand())
    return false;
  return collectStringSetFromStringValue(PtrExtract->getAggregateOperand(), DL,
                                         Names, Depth + 1);
}

// Collect every possible string from a lowered (ptr, len) pair.
//
// The pass is intentionally all-or-nothing: it returns false as soon as any
// incoming value is unknown. That way a partially understood dynamic
// MethodByName call cannot accidentally lose the generic reflect marker and
// make GlobalDCE drop a method that may still be reachable at runtime.
bool collectStringSet(Value *Ptr, Value *Len, const DataLayout &DL,
                      SmallVectorImpl<std::string> &Names,
                      unsigned Depth) {
  if (Depth > MaxStringAnalysisDepth)
    return false;

  Ptr = Ptr->stripPointerCasts();

  if (auto *LenC = dyn_cast<ConstantInt>(Len); LenC && LenC->isZero())
    return addName(Names, "");

  if (auto Name = readConstantString(Ptr, Len, DL))
    return addName(Names, *Name);

  if (collectExtractedStringSet(Ptr, Len, DL, Names, Depth))
    return true;

  auto *PtrSel = dyn_cast<SelectInst>(Ptr);
  auto *LenSel = dyn_cast<SelectInst>(Len);
  if (PtrSel && LenSel && PtrSel->getCondition() == LenSel->getCondition()) {
    return collectStringSet(PtrSel->getTrueValue(), LenSel->getTrueValue(), DL,
                            Names, Depth + 1) &&
           collectStringSet(PtrSel->getFalseValue(), LenSel->getFalseValue(),
                            DL, Names, Depth + 1);
  }
  if (PtrSel && isa<ConstantInt>(Len)) {
    return collectStringSet(PtrSel->getTrueValue(), Len, DL, Names,
                            Depth + 1) &&
           collectStringSet(PtrSel->getFalseValue(), Len, DL, Names,
                            Depth + 1);
  }

  auto *PtrPhi = dyn_cast<PHINode>(Ptr);
  auto *LenPhi = dyn_cast<PHINode>(Len);
  if (PtrPhi && LenPhi && PtrPhi->getParent() == LenPhi->getParent() &&
      PtrPhi->getNumIncomingValues() == LenPhi->getNumIncomingValues()) {
    for (unsigned I = 0, E = PtrPhi->getNumIncomingValues(); I != E; ++I) {
      BasicBlock *Incoming = PtrPhi->getIncomingBlock(I);
      int LenIndex = LenPhi->getBasicBlockIndex(Incoming);
      if (LenIndex < 0)
        return false;
      if (!collectStringSet(PtrPhi->getIncomingValue(I),
                            LenPhi->getIncomingValue(LenIndex), DL, Names,
                            Depth + 1))
        return false;
    }
    return true;
  }

  return false;
}

std::optional<unsigned> findReflectMethodNameArg(CallBase *ReflectCall) {
  for (unsigned I = 0, E = ReflectCall->arg_size(); I != E; ++I) {
    Attribute Attr = ReflectCall->getParamAttr(I, ReflectMethodByNameArgAttr);
    if (Attr.isStringAttribute())
      return I;
  }
  return std::nullopt;
}

// The plugin runs during LTO, after LLGo has applied C ABI lowering to every
// module that participates in the link. At this point a Go string argument is
// always split into (ptr, len). LLGo marks the string pointer parameter, and the
// length is the following parameter produced by the same lowering step.
bool collectStringSetFromCallArgs(CallBase *ReflectCall, const DataLayout &DL,
                                  SmallVectorImpl<std::string> &Names) {
  std::optional<unsigned> NameArg = findReflectMethodNameArg(ReflectCall);
  if (!NameArg)
    return false;

  if (*NameArg + 1 >= ReflectCall->arg_size())
    return false;
  Value *Ptr = ReflectCall->getArgOperand(*NameArg);
  Value *Len = ReflectCall->getArgOperand(*NameArg + 1);
  if (!Ptr->getType()->isPointerTy() || !Len->getType()->isIntegerTy())
    return false;
  return collectStringSet(Ptr, Len, DL, Names);
}

bool isTypeCheckedLoad(CallBase *CB) {
  if (!CB)
    return false;
  auto *Callee = CB->getCalledFunction();
  return Callee && Callee->getIntrinsicID() == Intrinsic::type_checked_load;
}

bool isAssume(CallBase *CB) {
  if (!CB)
    return false;
  auto *Callee = CB->getCalledFunction();
  return Callee && Callee->getIntrinsicID() == Intrinsic::assume;
}

std::optional<StringRef> checkedLoadTypeID(CallBase *CheckedLoad) {
  if (!isTypeCheckedLoad(CheckedLoad) || CheckedLoad->arg_size() < 3)
    return std::nullopt;
  auto *MDValue = dyn_cast<MetadataAsValue>(CheckedLoad->getArgOperand(2));
  if (!MDValue)
    return std::nullopt;
  auto *TypeID = dyn_cast<MDString>(MDValue->getMetadata());
  if (!TypeID)
    return std::nullopt;
  return TypeID->getString();
}

// Remove the original broad marker:
//
//   %r = call @llvm.type.checked.load(..., !"go.method.*.reflect")
//   %ok = extractvalue %r, 1
//   call @llvm.assume(%ok)
//
// The pointer result is intentionally unused in LLGo's GlobalDCE markers, so
// the pass only erases this pattern when all uses match the marker shape. If a
// future IR change gives the checked-load result a real data dependency, the
// pass fails loudly instead of silently changing semantics.
bool eraseGenericCheckedLoad(CallBase *CheckedLoad) {
  SmallVector<Instruction *, 4> ToErase;
  for (User *U : make_early_inc_range(CheckedLoad->users())) {
    auto *Extract = dyn_cast<ExtractValueInst>(U);
    if (!Extract || Extract->getNumIndices() != 1)
      return false;
    unsigned Index = Extract->getIndices()[0];
    if (Index == 0) {
      if (!Extract->use_empty())
        return false;
      ToErase.push_back(Extract);
      continue;
    }
    if (Index != 1)
      return false;
    for (User *EU : make_early_inc_range(Extract->users())) {
      auto *Assume = dyn_cast<CallBase>(EU);
      if (!isAssume(Assume))
        return false;
      ToErase.push_back(Assume);
    }
    ToErase.push_back(Extract);
  }

  for (Instruction *I : ToErase) {
    if (!I->use_empty())
      return false;
    I->eraseFromParent();
  }
  if (!CheckedLoad->use_empty())
    return false;
  CheckedLoad->eraseFromParent();
  return true;
}

// Replace one generic "some method may be reflected" marker with a disjunction
// of name-specific markers. The name-specific type IDs are emitted on ABI method
// slots by LLGo, so GlobalDCE can now keep only methods whose names are proven
// reachable by this call.
void insertMethodNameChecks(CallBase *GenericLoad,
                            ArrayRef<std::string> Names, StringRef Prefix) {
  IRBuilder<> B(GenericLoad);
  LLVMContext &Ctx = GenericLoad->getContext();
  SmallVector<Value *, 8> OKs;
  for (const std::string &Name : Names) {
    std::string TypeID = (Prefix + Name).str();
    Value *MDVal = MetadataAsValue::get(Ctx, MDString::get(Ctx, TypeID));
    CallInst *Checked = B.CreateIntrinsic(
        Intrinsic::type_checked_load, {},
        {GenericLoad->getArgOperand(0), GenericLoad->getArgOperand(1), MDVal});
    OKs.push_back(B.CreateExtractValue(Checked, {1}));
  }
  if (OKs.empty())
    return;

  Value *AnyOK = OKs.front();
  for (Value *OK : ArrayRef<Value *>(OKs).drop_front())
    AnyOK = B.CreateOr(AnyOK, OK);
  B.CreateIntrinsic(Intrinsic::assume, {}, {AnyOK});
}

bool isReflectMethodCheckedLoad(CallBase *CheckedLoad, StringRef GenericTypeID) {
  auto TypeID = checkedLoadTypeID(CheckedLoad);
  return TypeID && *TypeID == GenericTypeID;
}

Value *getSRetArg(CallBase *CB) {
  for (unsigned I = 0, E = CB->arg_size(); I != E; ++I) {
    if (CB->paramHasAttr(I, Attribute::StructRet))
      return CB->getArgOperand(I);
  }
  return nullptr;
}

void addReflectMethodCheckedLoad(CallBase *CheckedLoad, StringRef GenericTypeID,
                                 SmallPtrSetImpl<CallBase *> &SeenLoads,
                                 SmallVectorImpl<CallBase *> &Loads) {
  if (isReflectMethodCheckedLoad(CheckedLoad, GenericTypeID) &&
      SeenLoads.insert(CheckedLoad).second)
    Loads.push_back(CheckedLoad);
}

// MethodByName returns through an sret slot after C ABI lowering. LLGo then
// emits the generic reflect checked-load from the method value extracted out of
// that slot. This use-def walk is intentionally limited to value-preserving IR
// around that slot: GEPs into the sret object, loads from those fields, and
// scalar/aggregate extractions from the loaded values. It avoids a broad CFG
// scan while still covering both lowered shapes:
//
//   reflect.Value.MethodByName:
//     call @reflect.Value.MethodByName(sret %ret, ...)
//     %raw = load <2 x ptr>, ptr %ret
//     %fn = extractelement <2 x ptr> %raw, 1
//     call @llvm.type.checked.load(ptr %fn, ..., !"go.method.value.reflect")
//
//   reflect.Type.MethodByName:
//     call %methodByName(sret %ret, ...)
//     ... branch on the ok field ...
//     %raw = load <2 x ptr>, ptr gep(%ret, method.func)
//     %fn = extractelement <2 x ptr> %raw, 1
//     call @llvm.type.checked.load(ptr %fn, ..., !"go.method.type.reflect")
void collectSRetReflectMethodCheckedLoads(
    Value *V, StringRef GenericTypeID, SmallPtrSetImpl<Value *> &SeenValues,
    SmallPtrSetImpl<CallBase *> &SeenLoads, SmallVectorImpl<CallBase *> &Loads,
    unsigned Depth = 0) {
  if (!V || Depth > 16 || !SeenValues.insert(V).second)
    return;

  for (User *U : V->users()) {
    if (auto *CB = dyn_cast<CallBase>(U)) {
      addReflectMethodCheckedLoad(CB, GenericTypeID, SeenLoads, Loads);
      continue;
    }

    auto *I = dyn_cast<Instruction>(U);
    if (!I)
      continue;
    if (isa<GetElementPtrInst>(I) || isa<LoadInst>(I) ||
        isa<ExtractElementInst>(I) || isa<ExtractValueInst>(I) ||
        isa<CastInst>(I) || isa<SelectInst>(I) || isa<PHINode>(I)) {
      collectSRetReflectMethodCheckedLoads(I, GenericTypeID, SeenValues,
                                           SeenLoads, Loads, Depth + 1);
    }
  }
}

class LLGOLTOPreGlobalDCEPass
    : public PassInfoMixin<LLGOLTOPreGlobalDCEPass> {
public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &) {
    SmallVector<CallBase *, 16> Calls;
    for (Function &F : M) {
      for (BasicBlock &BB : F) {
        for (Instruction &I : BB) {
          auto *CB = dyn_cast<CallBase>(&I);
          if (!CB || !CB->hasFnAttr(ReflectMethodByNameCallAttr))
            continue;
          Calls.push_back(CB);
        }
      }
    }

    bool Changed = false;
    const DataLayout &DL = M.getDataLayout();
    for (CallBase *ReflectCall : Calls) {
      Attribute KindAttr = ReflectCall->getFnAttr(ReflectMethodByNameCallAttr);
      if (!KindAttr.isStringAttribute() || ReflectCall->arg_empty())
        continue;

      StringRef Prefix;
      StringRef GenericTypeID;
      StringRef Kind = KindAttr.getValueAsString();
      if (Kind == ReflectMethodByNameValueKind) {
        Prefix = ReflectValueMethodTypeIDPrefix;
        GenericTypeID = ReflectValueMethodTypeID;
      } else if (Kind == ReflectMethodByNameTypeKind) {
        Prefix = ReflectTypeMethodTypeIDPrefix;
        GenericTypeID = ReflectTypeMethodTypeID;
      } else {
        continue;
      }

      SmallVector<std::string, 4> Names;
      bool KnownNames = collectStringSetFromCallArgs(ReflectCall, DL, Names);
      if (!KnownNames || Names.empty())
        continue;

      SmallVector<CallBase *, 2> GenericLoads;
      SmallPtrSet<CallBase *, 4> SeenLoads;
      SmallPtrSet<Value *, 16> SeenValues;
      collectSRetReflectMethodCheckedLoads(getSRetArg(ReflectCall),
                                           GenericTypeID, SeenValues, SeenLoads,
                                           GenericLoads);
      for (CallBase *GenericLoad : GenericLoads) {
        insertMethodNameChecks(GenericLoad, Names, Prefix);
        if (!eraseGenericCheckedLoad(GenericLoad))
          report_fatal_error(
              "llgo-lto-plugin: failed to erase generic MethodByName check");
        Changed = true;
      }

      if (!GenericLoads.empty() && std::getenv("LLGO_LTO_PLUGIN_VERBOSE")) {
        errs() << "llgo-lto-plugin: refined " << Kind << " to";
        for (const std::string &Name : Names)
          errs() << " " << Name;
        errs() << "\n";
      }
    }

    return Changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
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

extern "C" LLVM_ATTRIBUTE_WEAK PassPluginLibraryInfo llvmGetPassPluginInfo() {
  return getLLGOLTOPluginInfo();
}
