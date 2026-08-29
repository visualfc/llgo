#include "LLGOLTOPasses.h"

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSet.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"

#include <algorithm>
#include <cstdlib>
#include <optional>
#include <string>
#include <utility>
#include <vector>

using namespace llvm;

namespace {

static constexpr char InterfaceTypeMetadata[] = "llgo.interface.type";
static constexpr char InterfaceMethodMetadata[] = "llgo.interface.method";
static constexpr char InterfaceTypeIDPrefix[] = "go.method.i.";
static constexpr char MethodTypeIDPrefix[] = "go.method.";
static constexpr char ReflectValueMethodTypeID[] = "go.method.value.reflect";
static constexpr char ReflectTypeMethodTypeID[] = "go.method.type.reflect";
static constexpr char ReflectValueMethodTypeIDPrefix[] =
    "go.method.value.reflect.";
static constexpr char ReflectTypeMethodTypeIDPrefix[] =
    "go.method.type.reflect.";
static constexpr uint64_t InterfaceMetadataVersion = 1;

struct InterfaceMethodDecl {
  unsigned Index = 0;
  std::string ExactTypeID;
  std::string BroadTypeID;
};

struct InterfaceDecl {
  std::string TypeID;
  std::vector<InterfaceMethodDecl> Methods;
};

struct DescriptorInfo {
  GlobalVariable *GV = nullptr;
  StringMap<uint64_t> BroadSlots;
  StringMap<uint64_t> ExactSlots;
};

struct TypeCheckSite {
  CallBase *Call = nullptr;
  unsigned TypeIDArg = 0;
};

std::optional<uint64_t> metadataUInt(Metadata *MD) {
  auto *CAM = dyn_cast_or_null<ConstantAsMetadata>(MD);
  auto *CI = CAM ? dyn_cast<ConstantInt>(CAM->getValue()) : nullptr;
  if (!CI || CI->isNegative())
    return std::nullopt;
  return CI->getZExtValue();
}

std::optional<StringRef> metadataString(Metadata *MD) {
  auto *S = dyn_cast_or_null<MDString>(MD);
  if (!S)
    return std::nullopt;
  return S->getString();
}

std::optional<StringRef> checkedLoadTypeID(CallBase *CB) {
  if (!CB || CB->arg_size() < 3)
    return std::nullopt;
  Function *Callee = CB->getCalledFunction();
  if (!Callee || Callee->getIntrinsicID() != Intrinsic::type_checked_load)
    return std::nullopt;
  auto *MDValue = dyn_cast<MetadataAsValue>(CB->getArgOperand(2));
  auto *TypeID = MDValue ? dyn_cast<MDString>(MDValue->getMetadata()) : nullptr;
  return TypeID ? std::optional<StringRef>(TypeID->getString()) : std::nullopt;
}

std::optional<StringRef> typeTestTypeID(CallBase *CB) {
  if (!CB || CB->arg_size() < 2)
    return std::nullopt;
  Function *Callee = CB->getCalledFunction();
  if (!Callee || Callee->getIntrinsicID() != Intrinsic::type_test)
    return std::nullopt;
  auto *MDValue = dyn_cast<MetadataAsValue>(CB->getArgOperand(1));
  auto *TypeID = MDValue ? dyn_cast<MDString>(MDValue->getMetadata()) : nullptr;
  return TypeID ? std::optional<StringRef>(TypeID->getString()) : std::nullopt;
}

bool isReflectMethodTypeID(StringRef TypeID) {
  return TypeID == ReflectValueMethodTypeID ||
         TypeID.starts_with(ReflectValueMethodTypeIDPrefix) ||
         TypeID == ReflectTypeMethodTypeID ||
         TypeID.starts_with(ReflectTypeMethodTypeIDPrefix);
}

[[noreturn]] void invalidMetadata(const Twine &Reason) {
  report_fatal_error(
      Twine("llgo-lto-plugin: invalid interface type-id metadata: ") + Reason);
}

bool sameDeclaration(const InterfaceDecl &A, const InterfaceDecl &B) {
  if (A.TypeID != B.TypeID || A.Methods.size() != B.Methods.size())
    return false;
  for (unsigned I = 0; I < A.Methods.size(); ++I) {
    const InterfaceMethodDecl &AM = A.Methods[I];
    const InterfaceMethodDecl &BM = B.Methods[I];
    if (AM.Index != BM.Index || AM.ExactTypeID != BM.ExactTypeID ||
        AM.BroadTypeID != BM.BroadTypeID)
      return false;
  }
  return true;
}

std::optional<InterfaceDecl> parseInterfaceDeclaration(GlobalVariable &GV,
                                                       unsigned HeaderKind,
                                                       unsigned MethodKind) {
  SmallVector<std::pair<unsigned, MDNode *>, 16> Metadata;
  GV.getAllMetadata(Metadata);

  MDNode *Header = nullptr;
  SmallVector<MDNode *, 8> MethodNodes;
  for (auto [Kind, Node] : Metadata) {
    if (Kind == HeaderKind) {
      if (Header)
        invalidMetadata(Twine("duplicate interface header on ") + GV.getName());
      Header = Node;
    } else if (Kind == MethodKind) {
      MethodNodes.push_back(Node);
    }
  }
  if (!Header) {
    if (!MethodNodes.empty())
      invalidMetadata(Twine("method declaration without header on ") +
                      GV.getName());
    return std::nullopt;
  }
  if (Header->getNumOperands() != 3)
    invalidMetadata(Twine("malformed header on ") + GV.getName());

  auto Version = metadataUInt(Header->getOperand(0));
  auto TypeID = metadataString(Header->getOperand(1));
  auto MethodCount = metadataUInt(Header->getOperand(2));
  if (!Version || *Version != InterfaceMetadataVersion || !TypeID ||
      !TypeID->starts_with(InterfaceTypeIDPrefix) || !MethodCount ||
      *MethodCount == 0 || *MethodCount > UINT_MAX)
    invalidMetadata(Twine("unsupported header on ") + GV.getName());
  if (MethodNodes.size() != *MethodCount)
    invalidMetadata(Twine("method count mismatch on ") + GV.getName());

  InterfaceDecl Decl;
  Decl.TypeID = TypeID->str();
  Decl.Methods.resize(static_cast<size_t>(*MethodCount));
  std::vector<bool> Seen(static_cast<size_t>(*MethodCount), false);
  for (MDNode *Node : MethodNodes) {
    if (Node->getNumOperands() != 3)
      invalidMetadata(Twine("malformed method on ") + GV.getName());
    auto Index = metadataUInt(Node->getOperand(0));
    auto ExactTypeID = metadataString(Node->getOperand(1));
    auto BroadTypeID = metadataString(Node->getOperand(2));
    if (!Index || *Index >= *MethodCount || !ExactTypeID || !BroadTypeID ||
        !BroadTypeID->starts_with(MethodTypeIDPrefix) ||
        BroadTypeID->starts_with(InterfaceTypeIDPrefix) ||
        isReflectMethodTypeID(*BroadTypeID))
      invalidMetadata(Twine("unsupported method on ") + GV.getName());
    unsigned I = static_cast<unsigned>(*Index);
    std::string Expected =
        Decl.TypeID + ".m" + std::to_string(static_cast<uint64_t>(I));
    if (*ExactTypeID != Expected || Seen[I])
      invalidMetadata(Twine("inconsistent method on ") + GV.getName());
    Seen[I] = true;
    Decl.Methods[I] = {I, ExactTypeID->str(), BroadTypeID->str()};
  }
  return Decl;
}

bool collectTypeSlots(GlobalVariable &GV, unsigned TypeKind,
                      DescriptorInfo &Info) {
  SmallVector<std::pair<unsigned, MDNode *>, 16> Metadata;
  GV.getAllMetadata(Metadata);
  for (auto [Kind, Node] : Metadata) {
    if (Kind != TypeKind || Node->getNumOperands() < 2)
      continue;
    auto Offset = metadataUInt(Node->getOperand(0));
    auto TypeID = metadataString(Node->getOperand(1));
    if (!Offset || !TypeID)
      continue;
    StringMap<uint64_t> *Slots = nullptr;
    if (TypeID->starts_with(InterfaceTypeIDPrefix))
      Slots = &Info.ExactSlots;
    else if (TypeID->starts_with(MethodTypeIDPrefix) &&
             !isReflectMethodTypeID(*TypeID))
      Slots = &Info.BroadSlots;
    else
      continue;
    auto [It, Inserted] = Slots->try_emplace(*TypeID, *Offset);
    if (!Inserted && It->second != *Offset)
      invalidMetadata(Twine("type id has multiple offsets on ") + GV.getName());
  }
  return !Info.BroadSlots.empty();
}

bool addExactTypeMetadata(DescriptorInfo &Info,
                          const InterfaceMethodDecl &Method,
                          unsigned TypeKind) {
  auto Broad = Info.BroadSlots.find(Method.BroadTypeID);
  if (Broad == Info.BroadSlots.end())
    return false;
  uint64_t Offset = Broad->second;
  auto [Existing, Inserted] =
      Info.ExactSlots.try_emplace(Method.ExactTypeID, Offset);
  if (!Inserted) {
    if (Existing->second != Offset)
      invalidMetadata(Twine("exact type id has multiple offsets on ") +
                      Info.GV->getName());
    return false;
  }

  LLVMContext &Ctx = Info.GV->getContext();
  Metadata *Ops[] = {
      ConstantAsMetadata::get(ConstantInt::get(Type::getInt64Ty(Ctx), Offset)),
      MDString::get(Ctx, Method.ExactTypeID),
  };
  Info.GV->addMetadata(TypeKind, *MDNode::get(Ctx, Ops));
  return true;
}

class LLGOInterfaceMethodTypeIDPass
    : public PassInfoMixin<LLGOInterfaceMethodTypeIDPass> {
public:
  PreservedAnalyses run(Module &M, ModuleAnalysisManager &) {
    StringSet<> ActiveExactTypeIDs;
    StringMap<SmallVector<TypeCheckSite, 4>> TypeChecksByExactTypeID;
    for (Function &F : M) {
      for (BasicBlock &BB : F) {
        for (Instruction &I : BB) {
          auto *CB = dyn_cast<CallBase>(&I);
          auto TypeID = checkedLoadTypeID(CB);
          unsigned TypeIDArg = 2;
          if (!TypeID) {
            TypeID = typeTestTypeID(CB);
            TypeIDArg = 1;
          }
          if (TypeID && TypeID->starts_with(InterfaceTypeIDPrefix)) {
            ActiveExactTypeIDs.insert(*TypeID);
            TypeChecksByExactTypeID[*TypeID].push_back({CB, TypeIDArg});
          }
        }
      }
    }

    unsigned HeaderKind = M.getContext().getMDKindID(InterfaceTypeMetadata);
    unsigned MethodKind = M.getContext().getMDKindID(InterfaceMethodMetadata);
    unsigned TypeKind = M.getContext().getMDKindID("type");
    unsigned VCallVisibilityKind =
        M.getContext().getMDKindID("vcall_visibility");

    StringMap<InterfaceDecl> Declarations;
    SmallPtrSet<GlobalVariable *, 16> DeclarationGlobals;
    for (GlobalVariable &GV : M.globals()) {
      auto Parsed = parseInterfaceDeclaration(GV, HeaderKind, MethodKind);
      if (!Parsed)
        continue;
      DeclarationGlobals.insert(&GV);
      std::string TypeID = Parsed->TypeID;
      auto [It, Inserted] =
          Declarations.try_emplace(TypeID, std::move(*Parsed));
      if (!Inserted && !sameDeclaration(It->second, *Parsed))
        invalidMetadata(Twine("conflicting declarations for ") + It->getKey());
    }

    StringSet<> CoveredExactTypeIDs;
    for (const auto &Entry : Declarations) {
      for (const InterfaceMethodDecl &Method : Entry.second.Methods) {
        if (ActiveExactTypeIDs.contains(Method.ExactTypeID))
          CoveredExactTypeIDs.insert(Method.ExactTypeID);
      }
    }
    for (const auto &Entry : ActiveExactTypeIDs) {
      if (!CoveredExactTypeIDs.contains(Entry.getKey()))
        invalidMetadata(Twine("no declaration for active type id ") +
                        Entry.getKey());
    }

    std::vector<DescriptorInfo> Descriptors;
    StringMap<SmallVector<unsigned, 8>> DescriptorsByBroadTypeID;
    for (GlobalVariable &GV : M.globals()) {
      if (DeclarationGlobals.contains(&GV) ||
          !GV.getMetadata(VCallVisibilityKind))
        continue;
      DescriptorInfo Info;
      Info.GV = &GV;
      if (!collectTypeSlots(GV, TypeKind, Info))
        continue;
      unsigned Index = Descriptors.size();
      for (const auto &Slot : Info.BroadSlots)
        DescriptorsByBroadTypeID[Slot.getKey()].push_back(Index);
      Descriptors.push_back(std::move(Info));
    }

    bool Changed = false;
    unsigned AddedTypeIDs = 0;
    unsigned ActiveInterfaces = 0;
    unsigned BroadFallbacks = 0;
    for (const auto &Entry : Declarations) {
      const InterfaceDecl &Decl = Entry.second;
      SmallVector<const InterfaceMethodDecl *, 4> ActiveMethods;
      for (const InterfaceMethodDecl &Method : Decl.Methods) {
        if (ActiveExactTypeIDs.contains(Method.ExactTypeID))
          ActiveMethods.push_back(&Method);
      }
      if (ActiveMethods.empty())
        continue;
      ++ActiveInterfaces;

      const SmallVector<unsigned, 8> *Seed = nullptr;
      for (const InterfaceMethodDecl &Method : Decl.Methods) {
        auto It = DescriptorsByBroadTypeID.find(Method.BroadTypeID);
        if (It == DescriptorsByBroadTypeID.end()) {
          Seed = nullptr;
          break;
        }
        if (!Seed || It->second.size() < Seed->size())
          Seed = &It->second;
      }

      unsigned Implementers = 0;
      if (Seed) {
        for (unsigned DescriptorIndex : *Seed) {
          DescriptorInfo &Info = Descriptors[DescriptorIndex];
          bool Implements = llvm::all_of(
              Decl.Methods, [&](const InterfaceMethodDecl &Method) {
                return Info.BroadSlots.contains(Method.BroadTypeID);
              });
          if (!Implements)
            continue;
          ++Implementers;
          for (const InterfaceMethodDecl *Method : ActiveMethods) {
            if (addExactTypeMetadata(Info, *Method, TypeKind)) {
              Changed = true;
              ++AddedTypeIDs;
            }
          }
        }
      }

      // Some runtime/reflection interfaces are constructed through special
      // paths that do not materialize a complete concrete descriptor in the
      // LTO module. An empty proven set is therefore not enough to conclude
      // that the call is unreachable. Fall back to the old signature-wide
      // capability for those calls; exact refinement remains enabled for every
      // interface with at least one closed-world implementer certificate.
      if (Implementers == 0) {
        for (const InterfaceMethodDecl *Method : ActiveMethods) {
          for (TypeCheckSite Site :
               TypeChecksByExactTypeID[Method->ExactTypeID]) {
            Site.Call->setArgOperand(
                Site.TypeIDArg,
                MetadataAsValue::get(
                    M.getContext(),
                    MDString::get(M.getContext(), Method->BroadTypeID)));
            Changed = true;
            ++BroadFallbacks;
          }
        }
      }

      if (std::getenv("LLGO_LTO_PLUGIN_VERBOSE"))
        errs() << "llgo-lto-plugin: interface " << Decl.TypeID << " has "
               << Implementers << " implementers for " << ActiveMethods.size()
               << " active methods"
               << (Implementers == 0 ? " (broad fallback)" : "") << "\n";
    }

    // The frontend preserves declaration descriptors because private metadata
    // is not an IR use. Once the closed-world mapping has been materialized as
    // LLVM !type entries, remove only that temporary preservation. Ordinary IR
    // references still keep descriptors that are needed at runtime.
    if (!DeclarationGlobals.empty()) {
      removeFromUsedLists(M, [&](Constant *C) {
        auto *GV = dyn_cast<GlobalVariable>(C->stripPointerCasts());
        bool Remove = GV && DeclarationGlobals.contains(GV);
        Changed |= Remove;
        return Remove;
      });
    }

    if (std::getenv("LLGO_LTO_PLUGIN_VERBOSE"))
      errs() << "llgo-lto-plugin: materialized " << AddedTypeIDs
             << " exact interface method type ids for " << ActiveInterfaces
             << " active interfaces; " << BroadFallbacks
             << " type checks used broad fallback\n";
    return Changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
  }
};

} // namespace

namespace llgo {

void addLLGOInterfaceMethodTypeIDPass(ModulePassManager &MPM) {
  MPM.addPass(LLGOInterfaceMethodTypeIDPass());
}

} // namespace llgo
