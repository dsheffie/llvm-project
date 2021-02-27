//===-- X86TargetMachine.cpp - Define TargetMachine for the X86 -----------===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the X86 specific subclass of TargetMachine.
//
//===----------------------------------------------------------------------===//

#include "X86TargetMachine.h"
#include "MCTargetDesc/X86MCTargetDesc.h"
#include "TargetInfo/X86TargetInfo.h"
#include "X86.h"
#include "X86CallLowering.h"
#include "X86LegalizerInfo.h"
#include "X86MacroFusion.h"
#include "X86Subtarget.h"
#include "X86TargetObjectFile.h"
#include "X86TargetTransformInfo.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/TargetTransformInfo.h"
#include "llvm/CodeGen/ExecutionDomainFix.h"
#include "llvm/CodeGen/GlobalISel/CallLowering.h"
#include "llvm/CodeGen/GlobalISel/IRTranslator.h"
#include "llvm/CodeGen/GlobalISel/InstructionSelect.h"
#include "llvm/CodeGen/GlobalISel/Legalizer.h"
#include "llvm/CodeGen/GlobalISel/RegBankSelect.h"
#include "llvm/CodeGen/MachineScheduler.h"
#include "llvm/CodeGen/Passes.h"
#include "llvm/CodeGen/TargetPassConfig.h"
#include "llvm/IR/Attributes.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/MC/MCAsmInfo.h"
#include "llvm/Pass.h"
#include "llvm/Support/CodeGen.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Target/TargetLoweringObjectFile.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/Transforms/CFGuard.h"
#include <memory>
#include <string>

using namespace llvm;

static cl::opt<bool> EnableMachineCombinerPass("x86-machine-combiner",
                               cl::desc("Enable the machine combiner pass"),
                               cl::init(true), cl::Hidden);

extern "C" LLVM_EXTERNAL_VISIBILITY void LLVMInitializeX86Target() {
  // Register the target.
  RegisterTargetMachine<X86TargetMachine> X(getTheX86_32Target());
  RegisterTargetMachine<X86TargetMachine> Y(getTheX86_64Target());

  PassRegistry &PR = *PassRegistry::getPassRegistry();
  initializeGlobalISel(PR);
  initializeWinEHStatePassPass(PR);
  initializeFixupBWInstPassPass(PR);
  initializeEvexToVexInstPassPass(PR);
  initializeFixupLEAPassPass(PR);
  initializeFPSPass(PR);
  initializeX86FixupSetCCPassPass(PR);
  initializeX86CallFrameOptimizationPass(PR);
  initializeX86CmovConverterPassPass(PR);
  initializeX86SetCCConverterPassPass(PR);
  initializeX86ExpandPseudoPass(PR);
  initializeX86ExecutionDomainFixPass(PR);
  initializeX86DomainReassignmentPass(PR);
  initializeX86AvoidSFBPassPass(PR);
  initializeX86AvoidTrailingCallPassPass(PR);
  initializeX86SpeculativeLoadHardeningPassPass(PR);
  initializeX86SpeculativeExecutionSideEffectSuppressionPass(PR);
  initializeX86FlagsCopyLoweringPassPass(PR);
  initializeX86LoadValueInjectionLoadHardeningPassPass(PR);
  initializeX86LoadValueInjectionRetHardeningPassPass(PR);
  initializeX86OptimizeLEAPassPass(PR);
  initializeX86PartialReductionPass(PR);
}

static std::unique_ptr<TargetLoweringObjectFile> createTLOF(const Triple &TT) {
  if (TT.isOSBinFormatMachO()) {
    if (TT.getArch() == Triple::x86_64)
      return std::make_unique<X86_64MachoTargetObjectFile>();
    return std::make_unique<TargetLoweringObjectFileMachO>();
  }

  if (TT.isOSBinFormatCOFF())
    return std::make_unique<TargetLoweringObjectFileCOFF>();
  return std::make_unique<X86ELFTargetObjectFile>();
}

static std::string computeDataLayout(const Triple &TT) {
  // X86 is little endian
  std::string Ret = "e";

  Ret += DataLayout::getManglingComponent(TT);
  // X86 and x32 have 32 bit pointers.
  if ((TT.isArch64Bit() &&
       (TT.getEnvironment() == Triple::GNUX32 || TT.isOSNaCl())) ||
      !TT.isArch64Bit())
    Ret += "-p:32:32";

  // Address spaces for 32 bit signed, 32 bit unsigned, and 64 bit pointers.
  Ret += "-p270:32:32-p271:32:32-p272:64:64";

  // Some ABIs align 64 bit integers and doubles to 64 bits, others to 32.
  if (TT.isArch64Bit() || TT.isOSWindows() || TT.isOSNaCl())
    Ret += "-i64:64";
  else if (TT.isOSIAMCU())
    Ret += "-i64:32-f64:32";
  else
    Ret += "-f64:32:64";

  // Some ABIs align long double to 128 bits, others to 32.
  if (TT.isOSNaCl() || TT.isOSIAMCU())
    ; // No f80
  else if (TT.isArch64Bit() || TT.isOSDarwin())
    Ret += "-f80:128";
  else
    Ret += "-f80:32";

  if (TT.isOSIAMCU())
    Ret += "-f128:32";

  // The registers can hold 8, 16, 32 or, in x86-64, 64 bits.
  if (TT.isArch64Bit())
    Ret += "-n8:16:32:64";
  else
    Ret += "-n8:16:32";

  // The stack is aligned to 32 bits on some ABIs and 128 bits on others.
  if ((!TT.isArch64Bit() && TT.isOSWindows()) || TT.isOSIAMCU())
    Ret += "-a:0:32-S32";
  else
    Ret += "-S128";

  return Ret;
}

static Reloc::Model getEffectiveRelocModel(const Triple &TT,
                                           bool JIT,
                                           Optional<Reloc::Model> RM) {
  bool is64Bit = TT.getArch() == Triple::x86_64;
  if (!RM.hasValue()) {
    // JIT codegen should use static relocations by default, since it's
    // typically executed in process and not relocatable.
    if (JIT)
      return Reloc::Static;

    // Darwin defaults to PIC in 64 bit mode and dynamic-no-pic in 32 bit mode.
    // Win64 requires rip-rel addressing, thus we force it to PIC. Otherwise we
    // use static relocation model by default.
    if (TT.isOSDarwin()) {
      if (is64Bit)
        return Reloc::PIC_;
      return Reloc::DynamicNoPIC;
    }
    if (TT.isOSWindows() && is64Bit)
      return Reloc::PIC_;
    return Reloc::Static;
  }

  // ELF and X86-64 don't have a distinct DynamicNoPIC model.  DynamicNoPIC
  // is defined as a model for code which may be used in static or dynamic
  // executables but not necessarily a shared library. On X86-32 we just
  // compile in -static mode, in x86-64 we use PIC.
  if (*RM == Reloc::DynamicNoPIC) {
    if (is64Bit)
      return Reloc::PIC_;
    if (!TT.isOSDarwin())
      return Reloc::Static;
  }

  // If we are on Darwin, disallow static relocation model in X86-64 mode, since
  // the Mach-O file format doesn't support it.
  if (*RM == Reloc::Static && TT.isOSDarwin() && is64Bit)
    return Reloc::PIC_;

  return *RM;
}

static CodeModel::Model getEffectiveX86CodeModel(Optional<CodeModel::Model> CM,
                                                 bool JIT, bool Is64Bit) {
  if (CM) {
    if (*CM == CodeModel::Tiny)
      report_fatal_error("Target does not support the tiny CodeModel", false);
    return *CM;
  }
  if (JIT)
    return Is64Bit ? CodeModel::Large : CodeModel::Small;
  return CodeModel::Small;
}

/// Create an X86 target.
///
X86TargetMachine::X86TargetMachine(const Target &T, const Triple &TT,
                                   StringRef CPU, StringRef FS,
                                   const TargetOptions &Options,
                                   Optional<Reloc::Model> RM,
                                   Optional<CodeModel::Model> CM,
                                   CodeGenOpt::Level OL, bool JIT)
    : LLVMTargetMachine(
          T, computeDataLayout(TT), TT, CPU, FS, Options,
          getEffectiveRelocModel(TT, JIT, RM),
          getEffectiveX86CodeModel(CM, JIT, TT.getArch() == Triple::x86_64),
          OL),
      TLOF(createTLOF(getTargetTriple())), IsJIT(JIT) {
  // On PS4, the "return address" of a 'noreturn' call must still be within
  // the calling function, and TrapUnreachable is an easy way to get that.
  if (TT.isPS4() || TT.isOSBinFormatMachO()) {
    this->Options.TrapUnreachable = true;
    this->Options.NoTrapAfterNoreturn = TT.isOSBinFormatMachO();
  }

  setMachineOutliner(true);

  // x86 supports the debug entry values.
  setSupportsDebugEntryValues(true);

  initAsmInfo();
}

X86TargetMachine::~X86TargetMachine() = default;

const X86Subtarget *
X86TargetMachine::getSubtargetImpl(const Function &F) const {
  Attribute CPUAttr = F.getFnAttribute("target-cpu");
  Attribute TuneAttr = F.getFnAttribute("tune-cpu");
  Attribute FSAttr = F.getFnAttribute("target-features");

  StringRef CPU =
      CPUAttr.isValid() ? CPUAttr.getValueAsString() : (StringRef)TargetCPU;
  StringRef TuneCPU =
      TuneAttr.isValid() ? TuneAttr.getValueAsString() : (StringRef)CPU;
  StringRef FS =
      FSAttr.isValid() ? FSAttr.getValueAsString() : (StringRef)TargetFS;

  SmallString<512> Key;
  // The additions here are ordered so that the definitely short strings are
  // added first so we won't exceed the small size. We append the
  // much longer FS string at the end so that we only heap allocate at most
  // one time.

  // Extract prefer-vector-width attribute.
  unsigned PreferVectorWidthOverride = 0;
  Attribute PreferVecWidthAttr = F.getFnAttribute("prefer-vector-width");
  if (PreferVecWidthAttr.isValid()) {
    StringRef Val = PreferVecWidthAttr.getValueAsString();
    unsigned Width;
    if (!Val.getAsInteger(0, Width)) {
      Key += "prefer-vector-width=";
      Key += Val;
      PreferVectorWidthOverride = Width;
    }
  }

  // Extract min-legal-vector-width attribute.
  unsigned RequiredVectorWidth = UINT32_MAX;
  Attribute MinLegalVecWidthAttr = F.getFnAttribute("min-legal-vector-width");
  if (MinLegalVecWidthAttr.isValid()) {
    StringRef Val = MinLegalVecWidthAttr.getValueAsString();
    unsigned Width;
    if (!Val.getAsInteger(0, Width)) {
      Key += "min-legal-vector-width=";
      Key += Val;
      RequiredVectorWidth = Width;
    }
  }

  // Add CPU to the Key.
  Key += CPU;

  // Add tune CPU to the Key.
  Key += "tune=";
  Key += TuneCPU;

  // Keep track of the start of the feature portion of the string.
  unsigned FSStart = Key.size();

  // FIXME: This is related to the code below to reset the target options,
  // we need to know whether or not the soft float flag is set on the
  // function before we can generate a subtarget. We also need to use
  // it as a key for the subtarget since that can be the only difference
  // between two functions.
  bool SoftFloat =
      F.getFnAttribute("use-soft-float").getValueAsString() == "true";
  // If the soft float attribute is set on the function turn on the soft float
  // subtarget feature.
  if (SoftFloat)
    Key += FS.empty() ? "+soft-float" : "+soft-float,";

  Key += FS;

  // We may have added +soft-float to the features so move the StringRef to
  // point to the full string in the Key.
  FS = Key.substr(FSStart);

  auto &I = SubtargetMap[Key];
  if (!I) {
    // This needs to be done before we create a new subtarget since any
    // creation will depend on the TM and the code generation flags on the
    // function that reside in TargetOptions.
    resetTargetOptions(F);
    I = std::make_unique<X86Subtarget>(
        TargetTriple, CPU, TuneCPU, FS, *this,
        MaybeAlign(Options.StackAlignmentOverride), PreferVectorWidthOverride,
        RequiredVectorWidth);
  }
  return I.get();
}

bool X86TargetMachine::isNoopAddrSpaceCast(unsigned SrcAS,
                                           unsigned DestAS) const {
  assert(SrcAS != DestAS && "Expected different address spaces!");
  if (getPointerSize(SrcAS) != getPointerSize(DestAS))
    return false;
  return SrcAS < 256 && DestAS < 256;
}

//===----------------------------------------------------------------------===//
// X86 TTI query.
//===----------------------------------------------------------------------===//

TargetTransformInfo
X86TargetMachine::getTargetTransformInfo(const Function &F) {
  return TargetTransformInfo(X86TTIImpl(this, F));
}

//===----------------------------------------------------------------------===//
// Pass Pipeline Configuration
//===----------------------------------------------------------------------===//

namespace {

/// X86 Code Generator Pass Configuration Options.
class X86PassConfig : public TargetPassConfig {
public:
  X86PassConfig(X86TargetMachine &TM, PassManagerBase &PM)
    : TargetPassConfig(TM, PM) {}

  X86TargetMachine &getX86TargetMachine() const {
    return getTM<X86TargetMachine>();
  }

  ScheduleDAGInstrs *
  createMachineScheduler(MachineSchedContext *C) const override {
    ScheduleDAGMILive *DAG = createGenericSchedLive(C);
    DAG->addMutation(createX86MacroFusionDAGMutation());
    return DAG;
  }

  ScheduleDAGInstrs *
  createPostMachineScheduler(MachineSchedContext *C) const override {
    ScheduleDAGMI *DAG = createGenericSchedPostRA(C);
    DAG->addMutation(createX86MacroFusionDAGMutation());
    return DAG;
  }

  void addIRPasses() override;
  bool addInstSelector() override;
  bool addIRTranslator() override;
  bool addLegalizeMachineIR() override;
  bool addRegBankSelect() override;
  bool addGlobalInstructionSelect() override;
  bool addILPOpts() override;
  bool addPreISel() override;
  void addMachineSSAOptimization() override;
  void addPreRegAlloc() override;
  void addPostRegAlloc() override;
  void addPreEmitPass() override;
  void addPreEmitPass2() override;
  void addPreSched2() override;

  std::unique_ptr<CSEConfigBase> getCSEConfig() const override;
};

class X86ExecutionDomainFix : public ExecutionDomainFix {
public:
  static char ID;
  X86ExecutionDomainFix() : ExecutionDomainFix(ID, X86::VR128XRegClass) {}
  StringRef getPassName() const override {
    return "X86 Execution Dependency Fix";
  }
};
char X86ExecutionDomainFix::ID;

} // end anonymous namespace

INITIALIZE_PASS_BEGIN(X86ExecutionDomainFix, "x86-execution-domain-fix",
  "X86 Execution Domain Fix", false, false)
INITIALIZE_PASS_DEPENDENCY(ReachingDefAnalysis)
INITIALIZE_PASS_END(X86ExecutionDomainFix, "x86-execution-domain-fix",
  "X86 Execution Domain Fix", false, false)

TargetPassConfig *X86TargetMachine::createPassConfig(PassManagerBase &PM) {
  return new X86PassConfig(*this, PM);
}

void X86PassConfig::addIRPasses() {
  addPass(createAtomicExpandPass());

  TargetPassConfig::addIRPasses();

  if (TM->getOptLevel() != CodeGenOpt::None) {
    addPass(createInterleavedAccessPass());
    addPass(createX86PartialReductionPass());
  }

  // Add passes that handle indirect branch removal and insertion of a retpoline
  // thunk. These will be a no-op unless a function subtarget has the retpoline
  // feature enabled.
  addPass(createIndirectBrExpandPass());

  // Add Control Flow Guard checks.
  const Triple &TT = TM->getTargetTriple();
  if (TT.isOSWindows()) {
    if (TT.getArch() == Triple::x86_64) {
      addPass(createCFGuardDispatchPass());
    } else {
      addPass(createCFGuardCheckPass());
    }
  }
}

bool X86PassConfig::addInstSelector() {
  // Install an instruction selector.
  addPass(createX86ISelDag(getX86TargetMachine(), getOptLevel()));

  // For ELF, cleanup any local-dynamic TLS accesses.
  if (TM->getTargetTriple().isOSBinFormatELF() &&
      getOptLevel() != CodeGenOpt::None)
    addPass(createCleanupLocalDynamicTLSPass());

  addPass(createX86GlobalBaseRegPass());
  return false;
}

bool X86PassConfig::addIRTranslator() {
  addPass(new IRTranslator(getOptLevel()));
  return false;
}

bool X86PassConfig::addLegalizeMachineIR() {
  addPass(new Legalizer());
  return false;
}

bool X86PassConfig::addRegBankSelect() {
  addPass(new RegBankSelect());
  return false;
}

bool X86PassConfig::addGlobalInstructionSelect() {
  addPass(new InstructionSelect());
  return false;
}

bool X86PassConfig::addILPOpts() {
  addPass(&EarlyIfConverterID);
  if (EnableMachineCombinerPass)
    addPass(&MachineCombinerID);
  addPass(createX86CmovConverterPass());
  addPass(createX86SetCCConverterPass());
  return true;
}

bool X86PassConfig::addPreISel() {
  // Only add this pass for 32-bit x86 Windows.
  const Triple &TT = TM->getTargetTriple();
  if (TT.isOSWindows() && TT.getArch() == Triple::x86)
    addPass(createX86WinEHStatePass());
  return true;
}

void X86PassConfig::addPreRegAlloc() {
  if (getOptLevel() != CodeGenOpt::None) {
    addPass(&LiveRangeShrinkID);
    addPass(createX86FixupSetCC());
    addPass(createX86OptimizeLEAs());
    addPass(createX86CallFrameOptimization());
    addPass(createX86AvoidStoreForwardingBlocks());
  }

  addPass(createX86SpeculativeLoadHardeningPass());
  addPass(createX86FlagsCopyLoweringPass());
  addPass(createX86WinAllocaExpander());
}
void X86PassConfig::addMachineSSAOptimization() {
  addPass(createX86DomainReassignmentPass());
  TargetPassConfig::addMachineSSAOptimization();
}

void X86PassConfig::addPostRegAlloc() {
  addPass(createX86FloatingPointStackifierPass());
  // When -O0 is enabled, the Load Value Injection Hardening pass will fall back
  // to using the Speculative Execution Side Effect Suppression pass for
  // mitigation. This is to prevent slow downs due to
  // analyses needed by the LVIHardening pass when compiling at -O0.
  if (getOptLevel() != CodeGenOpt::None)
    addPass(createX86LoadValueInjectionLoadHardeningPass());
}

void X86PassConfig::addPreSched2() { addPass(createX86ExpandPseudoPass()); }

void X86PassConfig::addPreEmitPass() {
  if (getOptLevel() != CodeGenOpt::None) {
    addPass(new X86ExecutionDomainFix());
    addPass(createBreakFalseDeps());
  }

  addPass(createX86IndirectBranchTrackingPass());

  addPass(createX86IssueVZeroUpperPass());

  if (getOptLevel() != CodeGenOpt::None) {
    addPass(createX86FixupBWInsts());
    addPass(createX86PadShortFunctions());
    addPass(createX86FixupLEAs());
  }
  addPass(createX86EvexToVexInsts());
  addPass(createX86DiscriminateMemOpsPass());
  addPass(createX86InsertPrefetchPass());
  addPass(createX86InsertX87waitPass());
}

void X86PassConfig::addPreEmitPass2() {
  const Triple &TT = TM->getTargetTriple();
  const MCAsmInfo *MAI = TM->getMCAsmInfo();

  // The X86 Speculative Execution Pass must run after all control
  // flow graph modifying passes. As a result it was listed to run right before
  // the X86 Retpoline Thunks pass. The reason it must run after control flow
  // graph modifications is that the model of LFENCE in LLVM has to be updated
  // (FIXME: https://bugs.llvm.org/show_bug.cgi?id=45167). Currently the
  // placement of this pass was hand checked to ensure that the subsequent
  // passes don't move the code around the LFENCEs in a way that will hurt the
  // correctness of this pass. This placement has been shown to work based on
  // hand inspection of the codegen output.
  addPass(createX86SpeculativeExecutionSideEffectSuppression());
  addPass(createX86IndirectThunksPass());

  // Insert extra int3 instructions after trailing call instructions to avoid
  // issues in the unwinder.
  if (TT.isOSWindows() && TT.getArch() == Triple::x86_64)
    addPass(createX86AvoidTrailingCallPass());

  // Verify basic block incoming and outgoing cfa offset and register values and
  // correct CFA calculation rule where needed by inserting appropriate CFI
  // instructions.
  if (!TT.isOSDarwin() &&
      (!TT.isOSWindows() ||
       MAI->getExceptionHandlingType() == ExceptionHandling::DwarfCFI))
    addPass(createCFIInstrInserter());
  // Identify valid longjmp targets for Windows Control Flow Guard.
  if (TT.isOSWindows())
    addPass(createCFGuardLongjmpPass());
  addPass(createX86LoadValueInjectionRetHardeningPass());
}

std::unique_ptr<CSEConfigBase> X86PassConfig::getCSEConfig() const {
  return getStandardCSEConfigForOpt(TM->getOptLevel());
}
