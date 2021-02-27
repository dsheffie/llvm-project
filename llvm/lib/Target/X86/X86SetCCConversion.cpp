/// Note: This pass is assumed to run on SSA machine code.
//
//===----------------------------------------------------------------------===//
//
//  External interfaces:
//      FunctionPass *llvm::createX86SetCCConverterPass();
//      bool X86SetCCConverterPass::runOnMachineFunction(MachineFunction &MF);
//
//===----------------------------------------------------------------------===//

#include "X86.h"
#include "X86InstrInfo.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineLoopInfo.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSchedule.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/InitializePasses.h"
#include "llvm/MC/MCSchedule.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <cassert>
#include <iterator>
#include <utility>

using namespace llvm;

#define DEBUG_TYPE "x86-setcc-conversion"

//STATISTIC(NumOfSkippedCmovGroups, "Number of unsupported CMOV-groups");
//STATISTIC(NumOfCmovGroupCandidate, "Number of CMOV-group candidates");
//STATISTIC(NumOfLoopCandidate, "Number of CMOV-conversion profitable loops");
//STATISTIC(NumOfOptimizedCmovGroups, "Number of optimized CMOV-groups");

namespace {

/// Converts X86 cmov instructions into branches when profitable.
class X86SetCCConverterPass : public MachineFunctionPass {
public:
  X86SetCCConverterPass() : MachineFunctionPass(ID) { }

  StringRef getPassName() const override { return "X86 setcc Conversion"; }
  bool runOnMachineFunction(MachineFunction &MF) override;

  /// Pass identification, replacement for typeid.
  static char ID;

private:
  MachineRegisterInfo *MRI = nullptr;
  const TargetInstrInfo *TII = nullptr;
  const TargetRegisterInfo *TRI = nullptr;
  void convertSetCCInstsToBranches(MachineInstr &MI) const;
};

} // end anonymous namespace

char X86SetCCConverterPass::ID = 0;


bool X86SetCCConverterPass::runOnMachineFunction(MachineFunction &MF) {
  if (skipFunction(MF.getFunction()))
    return false;

  LLVM_DEBUG(dbgs() << "********** " << getPassName() << " : " << MF.getName()
                    << "**********\n");

  bool Changed = false;
  const TargetSubtargetInfo &STI = MF.getSubtarget();
  MRI = &MF.getRegInfo();
  TII = STI.getInstrInfo();
  TRI = STI.getRegisterInfo();

 retry:
  for(MachineBasicBlock &MBB : MF) {
    for(MachineInstr &MI : MBB) {
      X86::CondCode CC = X86::getCondFromSETCC(MI);
      if(CC != X86::COND_INVALID) {
	//llvm::errs() << MI << "\n";
	convertSetCCInstsToBranches(MI);
	Changed = true;
	goto retry;
      }
    }
  }
  
  //convertCmovInstsToBranches(Group);
  //}
  return Changed;
}


static bool checkEFLAGSLive(MachineInstr *MI) {
  if (MI->killsRegister(X86::EFLAGS))
    return false;

  // The EFLAGS operand of MI might be missing a kill marker.
  // Figure out whether EFLAGS operand should LIVE after MI instruction.
  MachineBasicBlock *BB = MI->getParent();
  MachineBasicBlock::iterator ItrMI = MI;

  // Scan forward through BB for a use/def of EFLAGS.
  for (auto I = std::next(ItrMI), E = BB->end(); I != E; ++I) {
    if (I->readsRegister(X86::EFLAGS))
      return true;
    if (I->definesRegister(X86::EFLAGS))
      return false;
  }

  // We hit the end of the block, check whether EFLAGS is live into a successor.
  for (auto I = BB->succ_begin(), E = BB->succ_end(); I != E; ++I) {
    if ((*I)->isLiveIn(X86::EFLAGS))
      return true;
  }

  return false;
}

void X86SetCCConverterPass::convertSetCCInstsToBranches(MachineInstr & MI) const {

  // Before
  // -----
  // MBB:
  //   cond = cmp ...
  //   v1 = setcc cond
  //
  // After
  // -----
  // MBB:
  //   cond = cmp ...
  //   c0 = 0
  //   jge %SinkMBB
  //
  // FalseMBB:
  //   c1 = 1
  //   jmp %SinkMBB
  //
  // SinkMBB:
  //   %v1 = phi[%f1, %FalseMBB], [%t1, %MBB]
  //                                          ; previous Phi instruction result

  DebugLoc DL = MI.getDebugLoc();

  X86::CondCode CC = X86::CondCode(X86::getCondFromSETCC(MI));

  MachineBasicBlock *MBB = MI.getParent();
  MachineFunction::iterator It = ++MBB->getIterator();
  MachineFunction *F = MBB->getParent();
  const BasicBlock *BB = MBB->getBasicBlock();

  MachineBasicBlock *FalseMBB = F->CreateMachineBasicBlock(BB);
  MachineBasicBlock *SinkMBB = F->CreateMachineBasicBlock(BB);
  F->insert(It, FalseMBB);
  F->insert(It, SinkMBB);

  // If the EFLAGS register isn't dead in the terminator, then claim that it's
  // live into the sink and copy blocks.
  if (checkEFLAGSLive(&MI)) {
    FalseMBB->addLiveIn(X86::EFLAGS);
    SinkMBB->addLiveIn(X86::EFLAGS);
  }

  // Transfer the remainder of BB and its successor edges to SinkMBB.
  SinkMBB->splice(SinkMBB->begin(), MBB,
                  std::next(MachineBasicBlock::iterator(MI)), MBB->end());
  SinkMBB->transferSuccessorsAndUpdatePHIs(MBB);

  // Add the false and sink blocks as its successors.
  MBB->addSuccessor(FalseMBB);
  MBB->addSuccessor(SinkMBB);

  Register destReg = MI.getOperand(0).getReg();
  Register zeroReg = MRI->createVirtualRegister(&X86::GR32RegClass);
  Register oneReg = MRI->createVirtualRegister(&X86::GR32RegClass);
  Register phiReg = MRI->createVirtualRegister(&X86::GR32RegClass);
  auto CIT = MachineBasicBlock::iterator(MI);
  
  BuildMI(*MBB, CIT, DL, TII->get(X86::MOV32ri), zeroReg).addImm(0);
  BuildMI(*MBB, CIT, DL, TII->get(X86::JCC_1)).addMBB(FalseMBB).addImm(CC);
  BuildMI(*MBB, CIT, DL, TII->get(X86::JMP_1)).addMBB(SinkMBB);

  //llvm::errs() << *MBB;

  auto FIT = FalseMBB->begin();
  BuildMI(*FalseMBB, FIT, DL, TII->get(X86::MOV32ri), oneReg).addImm(1);
  BuildMI(*FalseMBB, FIT, DL, TII->get(X86::JMP_1)).addMBB(SinkMBB);

  
  // Add the sink block to the false block successors.
  FalseMBB->addSuccessor(SinkMBB);

  //llvm::errs() << *FalseMBB;

  auto SIT = SinkMBB->begin();
  BuildMI(*SinkMBB, SIT, DL, TII->get(X86::PHI), phiReg)
    .addReg(oneReg)
    .addMBB(FalseMBB)
    .addReg(zeroReg)
    .addMBB(MBB);

  
  BuildMI(*SinkMBB, SIT, DL, TII->get(X86::COPY), destReg)
    .addReg(phiReg, 0, X86::sub_8bit);  

  //llvm::errs() << *SinkMBB;
  
  // Now remove the CMOV(s).
  MBB->erase(MI);
  
}

INITIALIZE_PASS_BEGIN(X86SetCCConverterPass, DEBUG_TYPE, "X86 setcc Conversion",false, false)
INITIALIZE_PASS_END(X86SetCCConverterPass, DEBUG_TYPE, "X86 setcc Conversion", false, false)

FunctionPass *llvm::createX86SetCCConverterPass() {
  return new X86SetCCConverterPass();
}
