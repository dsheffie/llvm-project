#include "X86.h"
#include "X86InstrBuilder.h"
#include "X86InstrInfo.h"
#include "X86Subtarget.h"
#include "X86InstrFoldTables.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/ADT/ScopeExit.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallSet.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/SparseBitVector.h"
#include "llvm/ADT/Statistic.h"
#include "llvm/CodeGen/MachineBasicBlock.h"
#include "llvm/CodeGen/MachineConstantPool.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineInstrBuilder.h"
#include "llvm/CodeGen/MachineModuleInfo.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/CodeGen/MachineRegisterInfo.h"
#include "llvm/CodeGen/MachineSSAUpdater.h"
#include "llvm/CodeGen/TargetInstrInfo.h"
#include "llvm/CodeGen/TargetRegisterInfo.h"
#include "llvm/CodeGen/TargetSchedule.h"
#include "llvm/CodeGen/TargetSubtargetInfo.h"
#include "llvm/IR/DebugLoc.h"
#include "llvm/MC/MCSchedule.h"
#include "llvm/Pass.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include <algorithm>
#include <cassert>
#include <iterator>
#include <utility>

using namespace llvm;

#define JCC2IRET_DESC "Unfold load-ops"
#define JCC2IRET_NAME "x86-unfolder"

#define DEBUG_TYPE JCC2IRET_NAME


namespace {

class X86UnfoldPass : public MachineFunctionPass {
  bool run(MachineBasicBlock &MBB);
  
public:
  static char ID;

  X86UnfoldPass(): MachineFunctionPass(ID) { }

  StringRef getPassName() const override { return JCC2IRET_DESC; }

  bool runOnMachineFunction(MachineFunction &MF) override;


private:
  /// Machine instruction info used throughout the class.
  const X86InstrInfo *TII = nullptr;
  MachineRegisterInfo *MRI = nullptr;
  const X86Subtarget *Subtarget = nullptr;
  const X86RegisterInfo *TRI = nullptr;

};

} // end anonymous namespace

char X86UnfoldPass::ID = 0;


bool X86UnfoldPass::run(MachineBasicBlock &MBB) {
  bool changed = false;
  MachineFunction *MF = MBB.getParent();
  auto It = MBB.begin();
  while(It != MBB.end()) {
    MachineInstr &MI = *It;
    //check if the instruction may load
    unsigned op = MI.getOpcode(), LoadRegIndex, NewOpc;

    if(MI.mayLoad() && MI.mayStore()) {
      ++It;
      continue;
    }

    /* copied out of TwoAddressInstructionPass.cpp */
    NewOpc = TII->getOpcodeAfterMemoryUnfold(op, /*UnfoldLoad=*/true, /*UnfoldStore=*/false, &LoadRegIndex);
    if (NewOpc == 0)  {
      ++It;
      continue;
    }
    const MCInstrDesc &UnfoldMCID = TII->get(NewOpc);
    if (UnfoldMCID.getNumDefs() != 1) {
      continue;
    }
    // Get a fresh register to use as the destination of the MOV.
    const TargetRegisterClass *RC = TRI->getAllocatableClass(TII->getRegClass(UnfoldMCID, LoadRegIndex, TRI, *MF));
    Register TmpReg = MRI->createVirtualRegister(RC);

    SmallVector<MachineInstr *, 2> NewMIs;
    bool Unfolded = TII->unfoldMemoryOperand(*MF, MI, TmpReg,
                                             /*UnfoldLoad*/ true,
                                             /*UnfoldStore*/ false, NewMIs);
    (void)Unfolded;
    assert(Unfolded && "Must unfold");

    bool generatedSameOpC = false;
    for (auto *NewMI : NewMIs) {
      generatedSameOpC |= (NewMI->getOpcode() == op);
    }
    if(generatedSameOpC) {
      ++It;
      continue;
    }
    
    for (auto *NewMI : NewMIs) {
      It = MBB.insertAfter(It, NewMI);
    }
    MI.eraseFromParent();
    changed = true;
  }
  
  return changed;
}

bool X86UnfoldPass::runOnMachineFunction(MachineFunction &MF) {
  Subtarget = &MF.getSubtarget<X86Subtarget>();
  MRI = &MF.getRegInfo();
  TII = Subtarget->getInstrInfo();
  TRI = Subtarget->getRegisterInfo();
  TII = MF.getSubtarget<X86Subtarget>().getInstrInfo();
  
  if(not(Subtarget->is64Bit())) {
    return false;
  }
  bool Changed = false;
  for (MachineBasicBlock &MBB : MF) {
    bool C = run(MBB);
    //if(C) {
    //llvm::errs() << MBB;
    //}
    Changed |= C;
  }

  return Changed;
}


INITIALIZE_PASS(X86UnfoldPass, JCC2IRET_NAME, JCC2IRET_DESC, false, false)

FunctionPass *llvm::createX86UnfoldPass() {
  return new X86UnfoldPass();
}
