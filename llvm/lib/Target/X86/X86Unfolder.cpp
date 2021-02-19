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
  return changed;
  
  auto It = MBB.begin();
  
  while(It != MBB.end()) {
    MachineInstr &MI = *It;
    //check if the instruction may load
    unsigned op = MI.getOpcode();
#if 1
    switch(op)
      {
      case X86::MOVSX32rm8:	
      case X86::MOVSX32rm16:
      case X86::MOVZX32rm8:
      case X86::MOVZX32rm16:	

      case X86::MOVSX64rm8:
      case X86::MOVSX64rm16:
      case X86::MOVSX64rm32:

	
      case X86::MOV8mi:
      case X86::MOV16mi:
      case X86::MOV32mi:
	
      case X86::MOV8mr:
      case X86::MOV16mr:
      case X86::MOV32mr:
      case X86::MOV64mr:
      case X86::MOV8rm:
      case X86::MOV16rm:
      case X86::MOV32rm:
      case X86::MOV64rm:

      case X86::MOV64mi32:


      case X86::MOVAPSmr:
      case X86::MOVAPSrm:
      case X86::MOVAPDmr:
      case X86::MOVAPDrm:
      case X86::VMOVAPSmr:
      case X86::VMOVAPSrm:
      case X86::VMOVAPDmr:
      case X86::VMOVAPDrm:
	
	
      case X86::MOVSSrm_alt:
      case X86::MOVSDrm_alt:
      case X86::VMOVSSrm_alt:
      case X86::VMOVSDrm_alt:	
	
      case X86::VMOVUPSYrm:
      case X86::VMOVUPSYmr:
	
      case X86::MOVSDmr:
      case X86::MOVSSmr:
      case X86::MOVSDrm:
      case X86::MOVSSrm:
      case X86::MOVUPSrm:
      case X86::MOVUPSmr:
      case X86::MOVUPDrm:
      case X86::MOVUPDmr:

      case X86::VMOVSDmr:
      case X86::VMOVSSmr:
      case X86::VMOVSDrm:
      case X86::VMOVSSrm:
      case X86::VMOVUPSrm:
      case X86::VMOVUPSmr:
      case X86::VMOVUPDrm:
      case X86::VMOVUPDmr:
	
      case X86::CALL64m:
      case X86::JMP64m:
      case X86::TCRETURNmi64:
	++It;
	continue;	
      default:
	break;
      }
#endif
    
    const X86MemoryFoldTableEntry *I = lookupUnfoldTable(op);
    if ((I == nullptr))  {
      ++It;
      continue;
    }
    // Get a fresh register to use as the destination of the MOV.
    const TargetRegisterClass *RC = MRI->getRegClass(MI.getOperand(0).getReg());
    Register TmpReg = MRI->createVirtualRegister(RC);

    SmallVector<MachineInstr *, 4> NewMIs;
    bool Unfolded = TII->unfoldMemoryOperand(*(MBB.getParent()), MI, TmpReg,
                                             /*UnfoldLoad*/ true,
                                             /*UnfoldStore*/ false, NewMIs);
    (void)Unfolded;
    assert(Unfolded && "Must unfold");
    llvm::errs() << "replacing : " << *It << " with \n";
    for (auto *NewMI : NewMIs) {
      It = MBB.insert(It, NewMI);
      llvm::errs() << "\t" << *It << "\n";
    }
    MI.eraseFromParent();

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
    Changed |= run(MBB);
  }
  return Changed;
}


INITIALIZE_PASS(X86UnfoldPass, JCC2IRET_NAME, JCC2IRET_DESC, false, false)

FunctionPass *llvm::createX86UnfoldPass() {
  return new X86UnfoldPass();
}
