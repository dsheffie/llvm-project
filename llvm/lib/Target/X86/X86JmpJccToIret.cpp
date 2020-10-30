//===- X86JmpJccToIret.cpp 

#include "X86.h"
#include "X86InstrBuilder.h"
#include "X86InstrInfo.h"
#include "X86Subtarget.h"
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


#define JCC2IRET_DESC "Replace jmps and Jccs with IRET"
#define JCC2IRET_NAME "x86-jmp-and-jcc-to-iret"

#define DEBUG_TYPE JCC2IRET_NAME

namespace {

class Jcc2IretPass : public MachineFunctionPass {
  bool run(MachineBasicBlock &MBB);
  
public:
  static char ID;

  Jcc2IretPass(): MachineFunctionPass(ID) { }

  StringRef getPassName() const override { return JCC2IRET_DESC; }

  /// Loop over all of the basic blocks, replacing EVEX instructions
  /// by equivalent VEX instructions when possible for reducing code size.
  bool runOnMachineFunction(MachineFunction &MF) override;


private:
  /// Machine instruction info used throughout the class.
  const X86InstrInfo *TII = nullptr;
  MachineRegisterInfo *MRI = nullptr;
  const X86Subtarget *Subtarget = nullptr;
  const X86RegisterInfo *TRI = nullptr;
};

} // end anonymous namespace

char Jcc2IretPass::ID = 0;

/* 
                "mov %%ss, %0\n\t"
                "pushq %q0\n\t"
                "pushq %%rsp\n\t"
                "addq $8, (%%rsp)\n\t"
                "pushfq\n\t"
                "mov %%cs, %0\n\t"
                "pushq %q0\n\t"
                "pushq $1f\n\t"
                "iretq\n\t"
                "1:"
*/

bool Jcc2IretPass::run(MachineBasicBlock &MBB) {
  MachineBasicBlock::iterator termIt = MBB.getFirstTerminator();
  bool changed = false;
  while(termIt != MBB.end()) {
    unsigned op = termIt->getOpcode();
    if((op == X86::JMP_1) or (op == X86::JMP_4)) {

      unsigned reg = MRI->createVirtualRegister(&X86::GR64RegClass);
      //unsigned rsp = MRI->createVirtualRegister(&X86::GR64RegClass);

      /* save initial rsp pointer */
      //BuildMI(MBB, termIt, DebugLoc(), TII->get(X86::MOV64rr))
      // .addReg(reg, RegState::Define)
      // .addReg(X86::RSP);

      //push ss
      BuildMI(MBB, termIt, DebugLoc(), TII->get(X86::MOV64rs))
	.addReg(reg, RegState::Define)
	.addReg(X86::SS);
      
      BuildMI(MBB, termIt, DebugLoc(), TII->get(X86::PUSH64r))
	.addReg(reg, RegState::Kill);

      //push stack pointer
      //BuildMI(MBB, termIt, DebugLoc(), TII->get(X86::PUSH64r))
      //	.addReg(rsp, RegState::Kill);


      //push flags
      BuildMI(MBB, termIt, DebugLoc(), TII->get(X86::PUSHF64));

      //push cs
      BuildMI(MBB, termIt, DebugLoc(), TII->get(X86::MOV64rs))
       	.addReg(reg, RegState::Define)
       	.addReg(X86::CS);
      
      BuildMI(MBB, termIt, DebugLoc(), TII->get(X86::PUSH64r))
       	.addReg(reg, RegState::Kill);


      //push target


      //for(auto s : MBB.successors()) {
      //s->setLabelMustBeEmitted();
      //}
      changed = true;
      break;
    }
    ++termIt;
  }
  
  return changed;
}

bool Jcc2IretPass::runOnMachineFunction(MachineFunction &MF) {
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


INITIALIZE_PASS(Jcc2IretPass, JCC2IRET_NAME, JCC2IRET_DESC, false, false)

FunctionPass *llvm::createX86JmpAndJccToIretPass() {
  return new Jcc2IretPass();
}
