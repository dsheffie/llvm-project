//===- X86JmpJccToIret.cpp 

#include "MCTargetDesc/X86BaseInfo.h"
#include "MCTargetDesc/X86InstComments.h"
#include "X86.h"
#include "X86InstrInfo.h"
#include "X86Subtarget.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/CodeGen/MachineFunction.h"
#include "llvm/CodeGen/MachineFunctionPass.h"
#include "llvm/CodeGen/MachineInstr.h"
#include "llvm/CodeGen/MachineOperand.h"
#include "llvm/MC/MCInstrDesc.h"
#include "llvm/Pass.h"
#include <cassert>
#include <cstdint>

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
  const TargetRegisterInfo *TRI = nullptr;
};

} // end anonymous namespace

char Jcc2IretPass::ID = 0;

bool Jcc2IretPass::run(MachineBasicBlock &MBB) {
  return false;
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
