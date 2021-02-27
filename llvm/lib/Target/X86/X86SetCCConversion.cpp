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
  // To convert a CMOVcc instruction, we actually have to insert the diamond
  // control-flow pattern.  The incoming instruction knows the destination vreg
  // to set, the condition code register to branch on, the true/false values to
  // select between, and a branch opcode to use.

  // Before
  // -----
  // MBB:
  //   cond = cmp ...
  //   v1 = CMOVge t1, f1, cond
  //   v2 = CMOVlt t2, f2, cond
  //   v3 = CMOVge v1, f3, cond
  //
  // After
  // -----
  // MBB:
  //   cond = cmp ...
  //   jge %SinkMBB
  //
  // FalseMBB:
  //   jmp %SinkMBB
  //
  // SinkMBB:
  //   %v1 = phi[%f1, %FalseMBB], [%t1, %MBB]
  //   %v2 = phi[%t2, %FalseMBB], [%f2, %MBB] ; For CMOV with OppCC switch
  //                                          ; true-value with false-value
  //   %v3 = phi[%f3, %FalseMBB], [%t1, %MBB] ; Phi instruction cannot use
  //                                          ; previous Phi instruction result
#if 0
  MachineInstr &MI = *Group.front();
  MachineInstr *LastCMOV = Group.back();
  DebugLoc DL = MI.getDebugLoc();

  X86::CondCode CC = X86::CondCode(X86::getCondFromCMov(MI));
  X86::CondCode OppCC = X86::GetOppositeBranchCondition(CC);
  // Potentially swap the condition codes so that any memory operand to a CMOV
  // is in the *false* position instead of the *true* position. We can invert
  // any non-memory operand CMOV instructions to cope with this and we ensure
  // memory operand CMOVs are only included with a single condition code.
  if (llvm::any_of(Group, [&](MachineInstr *I) {
        return I->mayLoad() && X86::getCondFromCMov(*I) == CC;
      }))
    std::swap(CC, OppCC);

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
  if (checkEFLAGSLive(LastCMOV)) {
    FalseMBB->addLiveIn(X86::EFLAGS);
    SinkMBB->addLiveIn(X86::EFLAGS);
  }

  // Transfer the remainder of BB and its successor edges to SinkMBB.
  SinkMBB->splice(SinkMBB->begin(), MBB,
                  std::next(MachineBasicBlock::iterator(LastCMOV)), MBB->end());
  SinkMBB->transferSuccessorsAndUpdatePHIs(MBB);

  // Add the false and sink blocks as its successors.
  MBB->addSuccessor(FalseMBB);
  MBB->addSuccessor(SinkMBB);

  // Create the conditional branch instruction.
  BuildMI(MBB, DL, TII->get(X86::JCC_1)).addMBB(SinkMBB).addImm(CC);

  // Add the sink block to the false block successors.
  FalseMBB->addSuccessor(SinkMBB);

  MachineInstrBuilder MIB;
  MachineBasicBlock::iterator MIItBegin = MachineBasicBlock::iterator(MI);
  MachineBasicBlock::iterator MIItEnd =
      std::next(MachineBasicBlock::iterator(LastCMOV));
  MachineBasicBlock::iterator FalseInsertionPoint = FalseMBB->begin();
  MachineBasicBlock::iterator SinkInsertionPoint = SinkMBB->begin();

  // First we need to insert an explicit load on the false path for any memory
  // operand. We also need to potentially do register rewriting here, but it is
  // simpler as the memory operands are always on the false path so we can
  // simply take that input, whatever it is.
  DenseMap<unsigned, unsigned> FalseBBRegRewriteTable;
  for (MachineBasicBlock::iterator MIIt = MIItBegin; MIIt != MIItEnd;) {
    auto &MI = *MIIt++;
    // Skip any CMOVs in this group which don't load from memory.
    if (!MI.mayLoad()) {
      // Remember the false-side register input.
      Register FalseReg =
          MI.getOperand(X86::getCondFromCMov(MI) == CC ? 1 : 2).getReg();
      // Walk back through any intermediate cmovs referenced.
      while (true) {
        auto FRIt = FalseBBRegRewriteTable.find(FalseReg);
        if (FRIt == FalseBBRegRewriteTable.end())
          break;
        FalseReg = FRIt->second;
      }
      FalseBBRegRewriteTable[MI.getOperand(0).getReg()] = FalseReg;
      continue;
    }

    // The condition must be the *opposite* of the one we've decided to branch
    // on as the branch will go *around* the load and the load should happen
    // when the CMOV condition is false.
    assert(X86::getCondFromCMov(MI) == OppCC &&
           "Can only handle memory-operand cmov instructions with a condition "
           "opposite to the selected branch direction.");

    // The goal is to rewrite the cmov from:
    //
    //   MBB:
    //     %A = CMOVcc %B (tied), (mem)
    //
    // to
    //
    //   MBB:
    //     %A = CMOVcc %B (tied), %C
    //   FalseMBB:
    //     %C = MOV (mem)
    //
    // Which will allow the next loop to rewrite the CMOV in terms of a PHI:
    //
    //   MBB:
    //     JMP!cc SinkMBB
    //   FalseMBB:
    //     %C = MOV (mem)
    //   SinkMBB:
    //     %A = PHI [ %C, FalseMBB ], [ %B, MBB]

    // Get a fresh register to use as the destination of the MOV.
    const TargetRegisterClass *RC = MRI->getRegClass(MI.getOperand(0).getReg());
    Register TmpReg = MRI->createVirtualRegister(RC);

    SmallVector<MachineInstr *, 4> NewMIs;
    bool Unfolded = TII->unfoldMemoryOperand(*MBB->getParent(), MI, TmpReg,
                                             /*UnfoldLoad*/ true,
                                             /*UnfoldStore*/ false, NewMIs);
    (void)Unfolded;
    assert(Unfolded && "Should never fail to unfold a loading cmov!");

    // Move the new CMOV to just before the old one and reset any impacted
    // iterator.
    auto *NewCMOV = NewMIs.pop_back_val();
    assert(X86::getCondFromCMov(*NewCMOV) == OppCC &&
           "Last new instruction isn't the expected CMOV!");
    LLVM_DEBUG(dbgs() << "\tRewritten cmov: "; NewCMOV->dump());
    MBB->insert(MachineBasicBlock::iterator(MI), NewCMOV);
    if (&*MIItBegin == &MI)
      MIItBegin = MachineBasicBlock::iterator(NewCMOV);

    // Sink whatever instructions were needed to produce the unfolded operand
    // into the false block.
    for (auto *NewMI : NewMIs) {
      LLVM_DEBUG(dbgs() << "\tRewritten load instr: "; NewMI->dump());
      FalseMBB->insert(FalseInsertionPoint, NewMI);
      // Re-map any operands that are from other cmovs to the inputs for this block.
      for (auto &MOp : NewMI->uses()) {
        if (!MOp.isReg())
          continue;
        auto It = FalseBBRegRewriteTable.find(MOp.getReg());
        if (It == FalseBBRegRewriteTable.end())
          continue;

        MOp.setReg(It->second);
        // This might have been a kill when it referenced the cmov result, but
        // it won't necessarily be once rewritten.
        // FIXME: We could potentially improve this by tracking whether the
        // operand to the cmov was also a kill, and then skipping the PHI node
        // construction below.
        MOp.setIsKill(false);
      }
    }
    MBB->erase(MachineBasicBlock::iterator(MI),
               std::next(MachineBasicBlock::iterator(MI)));

    // Add this PHI to the rewrite table.
    FalseBBRegRewriteTable[NewCMOV->getOperand(0).getReg()] = TmpReg;
  }

  // As we are creating the PHIs, we have to be careful if there is more than
  // one.  Later CMOVs may reference the results of earlier CMOVs, but later
  // PHIs have to reference the individual true/false inputs from earlier PHIs.
  // That also means that PHI construction must work forward from earlier to
  // later, and that the code must maintain a mapping from earlier PHI's
  // destination registers, and the registers that went into the PHI.
  DenseMap<unsigned, std::pair<unsigned, unsigned>> RegRewriteTable;

  for (MachineBasicBlock::iterator MIIt = MIItBegin; MIIt != MIItEnd; ++MIIt) {
    Register DestReg = MIIt->getOperand(0).getReg();
    Register Op1Reg = MIIt->getOperand(1).getReg();
    Register Op2Reg = MIIt->getOperand(2).getReg();

    // If this CMOV we are processing is the opposite condition from the jump we
    // generated, then we have to swap the operands for the PHI that is going to
    // be generated.
    if (X86::getCondFromCMov(*MIIt) == OppCC)
      std::swap(Op1Reg, Op2Reg);

    auto Op1Itr = RegRewriteTable.find(Op1Reg);
    if (Op1Itr != RegRewriteTable.end())
      Op1Reg = Op1Itr->second.first;

    auto Op2Itr = RegRewriteTable.find(Op2Reg);
    if (Op2Itr != RegRewriteTable.end())
      Op2Reg = Op2Itr->second.second;

    //  SinkMBB:
    //   %Result = phi [ %FalseValue, FalseMBB ], [ %TrueValue, MBB ]
    //  ...
    MIB = BuildMI(*SinkMBB, SinkInsertionPoint, DL, TII->get(X86::PHI), DestReg)
              .addReg(Op1Reg)
              .addMBB(FalseMBB)
              .addReg(Op2Reg)
              .addMBB(MBB);
    (void)MIB;
    LLVM_DEBUG(dbgs() << "\tFrom: "; MIIt->dump());
    LLVM_DEBUG(dbgs() << "\tTo: "; MIB->dump());

    // Add this PHI to the rewrite table.
    RegRewriteTable[DestReg] = std::make_pair(Op1Reg, Op2Reg);
  }

  // Now remove the CMOV(s).
  MBB->erase(MIItBegin, MIItEnd);
#endif
}

INITIALIZE_PASS_BEGIN(X86SetCCConverterPass, DEBUG_TYPE, "X86 setcc Conversion",false, false)
INITIALIZE_PASS_END(X86SetCCConverterPass, DEBUG_TYPE, "X86 setcc Conversion", false, false)

FunctionPass *llvm::createX86SetCCConverterPass() {
  return new X86SetCCConverterPass();
}
