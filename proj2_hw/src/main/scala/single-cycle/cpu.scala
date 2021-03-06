// This file is where all of the CPU components are assembled into the whole CPU

package dinocpu

import chisel3._
import chisel3.util._

/**
 * The main CPU definition that hooks up all of the other components.
 *
 * For more information, see section 4.4 of Patterson and Hennessy
 * This follows figure 4.21
 */
class SingleCycleCPU(implicit val conf: CPUConfig) extends Module {
  val io = IO(new CoreIO())
  io := DontCare

  // All of the structures required
  val pc         = RegInit(0.U)
  val control    = Module(new Control())
  val registers  = Module(new RegisterFile())
  val aluControl = Module(new ALUControl())
  val alu        = Module(new ALU())
  val immGen     = Module(new ImmediateGenerator())
  val branchCtrl = Module(new BranchControl())
  val pcPlusFour = Module(new Adder())
  val branchAdd  = Module(new Adder())
  val (cycleCount, _) = Counter(true.B, 1 << 30)

  // To make the FIRRTL compiler happy. Remove this as you connect up the I/O's

  control.io := DontCare
  immGen.io := DontCare
  branchAdd.io := DontCare
  branchCtrl.io := DontCare

  registers.io.readreg1 := io.imem.instruction(19,15)
  registers.io.readreg2 := io.imem.instruction(24,20)
  registers.io.writereg := io.imem.instruction(11,7)
  registers.io.writedata := alu.io.result

  aluControl.io.funct7 := io.imem.instruction(31,25)
  aluControl.io.funct3 := io.imem.instruction(14,12)
  alu.io.operation := aluControl.io.operation 
  alu.io.inputx := registers.io.readdata1
  alu.io.inputy := registers.io.readdata2

  pcPlusFour.io.inputx := pc
  pcPlusFour.io.inputy := 4.U(32.W)
  pc := pcPlusFour.io.result
  io.imem.address := pc
  
   val instruction = io.imem.instruction


  // Debug / pipeline viewer
  val structures = List(
    (control, "control"),
    (registers, "registers"),
    (aluControl, "aluControl"),
    (alu, "alu"),
    (immGen, "immGen"),
    (branchCtrl, "branchCtrl"),
    (pcPlusFour, "pcPlusFour"),
    (branchAdd, "branchAdd")
  )

  printf("DASM(%x)\n", instruction)
  printf(p"CYCLE=$cycleCount\n")
  printf(p"pc: $pc\n")
  for (structure <- structures) {
    printf(p"${structure._2}: ${structure._1.io}\n")
  }
  printf("\n")

}
