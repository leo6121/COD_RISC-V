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
  branchCtrl.io := DontCare
  branchAdd.io  := DontCare

  immGen.io.instruction := io.imem.instruction

  io.imem.address := pc

  pcPlusFour.io.inputx := pc
  pcPlusFour.io.inputy := 4.U

  val instruction = io.imem.instruction

  control.io.opcode := instruction(6,0)

  registers.io.readreg1 := instruction(19,15)
  registers.io.readreg2 := instruction(24,20)

  val writereg = instruction(11,7)
  registers.io.writereg := writereg
  registers.io.wen :=  Mux(writereg === 0.U, false.B, true.B)

  aluControl.io.add       := false.B
  aluControl.io.immediate := control.io.immediate
  aluControl.io.funct7    := instruction(31,25)
  aluControl.io.funct3    := instruction(14,12)

  alu.io.operation := aluControl.io.operation
  alu.io.inputx := registers.io.readdata1
  alu.io.inputy := Mux(control.io.immediate, immGen.io.sextImm, registers.io.readdata2)

  when (control.io.toreg === 0.U) {
    registers.io.writedata := alu.io.result
  } .elsewhen (control.io.toreg === 1.U) {
    registers.io.writedata := io.dmem.readdata
  } .otherwise{registers.io.writedata := 0.U}

  io.dmem.address := alu.io.result
  io.dmem.memwrite := control.io.memwrite
  io.dmem.memread := control.io.memread
  io.dmem.writedata := registers.io.readdata2


  pc := pcPlusFour.io.result

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
