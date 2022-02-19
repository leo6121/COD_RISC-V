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
  val instruction = io.imem.instruction

  io.imem.address := pc  
  when (control.io.jump === 0.U){
    pc := pcPlusFour.io.result
  } .elsewhen(control.io.jump === 2.U){
    pc := branchAdd.io.result
  } .otherwise {pc := alu.io.result}

  pcPlusFour.io.inputx := pc
  pcPlusFour.io.inputy := 4.U

  registers.io.readreg1 := instruction(19,15)
  registers.io.readreg2 := instruction(24,20)
  registers.io.writereg := instruction(11,7)
  registers.io.wen := Mux(registers.io.writereg === 0.U, false.B, true.B)

  control.io.opcode := instruction(6,0)

  branchCtrl.io.funct3 := instruction(14,12)
  branchCtrl.io.inputx := registers.io.readdata1
  branchCtrl.io.inputy := registers.io.readdata2
  branchCtrl.io.branch := control.io.branch

  branchAdd.io.inputx := pc
  branchAdd.io.inputy := Mux(branchCtrl.io.taken, immGen.io.sextImm, 4.U)

  immGen.io.instruction := instruction

  aluControl.io.add := control.io.add
  aluControl.io.immediate := control.io.immediate
  aluControl.io.funct3 := instruction(14,12)
  aluControl.io.funct7 := instruction(31,25)

  alu.io.operation := aluControl.io.operation
  when (control.io.alusrc1 === 0.U) {
    alu.io.inputx := registers.io.readdata1
  } .elsewhen (control.io.alusrc1 === 1.U) {
    alu.io.inputx := pc
  } .otherwise {alu.io.inputx := 0.U}
  alu.io.inputy := Mux(control.io.immediate, immGen.io.sextImm, registers.io.readdata2)

  io.dmem.address := alu.io.result
  io.dmem.memread := control.io.memread
  io.dmem.memwrite := control.io.memwrite

  when(control.io.toreg === 0.U){
    registers.io.writedata := alu.io.result
  } .elsewhen(control.io.toreg === 1.U){
    registers.io.writedata := io.dmem.readdata
  } .elsewhen(control.io.toreg === 2.U){
    registers.io.writedata := pcPlusFour.io.result
  } .otherwise {registers.io.writedata := 0.U}



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
