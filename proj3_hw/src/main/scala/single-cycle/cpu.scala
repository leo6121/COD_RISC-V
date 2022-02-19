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
  branchAdd.io := DontCare

  val pcPlusresult = pcPlusFour.io.result
  
  val instruction = io.imem.instruction

  val readdata1 = registers.io.readdata1
  val readdata2 = registers.io.readdata2

  val memread = control.io.memread
  val memwrite = control.io.memwrite
  val toreg = control.io.toreg
  val add = control.io.add
  val regwrite = control.io.regwrite
  val immediate = control.io.immediate
  val alusrc1 = control.io.alusrc1

  val immgen = immGen.io.sextImm
  
  val operation = aluControl.io.operation
  
  val aluresult = alu.io.result
  
  val readdata = io.dmem.readdata

  io.imem.address := pc  
  pc := pcPlusresult
  
  pcPlusFour.io.inputx := pc
  pcPlusFour.io.inputy := 4.U

  registers.io.readreg1 := instruction(19,15)
  registers.io.readreg2 := instruction(24,20)
  registers.io.writereg := instruction(11,7)
  registers.io.wen := Mux(registers.io.writereg === 0.U, false.B, true.B)

  control.io.opcode := instruction(6,0)

  immGen.io.instruction := instruction

  aluControl.io.add := add
  aluControl.io.immediate := immediate
  aluControl.io.funct3 := instruction(14,12)
  aluControl.io.funct7 := instruction(31,25)

  alu.io.operation := operation
  when (alusrc1 === 0.U) {
    alu.io.inputx := readdata1
  } .elsewhen (alusrc1 === 1.U) {
    alu.io.inputx := pc
  } .otherwise {alu.io.inputx := 0.U}
  alu.io.inputy := Mux(immediate, immgen, readdata2)

  io.dmem.address := aluresult
  io.dmem.memread := memread
  io.dmem.memwrite := memwrite

  when(toreg === 0.U){
    registers.io.writedata := aluresult
  } .elsewhen(toreg === 1.U){
    registers.io.writedata := readdata
  } .elsewhen(toreg === 2.U){
    registers.io.writedata := pcPlusresult
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
