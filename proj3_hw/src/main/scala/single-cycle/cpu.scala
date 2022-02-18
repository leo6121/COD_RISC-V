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

  val pcaddr = Wire(UInt(32.W))
  val pcPlusresult = Wire(UInt(32.W))
  
  val instruction = Wire(UInt(32.W))

  val readdata1 = Wire(UInt(32.W))
  val readdata2 = Wire(UInt(32.W))
  
  val memread = Wire(Bool())
  val toreg = Wire(UInt(2.W))
  val add = Wire(Bool())
  val regwrite = Wire(Bool())
  val immediate = Wire(Bool())
  
  val immgen = Wire(UInt(32.W))
  
  val operation = Wire(UInt(4.W))
  
  val aluresult = Wire(UInt(32.W))
  
  val readdata = Wire(UInt(32.W))

  pcaddr := pc  
  pc := pcPlusresult
  
  pcPlusFour.io.inputx := pcaddr
  pcPlusFour.io.inputy := 4.U
  pcPlusresult := pcPlusFour.io.result

  io.imem.address := pcaddr
  instruction := io.imem.instruction

  registers.io.readreg1 := instruction(24,20)
  registers.io.readreg2 := instruction(19,15)
  registers.io.writereg := instruction(11,7)
  readdata1 := registers.io.readdata1
  readdata2 := registers.io.readdata2
  registers.io.wen := Mux(registers.io.writereg === 0.U, false.B, true.B)

  control.io.opcode := instruction(6,0)
  toreg := control.io.toreg
  add := control.io.add
  memread := control.io.memread
  immediate := control.io.immediate
  regwrite := control.io.regwrite

  immGen.io.instruction := instruction
  immgen := immGen.io.sextImm

  aluControl.io.add := add
  aluControl.io.immediate := immediate
  aluControl.io.funct3 := instruction(14,12)
  aluControl.io.funct7 := instruction(31,25)
  operation := aluControl.io.operation

  alu.io.operation := operation
  alu.io.inputx := Mux(instruction(6,0) === "b0010111".U, pcaddr, readdata1)
  alu.io.inputy := Mux(immediate, immgen, readdata2)
  aluresult := alu.io.result

  io.dmem.address := aluresult
  io.dmem.memread := memread
  readdata := io.dmem.readdata

  when(toreg === 0.U){
    registers.io.writedata := aluresult
  } .elsewhen(toreg === 1.U){
    registers.io.writedata := readdata
  } .otherwise{registers.io.writedata := 0.U}



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
