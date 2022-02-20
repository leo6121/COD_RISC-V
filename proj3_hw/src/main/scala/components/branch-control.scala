// Control logic for whether branches are taken or not

package dinocpu

import chisel3._
import chisel3.util._

/**
 * Controls whether or not branches are taken.
 *
 * Input:  branch, true if we are looking at a branch
 * Input:  funct3, the middle three bits of the instruction (12-14). Specifies the
 *         type of branch
 * Input:  inputx, first value (e.g., reg1)
 * Input:  inputy, second value (e.g., reg2)
 * Output: taken, true if the branch is taken.
 */
class BranchControl extends Module {
  val io = IO(new Bundle {
    val branch = Input(Bool())
    val funct3 = Input(UInt(3.W))
    val inputx = Input(UInt(32.W))
    val inputy = Input(UInt(32.W))

    val taken  = Output(Bool())
  })
  when (io.branch === true.B) {
    when (io.funct3 === "b000".U){//beq
      io.taken := Mux(io.inputx === io.inputy, true.B, false.B)
    } .elsewhen (io.funct3 === "b001".U) {//bne
      io.taken := Mux(io.inputx === io.inputy, false.B, true.B)
    }  .elsewhen (io.funct3 === "b100".U) {//blt
      io.taken := Mux(io.inputx.asSInt < io.inputy.asSInt, true.B, false.B)
    } .elsewhen (io.funct3 === "b101".U){//bge
      io.taken := Mux(io.inputx.asSInt < io.inputy.asSInt, false.B, true.B)
    } .elsewhen (io.funct3 === "b110".U){//bltu
      io.taken := Mux(io.inputx < io.inputy, true.B, false.B)
    } .elsewhen (io.funct3 === "b111".U){//bgeu
      io.taken := Mux(io.inputx < io.inputy, false.B, true.B)
    } .otherwise {io.taken := false.B}
  } .otherwise {io.taken := false.B}  

}
