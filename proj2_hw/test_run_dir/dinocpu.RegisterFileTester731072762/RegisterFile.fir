;buildInfoPackage: chisel3, version: 3.2.8, scalaVersion: 2.12.10, sbtVersion: 1.3.2
circuit RegisterFile : 
  module RegisterFile : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip readreg1 : UInt<5>, flip readreg2 : UInt<5>, flip writereg : UInt<5>, flip writedata : UInt<32>, flip wen : UInt<1>, readdata1 : UInt<32>, readdata2 : UInt<32>}
    
    reg regs : UInt<32>[32], clock @[register-file.scala 28:17]
    when io.wen : @[register-file.scala 31:17]
      regs[io.writereg] <= io.writedata @[register-file.scala 32:23]
      skip @[register-file.scala 31:17]
    io.readdata1 <= regs[io.readreg1] @[register-file.scala 37:16]
    io.readdata2 <= regs[io.readreg2] @[register-file.scala 38:16]
    
