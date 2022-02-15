;buildInfoPackage: chisel3, version: 3.2.8, scalaVersion: 2.12.10, sbtVersion: 1.3.2
circuit BranchControl : 
  module BranchControl : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip branch : UInt<1>, flip funct3 : UInt<3>, flip inputx : UInt<32>, flip inputy : UInt<32>, taken : UInt<1>}
    
    io.taken <= UInt<1>("h00") @[branch-control.scala 28:12]
    
