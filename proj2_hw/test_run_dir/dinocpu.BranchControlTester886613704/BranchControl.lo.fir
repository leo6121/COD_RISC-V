circuit BranchControl :
  module BranchControl :
    input clock : Clock
    input reset : UInt<1>
    input io_branch : UInt<1>
    input io_funct3 : UInt<3>
    input io_inputx : UInt<32>
    input io_inputy : UInt<32>
    output io_taken : UInt<1>
  
    io_taken <= UInt<1>("h0") @[branch-control.scala 28:12]
