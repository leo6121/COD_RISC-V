;buildInfoPackage: chisel3, version: 3.2.8, scalaVersion: 2.12.10, sbtVersion: 1.3.2
circuit Control : 
  module Control : 
    input clock : Clock
    input reset : UInt<1>
    output io : {flip opcode : UInt<7>, branch : UInt<1>, memread : UInt<1>, toreg : UInt<2>, add : UInt<1>, memwrite : UInt<1>, regwrite : UInt<1>, immediate : UInt<1>, alusrc1 : UInt<2>, jump : UInt<2>}
    
    node _T = and(io.opcode, UInt<7>("h07f")) @[Lookup.scala 31:38]
    node _T_1 = eq(UInt<1>("h00"), _T) @[Lookup.scala 31:38]
    node signals_0 = mux(_T_1, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_1 = mux(_T_1, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_2 = mux(_T_1, UInt<1>("h00"), UInt<2>("h03")) @[Lookup.scala 33:37]
    node signals_3 = mux(_T_1, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_4 = mux(_T_1, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_5 = mux(_T_1, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_6 = mux(_T_1, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_7 = mux(_T_1, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    node signals_8 = mux(_T_1, UInt<1>("h00"), UInt<1>("h00")) @[Lookup.scala 33:37]
    io.branch <= signals_0 @[control.scala 48:13]
    io.memread <= signals_1 @[control.scala 49:14]
    io.toreg <= signals_2 @[control.scala 50:12]
    io.add <= signals_3 @[control.scala 51:10]
    io.memwrite <= signals_4 @[control.scala 52:15]
    io.immediate <= signals_5 @[control.scala 53:16]
    io.regwrite <= signals_6 @[control.scala 54:15]
    io.alusrc1 <= signals_7 @[control.scala 55:14]
    io.jump <= signals_8 @[control.scala 56:11]
    
