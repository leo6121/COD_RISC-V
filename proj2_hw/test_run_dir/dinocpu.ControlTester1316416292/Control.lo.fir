circuit Control :
  module Control :
    input clock : Clock
    input reset : UInt<1>
    input io_opcode : UInt<7>
    output io_branch : UInt<1>
    output io_memread : UInt<1>
    output io_toreg : UInt<2>
    output io_add : UInt<1>
    output io_memwrite : UInt<1>
    output io_regwrite : UInt<1>
    output io_immediate : UInt<1>
    output io_alusrc1 : UInt<2>
    output io_jump : UInt<2>
  
    node _T = and(io_opcode, UInt<7>("h7f")) @[Lookup.scala 31:38]
    node _T_1 = eq(UInt<1>("h0"), _T) @[Lookup.scala 31:38]
    node signals_0 = mux(_T_1, UInt<1>("h0"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node signals_1 = mux(_T_1, UInt<1>("h0"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node signals_2 = mux(_T_1, UInt<1>("h0"), UInt<2>("h3")) @[Lookup.scala 33:37]
    node signals_3 = mux(_T_1, UInt<1>("h0"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node signals_4 = mux(_T_1, UInt<1>("h0"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node signals_5 = mux(_T_1, UInt<1>("h0"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node signals_6 = mux(_T_1, UInt<1>("h0"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node signals_7 = mux(_T_1, UInt<1>("h0"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node signals_8 = mux(_T_1, UInt<1>("h0"), UInt<1>("h0")) @[Lookup.scala 33:37]
    io_branch <= signals_0 @[control.scala 48:13]
    io_memread <= signals_1 @[control.scala 49:14]
    io_toreg <= signals_2 @[control.scala 50:12]
    io_add <= signals_3 @[control.scala 51:10]
    io_memwrite <= signals_4 @[control.scala 52:15]
    io_regwrite <= signals_6 @[control.scala 54:15]
    io_immediate <= signals_5 @[control.scala 53:16]
    io_alusrc1 <= signals_7 @[control.scala 55:14]
    io_jump <= signals_8 @[control.scala 56:11]
