circuit Top :
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
  
    node _T = and(io_opcode, UInt<6>("h3f")) @[Lookup.scala 31:38]
    node _T_1 = eq(UInt<6>("h33"), _T) @[Lookup.scala 31:38]
    node _T_2 = and(io_opcode, UInt<5>("h1f")) @[Lookup.scala 31:38]
    node _T_3 = eq(UInt<5>("h13"), _T_2) @[Lookup.scala 31:38]
    node _T_4 = and(io_opcode, UInt<2>("h3")) @[Lookup.scala 31:38]
    node _T_5 = eq(UInt<2>("h3"), _T_4) @[Lookup.scala 31:38]
    node _T_6 = mux(_T_5, UInt<1>("h0"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node _T_7 = mux(_T_3, UInt<1>("h0"), _T_6) @[Lookup.scala 33:37]
    node signals_0 = mux(_T_1, UInt<1>("h0"), _T_7) @[Lookup.scala 33:37]
    node _T_8 = mux(_T_5, UInt<1>("h1"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node _T_9 = mux(_T_3, UInt<1>("h0"), _T_8) @[Lookup.scala 33:37]
    node signals_1 = mux(_T_1, UInt<1>("h0"), _T_9) @[Lookup.scala 33:37]
    node _T_10 = mux(_T_5, UInt<1>("h1"), UInt<2>("h3")) @[Lookup.scala 33:37]
    node _T_11 = mux(_T_3, UInt<1>("h0"), _T_10) @[Lookup.scala 33:37]
    node signals_2 = mux(_T_1, UInt<1>("h0"), _T_11) @[Lookup.scala 33:37]
    node _T_12 = mux(_T_5, UInt<1>("h1"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node _T_13 = mux(_T_3, UInt<1>("h0"), _T_12) @[Lookup.scala 33:37]
    node signals_3 = mux(_T_1, UInt<1>("h0"), _T_13) @[Lookup.scala 33:37]
    node _T_14 = mux(_T_5, UInt<1>("h0"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node _T_15 = mux(_T_3, UInt<1>("h0"), _T_14) @[Lookup.scala 33:37]
    node signals_4 = mux(_T_1, UInt<1>("h0"), _T_15) @[Lookup.scala 33:37]
    node _T_16 = mux(_T_5, UInt<1>("h1"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node _T_17 = mux(_T_3, UInt<1>("h1"), _T_16) @[Lookup.scala 33:37]
    node signals_5 = mux(_T_1, UInt<1>("h0"), _T_17) @[Lookup.scala 33:37]
    node _T_18 = mux(_T_5, UInt<1>("h1"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node _T_19 = mux(_T_3, UInt<1>("h1"), _T_18) @[Lookup.scala 33:37]
    node signals_6 = mux(_T_1, UInt<1>("h1"), _T_19) @[Lookup.scala 33:37]
    node _T_20 = mux(_T_5, UInt<1>("h0"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node _T_21 = mux(_T_3, UInt<1>("h0"), _T_20) @[Lookup.scala 33:37]
    node signals_7 = mux(_T_1, UInt<1>("h0"), _T_21) @[Lookup.scala 33:37]
    node _T_22 = mux(_T_5, UInt<1>("h0"), UInt<1>("h0")) @[Lookup.scala 33:37]
    node _T_23 = mux(_T_3, UInt<1>("h0"), _T_22) @[Lookup.scala 33:37]
    node signals_8 = mux(_T_1, UInt<1>("h0"), _T_23) @[Lookup.scala 33:37]
    io_branch <= signals_0 @[control.scala 54:13]
    io_memread <= signals_1 @[control.scala 55:14]
    io_toreg <= signals_2 @[control.scala 56:12]
    io_add <= signals_3 @[control.scala 57:10]
    io_memwrite <= signals_4 @[control.scala 58:15]
    io_regwrite <= signals_6 @[control.scala 60:15]
    io_immediate <= signals_5 @[control.scala 59:16]
    io_alusrc1 <= signals_7 @[control.scala 61:14]
    io_jump <= signals_8 @[control.scala 62:11]

  module RegisterFile :
    input clock : Clock
    input reset : UInt<1>
    input io_readreg1 : UInt<5>
    input io_readreg2 : UInt<5>
    input io_writereg : UInt<5>
    input io_writedata : UInt<32>
    input io_wen : UInt<1>
    output io_readdata1 : UInt<32>
    output io_readdata2 : UInt<32>
  
    reg regs_0 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_0) @[register-file.scala 28:17]
    reg regs_1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_1) @[register-file.scala 28:17]
    reg regs_2 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_2) @[register-file.scala 28:17]
    reg regs_3 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_3) @[register-file.scala 28:17]
    reg regs_4 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_4) @[register-file.scala 28:17]
    reg regs_5 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_5) @[register-file.scala 28:17]
    reg regs_6 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_6) @[register-file.scala 28:17]
    reg regs_7 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_7) @[register-file.scala 28:17]
    reg regs_8 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_8) @[register-file.scala 28:17]
    reg regs_9 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_9) @[register-file.scala 28:17]
    reg regs_10 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_10) @[register-file.scala 28:17]
    reg regs_11 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_11) @[register-file.scala 28:17]
    reg regs_12 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_12) @[register-file.scala 28:17]
    reg regs_13 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_13) @[register-file.scala 28:17]
    reg regs_14 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_14) @[register-file.scala 28:17]
    reg regs_15 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_15) @[register-file.scala 28:17]
    reg regs_16 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_16) @[register-file.scala 28:17]
    reg regs_17 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_17) @[register-file.scala 28:17]
    reg regs_18 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_18) @[register-file.scala 28:17]
    reg regs_19 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_19) @[register-file.scala 28:17]
    reg regs_20 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_20) @[register-file.scala 28:17]
    reg regs_21 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_21) @[register-file.scala 28:17]
    reg regs_22 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_22) @[register-file.scala 28:17]
    reg regs_23 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_23) @[register-file.scala 28:17]
    reg regs_24 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_24) @[register-file.scala 28:17]
    reg regs_25 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_25) @[register-file.scala 28:17]
    reg regs_26 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_26) @[register-file.scala 28:17]
    reg regs_27 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_27) @[register-file.scala 28:17]
    reg regs_28 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_28) @[register-file.scala 28:17]
    reg regs_29 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_29) @[register-file.scala 28:17]
    reg regs_30 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_30) @[register-file.scala 28:17]
    reg regs_31 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), regs_31) @[register-file.scala 28:17]
    node _regs_io_writereg = io_writedata @[register-file.scala 32:23 register-file.scala 32:23]
    node _GEN_0 = mux(eq(UInt<1>("h0"), io_writereg), _regs_io_writereg, regs_0) @[register-file.scala 32:23]
    node _GEN_1 = mux(eq(UInt<1>("h1"), io_writereg), _regs_io_writereg, regs_1) @[register-file.scala 32:23]
    node _GEN_2 = mux(eq(UInt<2>("h2"), io_writereg), _regs_io_writereg, regs_2) @[register-file.scala 32:23]
    node _GEN_3 = mux(eq(UInt<2>("h3"), io_writereg), _regs_io_writereg, regs_3) @[register-file.scala 32:23]
    node _GEN_4 = mux(eq(UInt<3>("h4"), io_writereg), _regs_io_writereg, regs_4) @[register-file.scala 32:23]
    node _GEN_5 = mux(eq(UInt<3>("h5"), io_writereg), _regs_io_writereg, regs_5) @[register-file.scala 32:23]
    node _GEN_6 = mux(eq(UInt<3>("h6"), io_writereg), _regs_io_writereg, regs_6) @[register-file.scala 32:23]
    node _GEN_7 = mux(eq(UInt<3>("h7"), io_writereg), _regs_io_writereg, regs_7) @[register-file.scala 32:23]
    node _GEN_8 = mux(eq(UInt<4>("h8"), io_writereg), _regs_io_writereg, regs_8) @[register-file.scala 32:23]
    node _GEN_9 = mux(eq(UInt<4>("h9"), io_writereg), _regs_io_writereg, regs_9) @[register-file.scala 32:23]
    node _GEN_10 = mux(eq(UInt<4>("ha"), io_writereg), _regs_io_writereg, regs_10) @[register-file.scala 32:23]
    node _GEN_11 = mux(eq(UInt<4>("hb"), io_writereg), _regs_io_writereg, regs_11) @[register-file.scala 32:23]
    node _GEN_12 = mux(eq(UInt<4>("hc"), io_writereg), _regs_io_writereg, regs_12) @[register-file.scala 32:23]
    node _GEN_13 = mux(eq(UInt<4>("hd"), io_writereg), _regs_io_writereg, regs_13) @[register-file.scala 32:23]
    node _GEN_14 = mux(eq(UInt<4>("he"), io_writereg), _regs_io_writereg, regs_14) @[register-file.scala 32:23]
    node _GEN_15 = mux(eq(UInt<4>("hf"), io_writereg), _regs_io_writereg, regs_15) @[register-file.scala 32:23]
    node _GEN_16 = mux(eq(UInt<5>("h10"), io_writereg), _regs_io_writereg, regs_16) @[register-file.scala 32:23]
    node _GEN_17 = mux(eq(UInt<5>("h11"), io_writereg), _regs_io_writereg, regs_17) @[register-file.scala 32:23]
    node _GEN_18 = mux(eq(UInt<5>("h12"), io_writereg), _regs_io_writereg, regs_18) @[register-file.scala 32:23]
    node _GEN_19 = mux(eq(UInt<5>("h13"), io_writereg), _regs_io_writereg, regs_19) @[register-file.scala 32:23]
    node _GEN_20 = mux(eq(UInt<5>("h14"), io_writereg), _regs_io_writereg, regs_20) @[register-file.scala 32:23]
    node _GEN_21 = mux(eq(UInt<5>("h15"), io_writereg), _regs_io_writereg, regs_21) @[register-file.scala 32:23]
    node _GEN_22 = mux(eq(UInt<5>("h16"), io_writereg), _regs_io_writereg, regs_22) @[register-file.scala 32:23]
    node _GEN_23 = mux(eq(UInt<5>("h17"), io_writereg), _regs_io_writereg, regs_23) @[register-file.scala 32:23]
    node _GEN_24 = mux(eq(UInt<5>("h18"), io_writereg), _regs_io_writereg, regs_24) @[register-file.scala 32:23]
    node _GEN_25 = mux(eq(UInt<5>("h19"), io_writereg), _regs_io_writereg, regs_25) @[register-file.scala 32:23]
    node _GEN_26 = mux(eq(UInt<5>("h1a"), io_writereg), _regs_io_writereg, regs_26) @[register-file.scala 32:23]
    node _GEN_27 = mux(eq(UInt<5>("h1b"), io_writereg), _regs_io_writereg, regs_27) @[register-file.scala 32:23]
    node _GEN_28 = mux(eq(UInt<5>("h1c"), io_writereg), _regs_io_writereg, regs_28) @[register-file.scala 32:23]
    node _GEN_29 = mux(eq(UInt<5>("h1d"), io_writereg), _regs_io_writereg, regs_29) @[register-file.scala 32:23]
    node _GEN_30 = mux(eq(UInt<5>("h1e"), io_writereg), _regs_io_writereg, regs_30) @[register-file.scala 32:23]
    node _GEN_31 = mux(eq(UInt<5>("h1f"), io_writereg), _regs_io_writereg, regs_31) @[register-file.scala 32:23]
    node _GEN_32 = mux(io_wen, _GEN_0, regs_0) @[register-file.scala 31:17]
    node _GEN_33 = mux(io_wen, _GEN_1, regs_1) @[register-file.scala 31:17]
    node _GEN_34 = mux(io_wen, _GEN_2, regs_2) @[register-file.scala 31:17]
    node _GEN_35 = mux(io_wen, _GEN_3, regs_3) @[register-file.scala 31:17]
    node _GEN_36 = mux(io_wen, _GEN_4, regs_4) @[register-file.scala 31:17]
    node _GEN_37 = mux(io_wen, _GEN_5, regs_5) @[register-file.scala 31:17]
    node _GEN_38 = mux(io_wen, _GEN_6, regs_6) @[register-file.scala 31:17]
    node _GEN_39 = mux(io_wen, _GEN_7, regs_7) @[register-file.scala 31:17]
    node _GEN_40 = mux(io_wen, _GEN_8, regs_8) @[register-file.scala 31:17]
    node _GEN_41 = mux(io_wen, _GEN_9, regs_9) @[register-file.scala 31:17]
    node _GEN_42 = mux(io_wen, _GEN_10, regs_10) @[register-file.scala 31:17]
    node _GEN_43 = mux(io_wen, _GEN_11, regs_11) @[register-file.scala 31:17]
    node _GEN_44 = mux(io_wen, _GEN_12, regs_12) @[register-file.scala 31:17]
    node _GEN_45 = mux(io_wen, _GEN_13, regs_13) @[register-file.scala 31:17]
    node _GEN_46 = mux(io_wen, _GEN_14, regs_14) @[register-file.scala 31:17]
    node _GEN_47 = mux(io_wen, _GEN_15, regs_15) @[register-file.scala 31:17]
    node _GEN_48 = mux(io_wen, _GEN_16, regs_16) @[register-file.scala 31:17]
    node _GEN_49 = mux(io_wen, _GEN_17, regs_17) @[register-file.scala 31:17]
    node _GEN_50 = mux(io_wen, _GEN_18, regs_18) @[register-file.scala 31:17]
    node _GEN_51 = mux(io_wen, _GEN_19, regs_19) @[register-file.scala 31:17]
    node _GEN_52 = mux(io_wen, _GEN_20, regs_20) @[register-file.scala 31:17]
    node _GEN_53 = mux(io_wen, _GEN_21, regs_21) @[register-file.scala 31:17]
    node _GEN_54 = mux(io_wen, _GEN_22, regs_22) @[register-file.scala 31:17]
    node _GEN_55 = mux(io_wen, _GEN_23, regs_23) @[register-file.scala 31:17]
    node _GEN_56 = mux(io_wen, _GEN_24, regs_24) @[register-file.scala 31:17]
    node _GEN_57 = mux(io_wen, _GEN_25, regs_25) @[register-file.scala 31:17]
    node _GEN_58 = mux(io_wen, _GEN_26, regs_26) @[register-file.scala 31:17]
    node _GEN_59 = mux(io_wen, _GEN_27, regs_27) @[register-file.scala 31:17]
    node _GEN_60 = mux(io_wen, _GEN_28, regs_28) @[register-file.scala 31:17]
    node _GEN_61 = mux(io_wen, _GEN_29, regs_29) @[register-file.scala 31:17]
    node _GEN_62 = mux(io_wen, _GEN_30, regs_30) @[register-file.scala 31:17]
    node _GEN_63 = mux(io_wen, _GEN_31, regs_31) @[register-file.scala 31:17]
    node _GEN_64 = validif(eq(UInt<1>("h0"), io_readreg1), regs_0) @[register-file.scala 37:16]
    node _GEN_65 = mux(eq(UInt<1>("h1"), io_readreg1), regs_1, _GEN_64) @[register-file.scala 37:16]
    node _GEN_66 = mux(eq(UInt<2>("h2"), io_readreg1), regs_2, _GEN_65) @[register-file.scala 37:16]
    node _GEN_67 = mux(eq(UInt<2>("h3"), io_readreg1), regs_3, _GEN_66) @[register-file.scala 37:16]
    node _GEN_68 = mux(eq(UInt<3>("h4"), io_readreg1), regs_4, _GEN_67) @[register-file.scala 37:16]
    node _GEN_69 = mux(eq(UInt<3>("h5"), io_readreg1), regs_5, _GEN_68) @[register-file.scala 37:16]
    node _GEN_70 = mux(eq(UInt<3>("h6"), io_readreg1), regs_6, _GEN_69) @[register-file.scala 37:16]
    node _GEN_71 = mux(eq(UInt<3>("h7"), io_readreg1), regs_7, _GEN_70) @[register-file.scala 37:16]
    node _GEN_72 = mux(eq(UInt<4>("h8"), io_readreg1), regs_8, _GEN_71) @[register-file.scala 37:16]
    node _GEN_73 = mux(eq(UInt<4>("h9"), io_readreg1), regs_9, _GEN_72) @[register-file.scala 37:16]
    node _GEN_74 = mux(eq(UInt<4>("ha"), io_readreg1), regs_10, _GEN_73) @[register-file.scala 37:16]
    node _GEN_75 = mux(eq(UInt<4>("hb"), io_readreg1), regs_11, _GEN_74) @[register-file.scala 37:16]
    node _GEN_76 = mux(eq(UInt<4>("hc"), io_readreg1), regs_12, _GEN_75) @[register-file.scala 37:16]
    node _GEN_77 = mux(eq(UInt<4>("hd"), io_readreg1), regs_13, _GEN_76) @[register-file.scala 37:16]
    node _GEN_78 = mux(eq(UInt<4>("he"), io_readreg1), regs_14, _GEN_77) @[register-file.scala 37:16]
    node _GEN_79 = mux(eq(UInt<4>("hf"), io_readreg1), regs_15, _GEN_78) @[register-file.scala 37:16]
    node _GEN_80 = mux(eq(UInt<5>("h10"), io_readreg1), regs_16, _GEN_79) @[register-file.scala 37:16]
    node _GEN_81 = mux(eq(UInt<5>("h11"), io_readreg1), regs_17, _GEN_80) @[register-file.scala 37:16]
    node _GEN_82 = mux(eq(UInt<5>("h12"), io_readreg1), regs_18, _GEN_81) @[register-file.scala 37:16]
    node _GEN_83 = mux(eq(UInt<5>("h13"), io_readreg1), regs_19, _GEN_82) @[register-file.scala 37:16]
    node _GEN_84 = mux(eq(UInt<5>("h14"), io_readreg1), regs_20, _GEN_83) @[register-file.scala 37:16]
    node _GEN_85 = mux(eq(UInt<5>("h15"), io_readreg1), regs_21, _GEN_84) @[register-file.scala 37:16]
    node _GEN_86 = mux(eq(UInt<5>("h16"), io_readreg1), regs_22, _GEN_85) @[register-file.scala 37:16]
    node _GEN_87 = mux(eq(UInt<5>("h17"), io_readreg1), regs_23, _GEN_86) @[register-file.scala 37:16]
    node _GEN_88 = mux(eq(UInt<5>("h18"), io_readreg1), regs_24, _GEN_87) @[register-file.scala 37:16]
    node _GEN_89 = mux(eq(UInt<5>("h19"), io_readreg1), regs_25, _GEN_88) @[register-file.scala 37:16]
    node _GEN_90 = mux(eq(UInt<5>("h1a"), io_readreg1), regs_26, _GEN_89) @[register-file.scala 37:16]
    node _GEN_91 = mux(eq(UInt<5>("h1b"), io_readreg1), regs_27, _GEN_90) @[register-file.scala 37:16]
    node _GEN_92 = mux(eq(UInt<5>("h1c"), io_readreg1), regs_28, _GEN_91) @[register-file.scala 37:16]
    node _GEN_93 = mux(eq(UInt<5>("h1d"), io_readreg1), regs_29, _GEN_92) @[register-file.scala 37:16]
    node _GEN_94 = mux(eq(UInt<5>("h1e"), io_readreg1), regs_30, _GEN_93) @[register-file.scala 37:16]
    node _GEN_95 = mux(eq(UInt<5>("h1f"), io_readreg1), regs_31, _GEN_94) @[register-file.scala 37:16]
    node _GEN_96 = validif(eq(UInt<1>("h0"), io_readreg2), regs_0) @[register-file.scala 38:16]
    node _GEN_97 = mux(eq(UInt<1>("h1"), io_readreg2), regs_1, _GEN_96) @[register-file.scala 38:16]
    node _GEN_98 = mux(eq(UInt<2>("h2"), io_readreg2), regs_2, _GEN_97) @[register-file.scala 38:16]
    node _GEN_99 = mux(eq(UInt<2>("h3"), io_readreg2), regs_3, _GEN_98) @[register-file.scala 38:16]
    node _GEN_100 = mux(eq(UInt<3>("h4"), io_readreg2), regs_4, _GEN_99) @[register-file.scala 38:16]
    node _GEN_101 = mux(eq(UInt<3>("h5"), io_readreg2), regs_5, _GEN_100) @[register-file.scala 38:16]
    node _GEN_102 = mux(eq(UInt<3>("h6"), io_readreg2), regs_6, _GEN_101) @[register-file.scala 38:16]
    node _GEN_103 = mux(eq(UInt<3>("h7"), io_readreg2), regs_7, _GEN_102) @[register-file.scala 38:16]
    node _GEN_104 = mux(eq(UInt<4>("h8"), io_readreg2), regs_8, _GEN_103) @[register-file.scala 38:16]
    node _GEN_105 = mux(eq(UInt<4>("h9"), io_readreg2), regs_9, _GEN_104) @[register-file.scala 38:16]
    node _GEN_106 = mux(eq(UInt<4>("ha"), io_readreg2), regs_10, _GEN_105) @[register-file.scala 38:16]
    node _GEN_107 = mux(eq(UInt<4>("hb"), io_readreg2), regs_11, _GEN_106) @[register-file.scala 38:16]
    node _GEN_108 = mux(eq(UInt<4>("hc"), io_readreg2), regs_12, _GEN_107) @[register-file.scala 38:16]
    node _GEN_109 = mux(eq(UInt<4>("hd"), io_readreg2), regs_13, _GEN_108) @[register-file.scala 38:16]
    node _GEN_110 = mux(eq(UInt<4>("he"), io_readreg2), regs_14, _GEN_109) @[register-file.scala 38:16]
    node _GEN_111 = mux(eq(UInt<4>("hf"), io_readreg2), regs_15, _GEN_110) @[register-file.scala 38:16]
    node _GEN_112 = mux(eq(UInt<5>("h10"), io_readreg2), regs_16, _GEN_111) @[register-file.scala 38:16]
    node _GEN_113 = mux(eq(UInt<5>("h11"), io_readreg2), regs_17, _GEN_112) @[register-file.scala 38:16]
    node _GEN_114 = mux(eq(UInt<5>("h12"), io_readreg2), regs_18, _GEN_113) @[register-file.scala 38:16]
    node _GEN_115 = mux(eq(UInt<5>("h13"), io_readreg2), regs_19, _GEN_114) @[register-file.scala 38:16]
    node _GEN_116 = mux(eq(UInt<5>("h14"), io_readreg2), regs_20, _GEN_115) @[register-file.scala 38:16]
    node _GEN_117 = mux(eq(UInt<5>("h15"), io_readreg2), regs_21, _GEN_116) @[register-file.scala 38:16]
    node _GEN_118 = mux(eq(UInt<5>("h16"), io_readreg2), regs_22, _GEN_117) @[register-file.scala 38:16]
    node _GEN_119 = mux(eq(UInt<5>("h17"), io_readreg2), regs_23, _GEN_118) @[register-file.scala 38:16]
    node _GEN_120 = mux(eq(UInt<5>("h18"), io_readreg2), regs_24, _GEN_119) @[register-file.scala 38:16]
    node _GEN_121 = mux(eq(UInt<5>("h19"), io_readreg2), regs_25, _GEN_120) @[register-file.scala 38:16]
    node _GEN_122 = mux(eq(UInt<5>("h1a"), io_readreg2), regs_26, _GEN_121) @[register-file.scala 38:16]
    node _GEN_123 = mux(eq(UInt<5>("h1b"), io_readreg2), regs_27, _GEN_122) @[register-file.scala 38:16]
    node _GEN_124 = mux(eq(UInt<5>("h1c"), io_readreg2), regs_28, _GEN_123) @[register-file.scala 38:16]
    node _GEN_125 = mux(eq(UInt<5>("h1d"), io_readreg2), regs_29, _GEN_124) @[register-file.scala 38:16]
    node _GEN_126 = mux(eq(UInt<5>("h1e"), io_readreg2), regs_30, _GEN_125) @[register-file.scala 38:16]
    node _GEN_127 = mux(eq(UInt<5>("h1f"), io_readreg2), regs_31, _GEN_126) @[register-file.scala 38:16]
    node _regs_io_readreg1 = _GEN_95 @[register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16 register-file.scala 37:16]
    node _regs_io_readreg2 = _GEN_127 @[register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16 register-file.scala 38:16]
    io_readdata1 <= _regs_io_readreg1 @[register-file.scala 37:16]
    io_readdata2 <= _regs_io_readreg2 @[register-file.scala 38:16]
    regs_0 <= _GEN_32 @[register-file.scala 32:23]
    regs_1 <= _GEN_33 @[register-file.scala 32:23]
    regs_2 <= _GEN_34 @[register-file.scala 32:23]
    regs_3 <= _GEN_35 @[register-file.scala 32:23]
    regs_4 <= _GEN_36 @[register-file.scala 32:23]
    regs_5 <= _GEN_37 @[register-file.scala 32:23]
    regs_6 <= _GEN_38 @[register-file.scala 32:23]
    regs_7 <= _GEN_39 @[register-file.scala 32:23]
    regs_8 <= _GEN_40 @[register-file.scala 32:23]
    regs_9 <= _GEN_41 @[register-file.scala 32:23]
    regs_10 <= _GEN_42 @[register-file.scala 32:23]
    regs_11 <= _GEN_43 @[register-file.scala 32:23]
    regs_12 <= _GEN_44 @[register-file.scala 32:23]
    regs_13 <= _GEN_45 @[register-file.scala 32:23]
    regs_14 <= _GEN_46 @[register-file.scala 32:23]
    regs_15 <= _GEN_47 @[register-file.scala 32:23]
    regs_16 <= _GEN_48 @[register-file.scala 32:23]
    regs_17 <= _GEN_49 @[register-file.scala 32:23]
    regs_18 <= _GEN_50 @[register-file.scala 32:23]
    regs_19 <= _GEN_51 @[register-file.scala 32:23]
    regs_20 <= _GEN_52 @[register-file.scala 32:23]
    regs_21 <= _GEN_53 @[register-file.scala 32:23]
    regs_22 <= _GEN_54 @[register-file.scala 32:23]
    regs_23 <= _GEN_55 @[register-file.scala 32:23]
    regs_24 <= _GEN_56 @[register-file.scala 32:23]
    regs_25 <= _GEN_57 @[register-file.scala 32:23]
    regs_26 <= _GEN_58 @[register-file.scala 32:23]
    regs_27 <= _GEN_59 @[register-file.scala 32:23]
    regs_28 <= _GEN_60 @[register-file.scala 32:23]
    regs_29 <= _GEN_61 @[register-file.scala 32:23]
    regs_30 <= _GEN_62 @[register-file.scala 32:23]
    regs_31 <= _GEN_63 @[register-file.scala 32:23]

  module ALUControl :
    input clock : Clock
    input reset : UInt<1>
    input io_add : UInt<1>
    input io_immediate : UInt<1>
    input io_funct7 : UInt<7>
    input io_funct3 : UInt<3>
    output io_operation : UInt<4>
  
    node _T = eq(UInt<1>("h0"), io_funct3) @[Conditional.scala 37:30]
    node _T_1 = eq(io_funct7, UInt<1>("h0")) @[alucontrol.scala 37:41]
    node _T_2 = or(io_immediate, _T_1) @[alucontrol.scala 37:28]
    node _GEN_0 = mux(_T_2, UInt<2>("h2"), UInt<2>("h3")) @[alucontrol.scala 37:59]
    node _T_3 = eq(UInt<1>("h1"), io_funct3) @[Conditional.scala 37:30]
    node _T_4 = eq(UInt<2>("h2"), io_funct3) @[Conditional.scala 37:30]
    node _T_5 = eq(UInt<2>("h3"), io_funct3) @[Conditional.scala 37:30]
    node _T_6 = eq(UInt<3>("h4"), io_funct3) @[Conditional.scala 37:30]
    node _T_7 = eq(UInt<3>("h5"), io_funct3) @[Conditional.scala 37:30]
    node _T_8 = eq(io_funct7, UInt<1>("h0")) @[alucontrol.scala 48:25]
    node _GEN_1 = mux(_T_8, UInt<3>("h7"), UInt<4>("h8")) @[alucontrol.scala 48:43]
    node _T_9 = eq(UInt<3>("h6"), io_funct3) @[Conditional.scala 37:30]
    node _T_10 = eq(UInt<3>("h7"), io_funct3) @[Conditional.scala 37:30]
    node _GEN_2 = mux(_T_10, UInt<1>("h0"), UInt<4>("hf")) @[Conditional.scala 39:67]
    node _GEN_3 = mux(_T_9, UInt<1>("h1"), _GEN_2) @[Conditional.scala 39:67]
    node _GEN_4 = mux(_T_7, _GEN_1, _GEN_3) @[Conditional.scala 39:67]
    node _GEN_5 = mux(_T_6, UInt<4>("h9"), _GEN_4) @[Conditional.scala 39:67]
    node _GEN_6 = mux(_T_5, UInt<3>("h5"), _GEN_5) @[Conditional.scala 39:67]
    node _GEN_7 = mux(_T_4, UInt<3>("h4"), _GEN_6) @[Conditional.scala 39:67]
    node _GEN_8 = mux(_T_3, UInt<3>("h6"), _GEN_7) @[Conditional.scala 39:67]
    node _GEN_9 = mux(_T, _GEN_0, _GEN_8) @[Conditional.scala 40:58]
    node _GEN_10 = mux(io_add, UInt<2>("h2"), _GEN_9) @[alucontrol.scala 32:17]
    io_operation <= _GEN_10 @[alucontrol.scala 30:16 alucontrol.scala 33:18 alucontrol.scala 38:24 alucontrol.scala 40:24 alucontrol.scala 43:36 alucontrol.scala 44:36 alucontrol.scala 45:36 alucontrol.scala 46:36 alucontrol.scala 49:24 alucontrol.scala 51:24 alucontrol.scala 54:36 alucontrol.scala 55:36]

  module ALU :
    input clock : Clock
    input reset : UInt<1>
    input io_operation : UInt<4>
    input io_inputx : UInt<32>
    input io_inputy : UInt<32>
    output io_result : UInt<32>
  
    node _T = eq(UInt<1>("h0"), io_operation) @[Conditional.scala 37:30]
    node _T_1 = and(io_inputx, io_inputy) @[alu.scala 30:30]
    node _T_2 = eq(UInt<1>("h1"), io_operation) @[Conditional.scala 37:30]
    node _T_3 = or(io_inputx, io_inputy) @[alu.scala 33:30]
    node _T_4 = eq(UInt<2>("h2"), io_operation) @[Conditional.scala 37:30]
    node _T_5 = add(io_inputx, io_inputy) @[alu.scala 36:30]
    node _T_6 = tail(_T_5, 1) @[alu.scala 36:30]
    node _T_7 = eq(UInt<2>("h3"), io_operation) @[Conditional.scala 37:30]
    node _T_8 = sub(io_inputx, io_inputy) @[alu.scala 39:30]
    node _T_9 = tail(_T_8, 1) @[alu.scala 39:30]
    node _T_10 = eq(UInt<3>("h4"), io_operation) @[Conditional.scala 37:30]
    node _T_11 = asSInt(io_inputx) @[alu.scala 42:31]
    node _T_12 = asSInt(io_inputy) @[alu.scala 42:50]
    node _T_13 = lt(_T_11, _T_12) @[alu.scala 42:38]
    node _T_14 = eq(UInt<3>("h5"), io_operation) @[Conditional.scala 37:30]
    node _T_15 = lt(io_inputx, io_inputy) @[alu.scala 45:31]
    node _T_16 = eq(UInt<3>("h6"), io_operation) @[Conditional.scala 37:30]
    node _T_17 = bits(io_inputy, 4, 0) @[alu.scala 48:42]
    node _T_18 = dshl(io_inputx, _T_17) @[alu.scala 48:30]
    node _T_19 = eq(UInt<3>("h7"), io_operation) @[Conditional.scala 37:30]
    node _T_20 = bits(io_inputy, 4, 0) @[alu.scala 51:42]
    node _T_21 = dshr(io_inputx, _T_20) @[alu.scala 51:30]
    node _T_22 = eq(UInt<4>("h8"), io_operation) @[Conditional.scala 37:30]
    node _T_23 = asSInt(io_inputx) @[alu.scala 54:31]
    node _T_24 = bits(io_inputy, 4, 0) @[alu.scala 54:50]
    node _T_25 = dshr(_T_23, _T_24) @[alu.scala 54:38]
    node _T_26 = asUInt(_T_25) @[alu.scala 54:57]
    node _T_27 = eq(UInt<4>("h9"), io_operation) @[Conditional.scala 37:30]
    node _T_28 = xor(io_inputx, io_inputy) @[alu.scala 57:30]
    node _T_29 = eq(UInt<4>("ha"), io_operation) @[Conditional.scala 37:30]
    node _T_30 = or(io_inputx, io_inputy) @[alu.scala 60:32]
    node _T_31 = not(_T_30) @[alu.scala 60:20]
    node _GEN_0 = mux(_T_29, _T_31, UInt<1>("h0")) @[Conditional.scala 39:67]
    node _GEN_1 = mux(_T_27, _T_28, _GEN_0) @[Conditional.scala 39:67]
    node _GEN_2 = mux(_T_22, _T_26, _GEN_1) @[Conditional.scala 39:67]
    node _GEN_3 = mux(_T_19, _T_21, _GEN_2) @[Conditional.scala 39:67]
    node _GEN_4 = mux(_T_16, _T_18, _GEN_3) @[Conditional.scala 39:67]
    node _GEN_5 = mux(_T_14, _T_15, _GEN_4) @[Conditional.scala 39:67]
    node _GEN_6 = mux(_T_10, _T_13, _GEN_5) @[Conditional.scala 39:67]
    node _GEN_7 = mux(_T_7, _T_9, _GEN_6) @[Conditional.scala 39:67]
    node _GEN_8 = mux(_T_4, _T_6, _GEN_7) @[Conditional.scala 39:67]
    node _GEN_9 = mux(_T_2, _T_3, _GEN_8) @[Conditional.scala 39:67]
    node _GEN_10 = mux(_T, _T_1, _GEN_9) @[Conditional.scala 40:58]
    io_result <= bits(_GEN_10, 31, 0) @[alu.scala 26:13 alu.scala 30:17 alu.scala 33:17 alu.scala 36:17 alu.scala 39:17 alu.scala 42:17 alu.scala 45:17 alu.scala 48:17 alu.scala 51:17 alu.scala 54:17 alu.scala 57:17 alu.scala 60:17]

  module ImmediateGenerator :
    input clock : Clock
    input reset : UInt<1>
    input io_instruction : UInt<32>
    output io_sextImm : UInt<32>
  
    node opcode = bits(io_instruction, 6, 0) @[helpers.scala 44:30]
    node _T = eq(UInt<6>("h37"), opcode) @[Conditional.scala 37:30]
    node _T_1 = bits(io_instruction, 31, 12) @[helpers.scala 47:31]
    node _T_2 = mux(UInt<1>("h0"), UInt<12>("hfff"), UInt<12>("h0")) @[Bitwise.scala 72:12]
    node _T_3 = cat(_T_1, _T_2) @[Cat.scala 30:58]
    node _T_4 = eq(UInt<5>("h17"), opcode) @[Conditional.scala 37:30]
    node _T_5 = bits(io_instruction, 31, 12) @[helpers.scala 51:31]
    node _T_6 = mux(UInt<1>("h0"), UInt<12>("hfff"), UInt<12>("h0")) @[Bitwise.scala 72:12]
    node _T_7 = cat(_T_5, _T_6) @[Cat.scala 30:58]
    node _T_8 = eq(UInt<7>("h6f"), opcode) @[Conditional.scala 37:30]
    node _T_9 = bits(io_instruction, 31, 31) @[helpers.scala 55:35]
    node _T_10 = bits(io_instruction, 19, 12) @[helpers.scala 55:55]
    node _T_11 = bits(io_instruction, 20, 20) @[helpers.scala 56:35]
    node _T_12 = bits(io_instruction, 30, 21) @[helpers.scala 56:55]
    node _T_13 = cat(_T_11, _T_12) @[Cat.scala 30:58]
    node _T_14 = cat(_T_9, _T_10) @[Cat.scala 30:58]
    node _T_15 = cat(_T_14, _T_13) @[Cat.scala 30:58]
    node _T_16 = bits(_T_15, 19, 19) @[helpers.scala 57:36]
    node _T_17 = bits(_T_16, 0, 0) @[Bitwise.scala 72:15]
    node _T_18 = mux(_T_17, UInt<11>("h7ff"), UInt<11>("h0")) @[Bitwise.scala 72:12]
    node _T_19 = cat(_T_18, _T_15) @[Cat.scala 30:58]
    node _T_20 = cat(_T_19, UInt<1>("h0")) @[Cat.scala 30:58]
    node _T_21 = eq(UInt<7>("h67"), opcode) @[Conditional.scala 37:30]
    node _T_22 = bits(io_instruction, 31, 20) @[helpers.scala 60:31]
    node _T_23 = bits(_T_22, 11, 11) @[helpers.scala 61:36]
    node _T_24 = bits(_T_23, 0, 0) @[Bitwise.scala 72:15]
    node _T_25 = mux(_T_24, UInt<20>("hfffff"), UInt<20>("h0")) @[Bitwise.scala 72:12]
    node _T_26 = cat(_T_25, _T_22) @[Cat.scala 30:58]
    node _T_27 = eq(UInt<7>("h63"), opcode) @[Conditional.scala 37:30]
    node _T_28 = bits(io_instruction, 31, 31) @[helpers.scala 64:35]
    node _T_29 = bits(io_instruction, 7, 7) @[helpers.scala 64:55]
    node _T_30 = bits(io_instruction, 30, 25) @[helpers.scala 65:35]
    node _T_31 = bits(io_instruction, 11, 8) @[helpers.scala 65:58]
    node _T_32 = cat(_T_30, _T_31) @[Cat.scala 30:58]
    node _T_33 = cat(_T_28, _T_29) @[Cat.scala 30:58]
    node _T_34 = cat(_T_33, _T_32) @[Cat.scala 30:58]
    node _T_35 = bits(_T_34, 11, 11) @[helpers.scala 66:37]
    node _T_36 = bits(_T_35, 0, 0) @[Bitwise.scala 72:15]
    node _T_37 = mux(_T_36, UInt<19>("h7ffff"), UInt<19>("h0")) @[Bitwise.scala 72:12]
    node _T_38 = cat(_T_37, _T_34) @[Cat.scala 30:58]
    node _T_39 = cat(_T_38, UInt<1>("h0")) @[Cat.scala 30:58]
    node _T_40 = eq(UInt<2>("h3"), opcode) @[Conditional.scala 37:30]
    node _T_41 = bits(io_instruction, 31, 20) @[helpers.scala 69:31]
    node _T_42 = bits(_T_41, 11, 11) @[helpers.scala 70:36]
    node _T_43 = bits(_T_42, 0, 0) @[Bitwise.scala 72:15]
    node _T_44 = mux(_T_43, UInt<20>("hfffff"), UInt<20>("h0")) @[Bitwise.scala 72:12]
    node _T_45 = cat(_T_44, _T_41) @[Cat.scala 30:58]
    node _T_46 = eq(UInt<6>("h23"), opcode) @[Conditional.scala 37:30]
    node _T_47 = bits(io_instruction, 31, 25) @[helpers.scala 73:35]
    node _T_48 = bits(io_instruction, 11, 7) @[helpers.scala 73:59]
    node _T_49 = cat(_T_47, _T_48) @[Cat.scala 30:58]
    node _T_50 = bits(_T_49, 11, 11) @[helpers.scala 74:36]
    node _T_51 = bits(_T_50, 0, 0) @[Bitwise.scala 72:15]
    node _T_52 = mux(_T_51, UInt<20>("hfffff"), UInt<20>("h0")) @[Bitwise.scala 72:12]
    node _T_53 = cat(_T_52, _T_49) @[Cat.scala 30:58]
    node _T_54 = eq(UInt<5>("h13"), opcode) @[Conditional.scala 37:30]
    node _T_55 = bits(io_instruction, 31, 20) @[helpers.scala 77:31]
    node _T_56 = bits(_T_55, 11, 11) @[helpers.scala 78:36]
    node _T_57 = bits(_T_56, 0, 0) @[Bitwise.scala 72:15]
    node _T_58 = mux(_T_57, UInt<20>("hfffff"), UInt<20>("h0")) @[Bitwise.scala 72:12]
    node _T_59 = cat(_T_58, _T_55) @[Cat.scala 30:58]
    node _T_60 = eq(UInt<7>("h73"), opcode) @[Conditional.scala 37:30]
    node _T_61 = mux(UInt<1>("h0"), UInt<27>("h7ffffff"), UInt<27>("h0")) @[Bitwise.scala 72:12]
    node _T_62 = bits(io_instruction, 19, 15) @[helpers.scala 81:53]
    node _T_63 = cat(_T_61, _T_62) @[Cat.scala 30:58]
    node _GEN_0 = mux(_T_60, _T_63, UInt<1>("h0")) @[Conditional.scala 39:67]
    node _GEN_1 = mux(_T_54, _T_59, _GEN_0) @[Conditional.scala 39:67]
    node _GEN_2 = mux(_T_46, _T_53, _GEN_1) @[Conditional.scala 39:67]
    node _GEN_3 = mux(_T_40, _T_45, _GEN_2) @[Conditional.scala 39:67]
    node _GEN_4 = mux(_T_27, _T_39, _GEN_3) @[Conditional.scala 39:67]
    node _GEN_5 = mux(_T_21, _T_26, _GEN_4) @[Conditional.scala 39:67]
    node _GEN_6 = mux(_T_8, _T_20, _GEN_5) @[Conditional.scala 39:67]
    node _GEN_7 = mux(_T_4, _T_7, _GEN_6) @[Conditional.scala 39:67]
    node _GEN_8 = mux(_T, _T_3, _GEN_7) @[Conditional.scala 40:58]
    io_sextImm <= _GEN_8 @[helpers.scala 42:14 helpers.scala 48:18 helpers.scala 52:18 helpers.scala 57:18 helpers.scala 61:18 helpers.scala 66:18 helpers.scala 70:18 helpers.scala 74:18 helpers.scala 78:18 helpers.scala 81:18]

  module BranchControl :
    input clock : Clock
    input reset : UInt<1>
    input io_branch : UInt<1>
    input io_funct3 : UInt<3>
    input io_inputx : UInt<32>
    input io_inputy : UInt<32>
    output io_taken : UInt<1>
  
    io_taken is invalid

  module Adder :
    input clock : Clock
    input reset : UInt<1>
    input io_inputx : UInt<32>
    input io_inputy : UInt<32>
    output io_result : UInt<32>
  
    node _T = add(io_inputx, io_inputy) @[helpers.scala 23:26]
    node _T_1 = tail(_T, 1) @[helpers.scala 23:26]
    io_result <= _T_1 @[helpers.scala 23:13]

  module SingleCycleCPU :
    input clock : Clock
    input reset : UInt<1>
    output io_imem_address : UInt<32>
    input io_imem_instruction : UInt<32>
    output io_dmem_address : UInt<32>
    output io_dmem_writedata : UInt<32>
    output io_dmem_memread : UInt<1>
    output io_dmem_memwrite : UInt<1>
    output io_dmem_maskmode : UInt<2>
    output io_dmem_sext : UInt<1>
    input io_dmem_readdata : UInt<32>
  
    inst control of Control @[cpu.scala 20:26]
    inst registers of RegisterFile @[cpu.scala 21:26]
    inst aluControl of ALUControl @[cpu.scala 22:26]
    inst alu of ALU @[cpu.scala 23:26]
    inst immGen of ImmediateGenerator @[cpu.scala 24:26]
    inst branchCtrl of BranchControl @[cpu.scala 25:26]
    inst pcPlusFour of Adder @[cpu.scala 26:26]
    inst branchAdd of Adder @[cpu.scala 27:26]
    reg pc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), pc) @[cpu.scala 19:27]
    reg value : UInt<30>, clock with :
      reset => (UInt<1>("h0"), value) @[Counter.scala 29:33]
    node _T_1 = eq(value, UInt<30>("h3fffffff")) @[Counter.scala 38:24]
    node _T_2 = add(value, UInt<1>("h1")) @[Counter.scala 39:22]
    node _T_3 = tail(_T_2, 1) @[Counter.scala 39:22]
    node _GEN_0 = mux(UInt<1>("h1"), _T_3, value) @[Counter.scala 67:17]
    node _GEN_1 = mux(UInt<1>("h1"), _T_1, UInt<1>("h0")) @[Counter.scala 67:17]
    node instruction = io_imem_instruction @[cpu.scala 37:25 cpu.scala 65:15]
    node _T_4 = bits(instruction, 24, 20) @[cpu.scala 68:39]
    node _T_5 = bits(instruction, 19, 15) @[cpu.scala 69:39]
    node _T_6 = bits(instruction, 11, 7) @[cpu.scala 70:39]
    node _T_7 = eq(registers.io_writereg, UInt<1>("h0")) @[cpu.scala 73:49]
    node _T_8 = mux(_T_7, UInt<1>("h0"), UInt<1>("h1")) @[cpu.scala 73:26]
    node _T_9 = bits(instruction, 6, 0) @[cpu.scala 75:35]
    node _T_10 = bits(instruction, 14, 12) @[cpu.scala 87:38]
    node _T_11 = bits(instruction, 31, 25) @[cpu.scala 88:38]
    node immediate = control.io_immediate @[cpu.scala 46:23 cpu.scala 79:13]
    node immgen = immGen.io_sextImm @[cpu.scala 48:20 cpu.scala 83:10]
    node readdata2 = registers.io_readdata2 @[cpu.scala 40:23 cpu.scala 72:13]
    node _T_12 = mux(immediate, immgen, readdata2) @[cpu.scala 93:23]
    node toreg = control.io_toreg @[cpu.scala 43:19 cpu.scala 76:9]
    node _T_13 = eq(toreg, UInt<1>("h0")) @[cpu.scala 100:14]
    node _T_14 = eq(toreg, UInt<1>("h1")) @[cpu.scala 102:21]
    node readdata = io_dmem_readdata @[cpu.scala 54:22 cpu.scala 98:12]
    node _GEN_2 = mux(_T_14, readdata, UInt<1>("h0")) @[cpu.scala 102:29]
    node aluresult = alu.io_result @[cpu.scala 52:23 cpu.scala 94:13]
    node _GEN_3 = mux(_T_13, aluresult, _GEN_2) @[cpu.scala 100:22]
    node _T_15 = asUInt(reset) @[cpu.scala 120:9]
    node _T_16 = eq(_T_15, UInt<1>("h0")) @[cpu.scala 120:9]
    node _T_17 = asUInt(reset) @[cpu.scala 121:9]
    node _T_18 = eq(_T_17, UInt<1>("h0")) @[cpu.scala 121:9]
    node _T_19 = asUInt(reset) @[cpu.scala 122:9]
    node _T_20 = eq(_T_19, UInt<1>("h0")) @[cpu.scala 122:9]
    node _T_21 = asUInt(reset) @[cpu.scala 124:11]
    node _T_22 = eq(_T_21, UInt<1>("h0")) @[cpu.scala 124:11]
    node _T_23 = asUInt(reset) @[cpu.scala 124:11]
    node _T_24 = eq(_T_23, UInt<1>("h0")) @[cpu.scala 124:11]
    node _T_25 = asUInt(reset) @[cpu.scala 124:11]
    node _T_26 = eq(_T_25, UInt<1>("h0")) @[cpu.scala 124:11]
    node _T_27 = asUInt(reset) @[cpu.scala 124:11]
    node _T_28 = eq(_T_27, UInt<1>("h0")) @[cpu.scala 124:11]
    node _T_29 = asUInt(reset) @[cpu.scala 124:11]
    node _T_30 = eq(_T_29, UInt<1>("h0")) @[cpu.scala 124:11]
    node _T_31 = asUInt(reset) @[cpu.scala 124:11]
    node _T_32 = eq(_T_31, UInt<1>("h0")) @[cpu.scala 124:11]
    node _T_33 = asUInt(reset) @[cpu.scala 124:11]
    node _T_34 = eq(_T_33, UInt<1>("h0")) @[cpu.scala 124:11]
    node _T_35 = asUInt(reset) @[cpu.scala 124:11]
    node _T_36 = eq(_T_35, UInt<1>("h0")) @[cpu.scala 124:11]
    node _T_37 = asUInt(reset) @[cpu.scala 126:9]
    node _T_38 = eq(_T_37, UInt<1>("h0")) @[cpu.scala 126:9]
    node _T = _GEN_1 @[Counter.scala 67:24]
    node pcaddr = pc @[cpu.scala 34:20 cpu.scala 56:10]
    node pcPlusresult = pcPlusFour.io_result @[cpu.scala 35:26 cpu.scala 61:16]
    node readdata1 = registers.io_readdata1 @[cpu.scala 39:23 cpu.scala 71:13]
    node memread = control.io_memread @[cpu.scala 42:21 cpu.scala 78:11]
    node add = control.io_add @[cpu.scala 44:17 cpu.scala 77:7]
    node regwrite = control.io_regwrite @[cpu.scala 45:22 cpu.scala 80:12]
    node operation = aluControl.io_operation @[cpu.scala 50:23 cpu.scala 89:13]
    io_imem_address <= pcaddr @[cpu.scala 63:19]
    io_dmem_address <= aluresult @[cpu.scala 96:19]
    io_dmem_writedata is invalid
    io_dmem_memread <= memread @[cpu.scala 97:19]
    io_dmem_memwrite is invalid
    io_dmem_maskmode is invalid
    io_dmem_sext is invalid
    pc <= mux(reset, UInt<1>("h0"), pcPlusresult) @[cpu.scala 57:6]
    control.clock <= clock
    control.reset <= reset
    control.io_opcode <= _T_9 @[cpu.scala 75:21]
    registers.clock <= clock
    registers.reset <= reset
    registers.io_readreg1 <= _T_4 @[cpu.scala 68:25]
    registers.io_readreg2 <= _T_5 @[cpu.scala 69:25]
    registers.io_writereg <= _T_6 @[cpu.scala 70:25]
    registers.io_writedata <= _GEN_3 @[cpu.scala 101:28 cpu.scala 103:28 cpu.scala 104:39]
    registers.io_wen <= _T_8 @[cpu.scala 73:20]
    aluControl.clock <= clock
    aluControl.reset <= reset
    aluControl.io_add <= add @[cpu.scala 85:21]
    aluControl.io_immediate <= immediate @[cpu.scala 86:27]
    aluControl.io_funct7 <= _T_11 @[cpu.scala 88:24]
    aluControl.io_funct3 <= _T_10 @[cpu.scala 87:24]
    alu.clock <= clock
    alu.reset <= reset
    alu.io_operation <= operation @[cpu.scala 91:20]
    alu.io_inputx <= readdata1 @[cpu.scala 92:17]
    alu.io_inputy <= _T_12 @[cpu.scala 93:17]
    immGen.clock <= clock
    immGen.reset <= reset
    immGen.io_instruction <= instruction @[cpu.scala 82:25]
    branchCtrl.clock <= clock
    branchCtrl.reset <= reset
    branchCtrl.io_branch is invalid
    branchCtrl.io_funct3 is invalid
    branchCtrl.io_inputx is invalid
    branchCtrl.io_inputy is invalid
    pcPlusFour.clock <= clock
    pcPlusFour.reset <= reset
    pcPlusFour.io_inputx <= pcaddr @[cpu.scala 59:24]
    pcPlusFour.io_inputy <= UInt<3>("h4") @[cpu.scala 60:24]
    branchAdd.clock <= clock
    branchAdd.reset <= reset
    branchAdd.io_inputx is invalid
    branchAdd.io_inputy is invalid
    value <= mux(reset, UInt<30>("h0"), _GEN_0) @[Counter.scala 39:13]
    printf(clock, and(and(UInt<1>("h1"), _T_16), UInt<1>("h1")), "DASM(%x)\n", instruction) @[cpu.scala 120:9]
    printf(clock, and(and(UInt<1>("h1"), _T_18), UInt<1>("h1")), "CYCLE=%d\n", value) @[cpu.scala 121:9]
    printf(clock, and(and(UInt<1>("h1"), _T_20), UInt<1>("h1")), "pc: %d\n", pc) @[cpu.scala 122:9]
    printf(clock, and(and(UInt<1>("h1"), _T_22), UInt<1>("h1")), "control: AnonymousBundle(opcode -> %d, branch -> %d, memread -> %d, toreg -> %d, add -> %d, memwrite -> %d, regwrite -> %d, immediate -> %d, alusrc1 -> %d, jump -> %d)\n", control.io_opcode, control.io_branch, control.io_memread, control.io_toreg, control.io_add, control.io_memwrite, control.io_regwrite, control.io_immediate, control.io_alusrc1, control.io_jump) @[cpu.scala 124:11]
    printf(clock, and(and(UInt<1>("h1"), _T_24), UInt<1>("h1")), "registers: AnonymousBundle(readreg1 -> %d, readreg2 -> %d, writereg -> %d, writedata -> %d, wen -> %d, readdata1 -> %d, readdata2 -> %d)\n", registers.io_readreg1, registers.io_readreg2, registers.io_writereg, registers.io_writedata, registers.io_wen, registers.io_readdata1, registers.io_readdata2) @[cpu.scala 124:11]
    printf(clock, and(and(UInt<1>("h1"), _T_26), UInt<1>("h1")), "aluControl: AnonymousBundle(add -> %d, immediate -> %d, funct7 -> %d, funct3 -> %d, operation -> %d)\n", aluControl.io_add, aluControl.io_immediate, aluControl.io_funct7, aluControl.io_funct3, aluControl.io_operation) @[cpu.scala 124:11]
    printf(clock, and(and(UInt<1>("h1"), _T_28), UInt<1>("h1")), "alu: AnonymousBundle(operation -> %d, inputx -> %d, inputy -> %d, result -> %d)\n", alu.io_operation, alu.io_inputx, alu.io_inputy, alu.io_result) @[cpu.scala 124:11]
    printf(clock, and(and(UInt<1>("h1"), _T_30), UInt<1>("h1")), "immGen: AnonymousBundle(instruction -> %d, sextImm -> %d)\n", immGen.io_instruction, immGen.io_sextImm) @[cpu.scala 124:11]
    printf(clock, and(and(UInt<1>("h1"), _T_32), UInt<1>("h1")), "branchCtrl: AnonymousBundle(branch -> %d, funct3 -> %d, inputx -> %d, inputy -> %d, taken -> %d)\n", branchCtrl.io_branch, branchCtrl.io_funct3, branchCtrl.io_inputx, branchCtrl.io_inputy, branchCtrl.io_taken) @[cpu.scala 124:11]
    printf(clock, and(and(UInt<1>("h1"), _T_34), UInt<1>("h1")), "pcPlusFour: AnonymousBundle(inputx -> %d, inputy -> %d, result -> %d)\n", pcPlusFour.io_inputx, pcPlusFour.io_inputy, pcPlusFour.io_result) @[cpu.scala 124:11]
    printf(clock, and(and(UInt<1>("h1"), _T_36), UInt<1>("h1")), "branchAdd: AnonymousBundle(inputx -> %d, inputy -> %d, result -> %d)\n", branchAdd.io_inputx, branchAdd.io_inputy, branchAdd.io_result) @[cpu.scala 124:11]
    printf(clock, and(and(UInt<1>("h1"), _T_38), UInt<1>("h1")), "\n") @[cpu.scala 126:9]

  module DualPortedMemory :
    input clock : Clock
    input reset : UInt<1>
    input io_imem_address : UInt<32>
    output io_imem_instruction : UInt<32>
    input io_dmem_address : UInt<32>
    input io_dmem_writedata : UInt<32>
    input io_dmem_memread : UInt<1>
    input io_dmem_memwrite : UInt<1>
    input io_dmem_maskmode : UInt<2>
    input io_dmem_sext : UInt<1>
    output io_dmem_readdata : UInt<32>
  
    mem memory : @[memory.scala 60:19]
      data-type => UInt<32>
      depth => 1024
      read-latency => 0
      write-latency => 1
      reader => _T_2
      reader => _T_8
      reader => _T_14
      reader => _T_18
      reader => _T_22
      reader => _T_39
      writer => _T_52
      writer => _T_58
      read-under-write => undefined
    node _T = shr(io_imem_address, 2) @[memory.scala 63:49]
    node _T_1 = bits(_T, 9, 0) @[memory.scala 63:32]
    node _T_4 = neq(io_dmem_maskmode, UInt<2>("h2")) @[memory.scala 68:28]
    node _T_5 = bits(io_dmem_address, 1, 0) @[memory.scala 69:35]
    node _T_6 = shr(io_dmem_address, 2) @[memory.scala 70:42]
    node _T_7 = bits(_T_6, 9, 0) @[memory.scala 70:25]
    node _T_9 = mul(_T_5, UInt<4>("h8")) @[memory.scala 70:59]
    node _T_10 = dshr(memory._T_8.data, _T_9) @[memory.scala 70:48]
    node _T_11 = eq(io_dmem_maskmode, UInt<1>("h0")) @[memory.scala 71:30]
    node _T_12 = shr(io_dmem_address, 2) @[memory.scala 72:44]
    node _T_13 = bits(_T_12, 9, 0) @[memory.scala 72:27]
    node _T_15 = and(memory._T_14.data, UInt<8>("hff")) @[memory.scala 72:50]
    node _T_16 = shr(io_dmem_address, 2) @[memory.scala 74:44]
    node _T_17 = bits(_T_16, 9, 0) @[memory.scala 74:27]
    node _T_19 = and(memory._T_18.data, UInt<16>("hffff")) @[memory.scala 74:50]
    node _GEN_0 = validif(_T_11, _T_13) @[memory.scala 71:39]
    node _GEN_1 = validif(_T_11, clock) @[memory.scala 71:39]
    node _GEN_2 = mux(_T_11, UInt<1>("h1"), UInt<1>("h0")) @[memory.scala 71:39]
    node _GEN_3 = mux(_T_11, _T_15, _T_19) @[memory.scala 71:39]
    node _GEN_4 = validif(eq(_T_11, UInt<1>("h0")), _T_17) @[memory.scala 71:39]
    node _GEN_5 = validif(eq(_T_11, UInt<1>("h0")), clock) @[memory.scala 71:39]
    node _GEN_6 = mux(_T_11, UInt<1>("h0"), UInt<1>("h1")) @[memory.scala 71:39]
    node _T_20 = shr(io_dmem_address, 2) @[memory.scala 77:42]
    node _T_21 = bits(_T_20, 9, 0) @[memory.scala 77:25]
    node _GEN_7 = validif(_T_4, _T_7) @[memory.scala 68:37]
    node _GEN_8 = validif(_T_4, clock) @[memory.scala 68:37]
    node _GEN_9 = mux(_T_4, UInt<1>("h1"), UInt<1>("h0")) @[memory.scala 68:37]
    node _GEN_10 = mux(_T_4, _GEN_3, memory._T_22.data) @[memory.scala 68:37]
    node _GEN_11 = validif(_T_4, _GEN_0) @[memory.scala 68:37]
    node _GEN_12 = validif(_T_4, _GEN_1) @[memory.scala 68:37]
    node _GEN_13 = mux(_T_4, _GEN_2, UInt<1>("h0")) @[memory.scala 68:37]
    node _GEN_14 = validif(_T_4, _GEN_4) @[memory.scala 68:37]
    node _GEN_15 = validif(_T_4, _GEN_5) @[memory.scala 68:37]
    node _GEN_16 = mux(_T_4, _GEN_6, UInt<1>("h0")) @[memory.scala 68:37]
    node _GEN_17 = validif(eq(_T_4, UInt<1>("h0")), _T_21) @[memory.scala 68:37]
    node _GEN_18 = validif(eq(_T_4, UInt<1>("h0")), clock) @[memory.scala 68:37]
    node _GEN_19 = mux(_T_4, UInt<1>("h0"), UInt<1>("h1")) @[memory.scala 68:37]
    node _T_23 = eq(io_dmem_maskmode, UInt<1>("h0")) @[memory.scala 81:30]
    node _T_3 = _GEN_10 @[memory.scala 66:24 memory.scala 70:16 memory.scala 72:18 memory.scala 74:18 memory.scala 77:16]
    node _T_24 = bits(_T_3, 7, 7) @[memory.scala 82:50]
    node _T_25 = bits(_T_24, 0, 0) @[Bitwise.scala 72:15]
    node _T_26 = mux(_T_25, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 72:12]
    node _T_27 = bits(_T_3, 7, 0) @[memory.scala 82:64]
    node _T_28 = cat(_T_26, _T_27) @[Cat.scala 30:58]
    node _T_29 = bits(_T_3, 15, 15) @[memory.scala 84:50]
    node _T_30 = bits(_T_29, 0, 0) @[Bitwise.scala 72:15]
    node _T_31 = mux(_T_30, UInt<16>("hffff"), UInt<16>("h0")) @[Bitwise.scala 72:12]
    node _T_32 = bits(_T_3, 15, 0) @[memory.scala 84:65]
    node _T_33 = cat(_T_31, _T_32) @[Cat.scala 30:58]
    node _GEN_20 = mux(_T_23, _T_28, _T_33) @[memory.scala 81:39]
    node _GEN_21 = mux(io_dmem_sext, _GEN_20, _T_3) @[memory.scala 80:25]
    node _GEN_22 = validif(io_dmem_memread, _GEN_7) @[memory.scala 65:26]
    node _GEN_23 = validif(io_dmem_memread, _GEN_8) @[memory.scala 65:26]
    node _GEN_24 = mux(io_dmem_memread, _GEN_9, UInt<1>("h0")) @[memory.scala 65:26]
    node _GEN_25 = validif(io_dmem_memread, _GEN_11) @[memory.scala 65:26]
    node _GEN_26 = validif(io_dmem_memread, _GEN_12) @[memory.scala 65:26]
    node _GEN_27 = mux(io_dmem_memread, _GEN_13, UInt<1>("h0")) @[memory.scala 65:26]
    node _GEN_28 = validif(io_dmem_memread, _GEN_14) @[memory.scala 65:26]
    node _GEN_29 = validif(io_dmem_memread, _GEN_15) @[memory.scala 65:26]
    node _GEN_30 = mux(io_dmem_memread, _GEN_16, UInt<1>("h0")) @[memory.scala 65:26]
    node _GEN_31 = validif(io_dmem_memread, _GEN_17) @[memory.scala 65:26]
    node _GEN_32 = validif(io_dmem_memread, _GEN_18) @[memory.scala 65:26]
    node _GEN_33 = mux(io_dmem_memread, _GEN_19, UInt<1>("h0")) @[memory.scala 65:26]
    node _GEN_34 = validif(io_dmem_memread, _GEN_21) @[memory.scala 65:26]
    node _T_34 = neq(io_dmem_maskmode, UInt<2>("h2")) @[memory.scala 92:28]
    node _T_35 = bits(io_dmem_address, 1, 0) @[memory.scala 93:35]
    node _T_37 = shr(io_dmem_address, 2) @[memory.scala 96:42]
    node _T_38 = bits(_T_37, 9, 0) @[memory.scala 96:25]
    node _T_41 = eq(io_dmem_maskmode, UInt<1>("h0")) @[memory.scala 99:30]
    node _T_42 = mul(_T_35, UInt<4>("h8")) @[memory.scala 100:48]
    node _T_43 = dshl(UInt<8>("hff"), _T_42) @[memory.scala 100:37]
    node _T_44 = not(_T_43) @[memory.scala 100:28]
    node _T_36 = memory._T_39.data @[memory.scala 95:26 memory.scala 96:16]
    node _T_45 = and(_T_36, _T_44) @[memory.scala 100:26]
    node _T_46 = mul(_T_35, UInt<4>("h8")) @[memory.scala 102:50]
    node _T_47 = dshl(UInt<16>("hffff"), _T_46) @[memory.scala 102:39]
    node _T_48 = not(_T_47) @[memory.scala 102:28]
    node _T_49 = and(_T_36, _T_48) @[memory.scala 102:26]
    node _GEN_35 = mux(_T_41, _T_45, _T_49) @[memory.scala 99:39]
    node _T_50 = shr(io_dmem_address, 2) @[memory.scala 104:30]
    node _T_51 = bits(_T_50, 9, 0) @[memory.scala 104:13]
    node _T_53 = mul(_T_35, UInt<4>("h8")) @[memory.scala 104:76]
    node _T_54 = dshl(io_dmem_writedata, _T_53) @[memory.scala 104:65]
    node _T_40 = bits(_GEN_35, 31, 0) @[memory.scala 98:22 memory.scala 100:14 memory.scala 102:14]
    node _T_55 = or(_T_40, _T_54) @[memory.scala 104:44]
    node _T_56 = shr(io_dmem_address, 2) @[memory.scala 106:30]
    node _T_57 = bits(_T_56, 9, 0) @[memory.scala 106:13]
    node _GEN_36 = validif(_T_34, _T_38) @[memory.scala 92:37]
    node _GEN_37 = validif(_T_34, clock) @[memory.scala 92:37]
    node _GEN_38 = mux(_T_34, UInt<1>("h1"), UInt<1>("h0")) @[memory.scala 92:37]
    node _GEN_39 = validif(_T_34, _T_51) @[memory.scala 92:37]
    node _GEN_40 = validif(_T_34, UInt<1>("h1")) @[memory.scala 92:37]
    node _GEN_41 = validif(_T_34, _T_55) @[memory.scala 92:37]
    node _GEN_42 = validif(eq(_T_34, UInt<1>("h0")), _T_57) @[memory.scala 92:37]
    node _GEN_43 = validif(eq(_T_34, UInt<1>("h0")), clock) @[memory.scala 92:37]
    node _GEN_44 = mux(_T_34, UInt<1>("h0"), UInt<1>("h1")) @[memory.scala 92:37]
    node _GEN_45 = validif(eq(_T_34, UInt<1>("h0")), UInt<1>("h1")) @[memory.scala 92:37]
    node _GEN_46 = validif(eq(_T_34, UInt<1>("h0")), io_dmem_writedata) @[memory.scala 92:37]
    node _GEN_47 = validif(io_dmem_memwrite, _GEN_36) @[memory.scala 91:27]
    node _GEN_48 = validif(io_dmem_memwrite, _GEN_37) @[memory.scala 91:27]
    node _GEN_49 = mux(io_dmem_memwrite, _GEN_38, UInt<1>("h0")) @[memory.scala 91:27]
    node _GEN_50 = validif(io_dmem_memwrite, _GEN_39) @[memory.scala 91:27]
    node _GEN_51 = validif(io_dmem_memwrite, _GEN_40) @[memory.scala 91:27]
    node _GEN_52 = validif(io_dmem_memwrite, _GEN_41) @[memory.scala 91:27]
    node _GEN_53 = validif(io_dmem_memwrite, _GEN_42) @[memory.scala 91:27]
    node _GEN_54 = validif(io_dmem_memwrite, _GEN_43) @[memory.scala 91:27]
    node _GEN_55 = mux(io_dmem_memwrite, _GEN_44, UInt<1>("h0")) @[memory.scala 91:27]
    node _GEN_56 = validif(io_dmem_memwrite, _GEN_45) @[memory.scala 91:27]
    node _GEN_57 = validif(io_dmem_memwrite, _GEN_46) @[memory.scala 91:27]
    io_imem_instruction <= memory._T_2.data @[memory.scala 63:23]
    io_dmem_readdata <= _GEN_34 @[memory.scala 82:26 memory.scala 84:26 memory.scala 87:24]
    memory._T_2.addr <= _T_1 @[memory.scala 63:32]
    memory._T_2.en <= UInt<1>("h1") @[memory.scala 60:19 memory.scala 63:32]
    memory._T_2.clk <= clock @[memory.scala 63:32]
    memory._T_8.addr <= _GEN_22 @[memory.scala 70:25]
    memory._T_8.en <= _GEN_24 @[memory.scala 60:19 memory.scala 70:25]
    memory._T_8.clk <= _GEN_23 @[memory.scala 70:25]
    memory._T_14.addr <= _GEN_25 @[memory.scala 72:27]
    memory._T_14.en <= _GEN_27 @[memory.scala 60:19 memory.scala 72:27]
    memory._T_14.clk <= _GEN_26 @[memory.scala 72:27]
    memory._T_18.addr <= _GEN_28 @[memory.scala 74:27]
    memory._T_18.en <= _GEN_30 @[memory.scala 60:19 memory.scala 74:27]
    memory._T_18.clk <= _GEN_29 @[memory.scala 74:27]
    memory._T_22.addr <= _GEN_31 @[memory.scala 77:25]
    memory._T_22.en <= _GEN_33 @[memory.scala 60:19 memory.scala 77:25]
    memory._T_22.clk <= _GEN_32 @[memory.scala 77:25]
    memory._T_39.addr <= _GEN_47 @[memory.scala 96:25]
    memory._T_39.en <= _GEN_49 @[memory.scala 60:19 memory.scala 96:25]
    memory._T_39.clk <= _GEN_48 @[memory.scala 96:25]
    memory._T_52.addr <= _GEN_50 @[memory.scala 104:13]
    memory._T_52.en <= _GEN_49 @[memory.scala 60:19 memory.scala 104:13]
    memory._T_52.clk <= _GEN_48 @[memory.scala 104:13]
    memory._T_52.data <= bits(_GEN_52, 31, 0) @[memory.scala 104:36]
    memory._T_52.mask <= _GEN_51 @[memory.scala 104:13 memory.scala 104:36]
    memory._T_58.addr <= _GEN_53 @[memory.scala 106:13]
    memory._T_58.en <= _GEN_55 @[memory.scala 60:19 memory.scala 106:13]
    memory._T_58.clk <= _GEN_54 @[memory.scala 106:13]
    memory._T_58.data <= _GEN_57 @[memory.scala 106:36]
    memory._T_58.mask <= _GEN_56 @[memory.scala 106:13 memory.scala 106:36]

  module Top :
    input clock : Clock
    input reset : UInt<1>
    output io_success : UInt<1>
  
    inst cpu of SingleCycleCPU @[top.scala 14:21]
    inst mem of DualPortedMemory @[top.scala 15:20]
    io_success is invalid
    cpu.clock <= clock
    cpu.reset <= reset
    cpu.io_imem_instruction <= mem.io_imem_instruction @[top.scala 17:15]
    cpu.io_dmem_readdata <= mem.io_dmem_readdata @[top.scala 18:15]
    mem.clock <= clock
    mem.reset <= reset
    mem.io_imem_address <= cpu.io_imem_address @[top.scala 17:15]
    mem.io_dmem_address <= cpu.io_dmem_address @[top.scala 18:15]
    mem.io_dmem_writedata <= cpu.io_dmem_writedata @[top.scala 18:15]
    mem.io_dmem_memread <= cpu.io_dmem_memread @[top.scala 18:15]
    mem.io_dmem_memwrite <= cpu.io_dmem_memwrite @[top.scala 18:15]
    mem.io_dmem_maskmode <= cpu.io_dmem_maskmode @[top.scala 18:15]
    mem.io_dmem_sext <= cpu.io_dmem_sext @[top.scala 18:15]
