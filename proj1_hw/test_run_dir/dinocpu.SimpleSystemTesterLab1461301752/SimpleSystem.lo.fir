circuit SimpleSystem :
  module SimpleAdder :
    input clock : Clock
    input reset : UInt<1>
    input io_inputx : UInt<32>
    input io_inputy : UInt<32>
    output io_result : UInt<32>
  
    node _T = add(io_inputx, io_inputy) @[simple.scala 11:28]
    node _T_1 = tail(_T, 1) @[simple.scala 11:28]
    io_result <= _T_1 @[simple.scala 11:15]

  module SimpleSystem :
    input clock : Clock
    input reset : UInt<1>
    output io_success : UInt<1>
  
    inst adder1 of SimpleAdder @[simple.scala 20:24]
    inst adder2 of SimpleAdder @[simple.scala 21:24]
    reg reg1 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg1) @[simple.scala 18:23]
    reg reg2 : UInt<32>, clock with :
      reset => (UInt<1>("h0"), reg2) @[simple.scala 19:23]
    node _T = eq(adder2.io_result, UInt<32>("h80")) @[simple.scala 30:40]
    node _T_1 = mux(_T, UInt<1>("h1"), UInt<1>("h0")) @[simple.scala 30:22]
    node _T_2 = bits(reset, 0, 0) @[simple.scala 32:11]
    node _T_3 = eq(_T_2, UInt<1>("h0")) @[simple.scala 32:11]
    io_success <= _T_1 @[simple.scala 30:16]
    reg1 <= mux(reset, UInt<32>("h1"), adder1.io_result) @[simple.scala 23:10]
    reg2 <= mux(reset, UInt<32>("h0"), adder2.io_result) @[simple.scala 25:10]
    adder1.clock <= clock
    adder1.reset <= reset
    adder1.io_inputx <= reg1 @[simple.scala 24:22]
    adder1.io_inputy <= reg2 @[simple.scala 26:22]
    adder2.clock <= clock
    adder2.reset <= reset
    adder2.io_inputx <= adder1.io_result @[simple.scala 27:22]
    adder2.io_inputy <= UInt<32>("h3") @[simple.scala 28:22]
    printf(clock, and(and(UInt<1>("h1"), _T_3), UInt<1>("h1")), "reg1: %d, reg2: %d, success: %d\n", reg1, reg2, io_success) @[simple.scala 32:11]
