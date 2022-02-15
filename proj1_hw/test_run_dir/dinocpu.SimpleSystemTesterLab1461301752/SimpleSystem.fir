;buildInfoPackage: chisel3, version: 3.2.8, scalaVersion: 2.12.10, sbtVersion: 1.3.2
circuit SimpleSystem : 
  module SimpleAdder : 
    input clock : Clock
    input reset : Reset
    output io : {flip inputx : UInt<32>, flip inputy : UInt<32>, result : UInt<32>}
    
    node _T = add(io.inputx, io.inputy) @[simple.scala 11:28]
    node _T_1 = tail(_T, 1) @[simple.scala 11:28]
    io.result <= _T_1 @[simple.scala 11:15]
    
  module SimpleAdder_1 : 
    input clock : Clock
    input reset : Reset
    output io : {flip inputx : UInt<32>, flip inputy : UInt<32>, result : UInt<32>}
    
    node _T = add(io.inputx, io.inputy) @[simple.scala 11:28]
    node _T_1 = tail(_T, 1) @[simple.scala 11:28]
    io.result <= _T_1 @[simple.scala 11:15]
    
  module SimpleSystem : 
    input clock : Clock
    input reset : UInt<1>
    output io : {success : UInt<1>}
    
    reg reg1 : UInt<32>, clock with : (reset => (reset, UInt<32>("h01"))) @[simple.scala 18:23]
    reg reg2 : UInt<32>, clock with : (reset => (reset, UInt<32>("h00"))) @[simple.scala 19:23]
    inst adder1 of SimpleAdder @[simple.scala 20:24]
    adder1.clock <= clock
    adder1.reset <= reset
    inst adder2 of SimpleAdder_1 @[simple.scala 21:24]
    adder2.clock <= clock
    adder2.reset <= reset
    reg1 <= adder1.io.result @[simple.scala 23:10]
    adder1.io.inputx <= reg1 @[simple.scala 24:22]
    reg2 <= adder2.io.result @[simple.scala 25:10]
    adder1.io.inputy <= reg2 @[simple.scala 26:22]
    adder2.io.inputx <= adder1.io.result @[simple.scala 27:22]
    adder2.io.inputy <= UInt<32>("h03") @[simple.scala 28:22]
    node _T = eq(adder2.io.result, UInt<32>("h080")) @[simple.scala 30:40]
    node _T_1 = mux(_T, UInt<1>("h01"), UInt<1>("h00")) @[simple.scala 30:22]
    io.success <= _T_1 @[simple.scala 30:16]
    node _T_2 = bits(reset, 0, 0) @[simple.scala 32:11]
    node _T_3 = eq(_T_2, UInt<1>("h00")) @[simple.scala 32:11]
    when _T_3 : @[simple.scala 32:11]
      printf(clock, UInt<1>(1), "reg1: %d, reg2: %d, success: %d\n", reg1, reg2, io.success) @[simple.scala 32:11]
      skip @[simple.scala 32:11]
    
