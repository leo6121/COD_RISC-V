// Tests for Lab 1. Feel free to modify and add more tests here.
// If you name your test class something that ends with "TesterLab1" it will
// automatically be run when you use `Lab1 / test` at the sbt prompt.

package dinocpu

import chisel3._

import chisel3.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}


class SimpleSystemUnitTester(c: SimpleSystem) extends PeekPokeTester(c) {

  
  for (i <- 0 until 10) {
    step(1)
    val expectOut =  (i == 4)
    expect(c.io.success, expectOut, s"for SimpleSystemTester")
  }
}

/**
  * This is a trivial example of how to run this Specification
  * From within sbt use:
  * {{{
  * Lab1 / testOnly dinocpu.SimpleSystemTesterLab1
  * }}}
  * From a terminal shell use:
  * {{{
  * sbt 'testOnly dinocpu.SimpleSystemTesterLab1'
  * }}}
  */

class SimpleSystemTesterLab1 extends ChiselFlatSpec {
  "SimpleSystem" should s"match expectations for unit tests" in {
    Driver(() => new SimpleSystem) {
      c => new SimpleSystemUnitTester(c)
    } should be (true) 
  }
}

