package javabackend

import essent.{IsSimulator, SimulatorWrapper}
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

// sbt "testOnly *javabackend.GCDTest"
// sbt "testOnly *javabackend.GCDTest -- -z testZero"
class GCDTest extends AnyFreeSpec{
  private val source = """
     |circuit DecoupledGCD :
     |  module DecoupledGCD :
     |    input clock : Clock
     |    input reset : UInt<1>
     |    input input : { flip ready : UInt<1>, valid : UInt<1>, bits : { value1 : UInt<60>, value2 : UInt<60>}}
     |    output output : { flip ready : UInt<1>, valid : UInt<1>, bits : { value1 : UInt<60>, value2 : UInt<60>, gcd : UInt<60>}}
     |
     |    reg xInitial : UInt<60>, clock with :
     |      reset => (UInt<1>("h0"), xInitial) @[main.scala 27:24]
     |    reg yInitial : UInt<60>, clock with :
     |      reset => (UInt<1>("h0"), yInitial) @[main.scala 28:24]
     |    reg x : UInt<60>, clock with :
     |      reset => (UInt<1>("h0"), x) @[main.scala 29:24]
     |    reg y : UInt<60>, clock with :
     |      reset => (UInt<1>("h0"), y) @[main.scala 30:24]
     |    reg busy : UInt<1>, clock with :
     |      reset => (reset, UInt<1>("h0")) @[main.scala 31:28]
     |    reg resultValid : UInt<1>, clock with :
     |      reset => (reset, UInt<1>("h0")) @[main.scala 60:28]
     |    node _input_ready_T = eq(busy, UInt<1>("h0")) @[main.scala 34:18]
     |    input.ready <= _input_ready_T @[main.scala 34:15]
     |    output.valid <= resultValid @[main.scala 35:16]
     |    output.bits.gcd is invalid @[main.scala 36:15]
     |    output.bits.value2 is invalid @[main.scala 36:15]
     |    output.bits.value1 is invalid @[main.scala 36:15]
     |    reg cycle : UInt<60>, clock with :
     |      reset => (reset, UInt<60>("h0")) @[main.scala 38:22]
     |    node _cycle_T = add(cycle, UInt<1>("h1")) @[main.scala 39:18]
     |    node _cycle_T_1 = tail(_cycle_T, 1) @[main.scala 39:18]
     |    cycle <= _cycle_T_1 @[main.scala 39:9]
     |    when busy : @[main.scala 44:15]
     |      node _T = geq(x, y) @[main.scala 45:12]
     |      when _T : @[main.scala 45:18]
     |        node _x_T = sub(x, y) @[main.scala 46:14]
     |        node _x_T_1 = tail(_x_T, 1) @[main.scala 46:14]
     |        x <= _x_T_1 @[main.scala 46:9]
     |      else :
     |        node _y_T = sub(y, x) @[main.scala 48:14]
     |        node _y_T_1 = tail(_y_T, 1) @[main.scala 48:14]
     |        y <= _y_T_1 @[main.scala 48:9]
     |      node _T_1 = eq(x, UInt<1>("h0")) @[main.scala 50:12]
     |      node _T_2 = eq(y, UInt<1>("h0")) @[main.scala 50:25]
     |      node _T_3 = or(_T_1, _T_2) @[main.scala 50:20]
     |      when _T_3 : @[main.scala 50:34]
     |        node _T_4 = eq(x, UInt<1>("h0")) @[main.scala 51:14]
     |        when _T_4 : @[main.scala 51:23]
     |          output.bits.gcd <= y @[main.scala 52:25]
     |        else :
     |          output.bits.gcd <= x @[main.scala 54:25]
     |        output.bits.value1 <= xInitial @[main.scala 57:26]
     |        output.bits.value2 <= yInitial @[main.scala 58:26]
     |        resultValid <= UInt<1>("h1") @[main.scala 59:19]
     |        node _T_5 = and(output.ready, resultValid) @[main.scala 61:25]
     |        when _T_5 : @[main.scala 61:41]
     |          busy <= UInt<1>("h0") @[main.scala 62:14]
     |          resultValid <= UInt<1>("h0") @[main.scala 63:21]
     |    else :
     |      when input.valid : @[main.scala 67:23]
     |        input.ready <= UInt<1>("h1") @[Decoupled.scala 81:20]
     |        x <= input.bits.value1 @[main.scala 69:9]
     |        y <= input.bits.value2 @[main.scala 70:9]
     |        xInitial <= input.bits.value1 @[main.scala 71:16]
     |        yInitial <= input.bits.value2 @[main.scala 72:16]
     |        busy <= UInt<1>("h1") @[main.scala 73:12]
     |""".stripMargin

  private def runTest(dut: IsSimulator, testValues: Iterable[(BigInt, BigInt)]): Unit = {
    dut.poke("reset", 1)
    dut.step(true)
    dut.step(true)
    dut.poke("reset", 0)

    dut.poke("output_ready", 1)
    for((i, j) <- testValues) {
      dut.poke("input_bits_value1", i)
      dut.poke("input_bits_value2", j)
      dut.poke("input_valid", 1)
      dut.step(true)

      while(dut.peek("output_valid") == 0) {
        dut.step(true)
      }
      dut.peek("output_bits_gcd")
      dut.peek("output_valid")
    }
  }

  "testZero" in {
    val essentSim = SimulatorWrapper(source)
    val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
    val dut = new DeltaTester(treadleSim, essentSim, Seq("x", "y", "xInitial", "yInitial", "busy", "resultValid"))

    val numMax = 200
    val testValues = for {x <- 2 to numMax; y <- 2 to numMax} yield (BigInt(x), BigInt(y))
    runTest(dut, testValues)
  }
}