package javabackend

import essent.{IsSimulator, SimulatorWrapper}
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

class NestedModulesTest extends AnyFreeSpec{
  private val source =
    """
      |circuit Top :
      |    module Level1 :
      |        input clk : Clock
      |        input reset : UInt<1>
      |        input in1   : UInt<16>
      |        output out1 : UInt<16>
      |        output out2 : UInt<16>
      |        output out3 : UInt<16>
      |
      |        reg reg1 : UInt<16>, clk
      |
      |        reg1 <= in1
      |        out1 <= reg1
      |
      |        inst level2 of Level2
      |        level2.clk <= clk
      |        level2.reset <= reset
      |
      |        level2.in1 <= in1
      |        out2 <= level2.out2
      |        out3 <= level2.out3
      |
      |    module Level2 :
      |        input clk : Clock
      |        input reset : UInt<1>
      |        input in1 : UInt<16>
      |        output out2 : UInt<16>
      |        output out3 : UInt<16>
      |
      |        reg reg2 : UInt<16>, clk
      |
      |        reg2 <= in1
      |        out2 <= reg2
      |
      |        inst level3 of Level3
      |        level3.clk <= clk
      |        level3.reset <= reset
      |
      |        level3.in1 <= in1
      |        out3 <= level3.out3
      |
      |    module Level3 :
      |        input clk : Clock
      |        input reset : UInt<1>
      |        input in1 : UInt<16>
      |        output out3 : UInt<16>
      |
      |        reg reg3 : UInt<16>, clk
      |
      |        reg3 <= in1
      |        out3 <= in1
      |
      |    module Top :
      |        input clk : Clock
      |        input reset : UInt<1>
      |        input in1 : UInt<16>
      |        output out1 : UInt<16>
      |        output out2 : UInt<16>
      |        output out3 : UInt<16>
      |
      |        inst level1 of Level1
      |        level1.clk <= clk
      |        level1.reset <= reset
      |
      |        level1.in1 <= in1
      |        out1 <= level1.out1
      |        out2 <= level1.out2
      |        out3 <= level1.out3
      |""".stripMargin

  private def runTest(dut: IsSimulator, testValues: Iterable[BigInt]): Unit = {
    dut.poke("reset", 1)
    dut.step(true)
    dut.step(true)
    dut.poke("reset", 0)

    for (i <- testValues) {
      dut.poke("in1", i)
      dut.step(true)
    }
  }

  "testZero" in {
    val essentSim = SimulatorWrapper(source)
    val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
    val dut = new DeltaTester(treadleSim, essentSim, Seq("in1", "out1", "out2", "out3"))

    val numMax = 200
    val testValues = for {x <- 1 to numMax} yield BigInt(x)
    runTest(dut, testValues)
  }
}
