package javabackend

import essent.{Driver, IsSimulator, JavaRuntimeCompiler, SimulatorWrapper}
import firrtl.stage.FirrtlFileAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

class NestedModulesTest extends AnyFreeSpec{

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
    Driver.main(Array("-O0", "-java", System.getProperty("user.dir") + "/examples/NestedModsWithReg.fir"))
    val essentSim : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile(os.pwd / "examples" / "Top.java"))
    val treadleSim = TreadleTester(Seq(FirrtlFileAnnotation(System.getProperty("user.dir") + "/examples/NestedModsWithReg.fir")))
    val dut = new DeltaTester(treadleSim, essentSim, Seq("in1", "out1", "out2", "out3"))

    val numMax = 200
    val testValues = for {x <- 2 to numMax; y <- 2 to numMax} yield BigInt(y)
    runTest(dut, testValues)
  }
}
