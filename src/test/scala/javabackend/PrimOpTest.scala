package javabackend

import essent.{Driver, IsSimulator, JavaRuntimeCompiler, SimulatorWrapper}
import firrtl.stage.FirrtlFileAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

class PrimOpTest extends AnyFreeSpec{

  private def runTest(dut: IsSimulator, testValues: Iterable[(BigInt, BigInt)]): Unit = {
    for ((i, j) <- testValues) {
      dut.poke("io_UArg1", i)
      dut.poke("io_UArg2", j)
      dut.poke("io_SArg1", i)
      dut.poke("io_SArg2", j)
      dut.step(true)
    }
  }

  "testZero" in {
    Driver.main(Array("-O0", "-java", System.getProperty("user.dir") + "/examples/PrimOpTester.fir"))
    val essentSim : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile(os.pwd / "examples" / "PrimOpTester.java"))
    val treadleSim = TreadleTester(Seq(FirrtlFileAnnotation(System.getProperty("user.dir") + "/examples/PrimOpTester.fir")))
    val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))

    val numMax = 100
    val testValues = for {x <- 1 to numMax} yield (BigInt(x), BigInt(x))
    runTest(dut, testValues)
  }
}
