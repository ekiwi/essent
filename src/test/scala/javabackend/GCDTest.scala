package javabackend

import essent.{Driver, IsSimulator, JavaRuntimeCompiler, SimulatorWrapper}
import firrtl.stage.FirrtlFileAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

// sbt "testOnly *javabackend.GCDTest"
// sbt "testOnly *javabackend.GCDTest -- -z testZero"
class GCDTest extends AnyFreeSpec{
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
    Driver.main(Array("-O0", "-java", System.getProperty("user.dir") + "/examples/DecoupledGCD.fir"))
    val essentSim : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile(os.pwd / "examples" / "DecoupledGCD.java"))
    val treadleSim = TreadleTester(Seq(FirrtlFileAnnotation(System.getProperty("user.dir") + "/examples/DecoupledGCD.fir")))
    val dut = new DeltaTester(treadleSim, essentSim, Seq("x", "y", "xInitial", "yInitial", "busy", "resultValid"))

    val numMax = 200
    val testValues = for {x <- 2 to numMax; y <- 2 to numMax} yield (BigInt(x), BigInt(y))
    runTest(dut, testValues)
  }
}
