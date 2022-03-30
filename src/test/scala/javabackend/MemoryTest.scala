package javabackend

import essent.{Driver, IsSimulator, JavaRuntimeCompiler, SimulatorWrapper}
import firrtl.stage.FirrtlFileAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

class MemoryTest extends AnyFreeSpec{

  "ReadWriteMemory" in {
    Driver.main(Array("-O0", "-java", System.getProperty("user.dir") + "/examples/ReadWriteSyncMem.fir"))
    val essentSim : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile(os.pwd / "examples" / "ReadWriteSyncMem.java"))
    val treadleSim = TreadleTester(Seq(FirrtlFileAnnotation(System.getProperty("user.dir") + "/examples/ReadWriteSyncMem.fir")))
    val dut = new DeltaTester(treadleSim, essentSim, Seq("dataOut"))

    dut.poke("enable", 0)
    for (i <- 0 until 1024) {
      dut.poke("addr", i)
      dut.poke("dataIn", i)
      dut.step(true)
    }
    dut.poke("enable", 1)
    dut.step(true)

    for (i <- 0 until 1024) {
      dut.poke("addr", i)
      dut.step(true)
    }
  }
}