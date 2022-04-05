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
    val dut = new DeltaTester(treadleSim, essentSim, Seq("io_dataOut"))

    dut.poke("io_enable", 0)
    for (i <- 0 until 1024) {
      dut.poke("io_addr", i)
      dut.poke("io_dataIn", i)
      dut.step(true)
    }
    dut.poke("io_enable", 1)
    dut.step(true)

    for (i <- 0 until 1024) {
      dut.poke("io_addr", i)
      dut.step(true)
    }
  }
}