package javabackend

import essent.SimulatorWrapper
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

class MemoryTest extends AnyFreeSpec{
  private val source =
    """
      |circuit ReadWriteSyncMem :
      |  module ReadWriteSyncMem :
      |    input clock : Clock
      |    input reset : UInt<1>
      |    output io : { flip enable : UInt<1>, flip write : UInt<1>, flip addr : UInt<10>, flip dataIn : UInt<32>, dataOut : UInt<32>}
      |
      |    smem mem : UInt<32> [1024] @[main.scala 16:24]
      |    write mport MPORT = mem[io.addr], clock
      |    MPORT <= io.dataIn
      |    wire _io_dataOut_WIRE : UInt @[main.scala 19:25]
      |    _io_dataOut_WIRE is invalid @[main.scala 19:25]
      |    when io.enable : @[main.scala 19:25]
      |      _io_dataOut_WIRE <= io.addr @[main.scala 19:25]
      |      node _io_dataOut_T = or(_io_dataOut_WIRE, UInt<10>("h0")) @[main.scala 19:25]
      |      node _io_dataOut_T_1 = bits(_io_dataOut_T, 9, 0) @[main.scala 19:25]
      |      read mport io_dataOut_MPORT = mem[_io_dataOut_T_1], clock @[main.scala 19:25]
      |    io.dataOut <= io_dataOut_MPORT @[main.scala 19:14]
      |""".stripMargin

  "ReadWriteMemory" in {
    val essentSim = SimulatorWrapper(source)
    val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
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