package javabackend

import essent.{IsSimulator, SimulatorWrapper}
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

class PrimOpTest extends AnyFreeSpec{
  private val numInputs = 50
  private val rand = new scala.util.Random(0)

  private def runTest(dut: IsSimulator, testValues: Iterable[(BigInt, BigInt)]): Unit = {
    for ((i, j) <- testValues) {
      dut.poke("io_UArg1", i)
      dut.poke("io_UArg2", j)
      dut.poke("io_SArg1", i)
      dut.poke("io_SArg2", j)
      dut.step(true)
    }
  }

  private def runTestUnary(dut: IsSimulator, testValues: Iterable[BigInt]) : Unit = {
    for (i <- testValues) {
      dut.poke("io_UArg", i)
      dut.poke("io_SArg", i)
      dut.step(true)
    }
  }

  "add" in {
    for (w <- List(1, 2, 32, 33, 63, 64, 65, 70)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<$w>, SOut : SInt<$w>}
           |
           |    node _io_UOut_T = add(io.UArg1, io.UArg2) @[main.scala 17:23]
           |    node _io_UOut_T_1 = tail(_io_UOut_T, 1) @[main.scala 17:23]
           |    io.UOut <= _io_UOut_T_1 @[main.scala 17:11]
           |    node _io_SOut_T = add(io.SArg1, io.SArg2) @[main.scala 18:23]
           |    node _io_SOut_T_1 = tail(_io_SOut_T, 1) @[main.scala 18:23]
           |    node _io_SOut_T_2 = asSInt(_io_SOut_T_1) @[main.scala 18:23]
           |    io.SOut <= _io_SOut_T_2 @[main.scala 18:11]
           |""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues = for {_ <- 1 to numInputs} yield (BigInt(w, rand), BigInt(w, rand))
      runTest(dut, testValues)
    }
  }

  // wip
  "cat" in {
    for (w <- List(1, 5, 10, 20, 40, 60)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<${w+w}>, SOut : UInt<${w+w}>}
           |
           |    node _io_UOut_T = cat(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = cat(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T
           |""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues = for {_ <- 1 to numInputs} yield (BigInt(w, rand), BigInt(w, rand))
      runTest(dut, testValues)
    }
  }

  // wip
  "bits" in {
    val source =
      s"""
         |circuit PrimOpTester :
         |  module PrimOpTester :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    output io : { flip UArg : UInt<64>, flip SArg : SInt<64>, UOut : UInt<64>, SOut : SInt<64>}
         |
         |    node _io_UOut_T = io.UArg
         |    node _io_UOut_T_1 = bits(_io_UOut_T, 64)
         |    io.UOut <= _io_UOut_T_1
         |    node _io_SOut_T = io.SArg
         |    node _io_SOut_T_1 = head(_io_SOut_T, 64)
         |    node _io_SOut_T_2 = asSInt(_io_SOut_T_1)
         |    io.SOut <= _io_SOut_T_2
         |""".stripMargin
    val essentSim = SimulatorWrapper(source)
    val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
    val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
    val testValues = for {_ <- 1 to numInputs} yield BigInt(64, rand)
    runTestUnary(dut, testValues)
  }

  "head" in {
    val source =
      s"""
         |circuit PrimOpTester :
         |  module PrimOpTester :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    output io : { flip UArg : UInt<64>, flip SArg : SInt<64>, UOut : UInt<64>, SOut : SInt<64>}
         |
         |    node _io_UOut_T = io.UArg
         |    node _io_UOut_T_1 = head(_io_UOut_T, 64)
         |    io.UOut <= _io_UOut_T_1
         |    node _io_SOut_T = io.SArg
         |    node _io_SOut_T_1 = head(_io_SOut_T, 64)
         |    node _io_SOut_T_2 = asSInt(_io_SOut_T_1)
         |    io.SOut <= _io_SOut_T_2
         |""".stripMargin
    val essentSim = SimulatorWrapper(source)
    val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
    val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
    val testValues = for {_ <- 1 to numInputs} yield BigInt(64, rand)
    runTestUnary(dut, testValues)
  }

  "tail" in {
    val source =
      s"""
         |circuit PrimOpTester :
         |  module PrimOpTester :
         |    input clock : Clock
         |    input reset : UInt<1>
         |    output io : { flip UArg : UInt<64>, flip SArg : SInt<64>, UOut : UInt<64>, SOut : SInt<64>}
         |
         |    node _io_UOut_T = io.UArg
         |    node _io_UOut_T_1 = tail(_io_UOut_T, 0)
         |    io.UOut <= _io_UOut_T_1
         |    node _io_SOut_T = io.SArg
         |    node _io_SOut_T_1 = tail(_io_SOut_T, 0)
         |    node _io_SOut_T_2 = asSInt(_io_SOut_T_1)
         |    io.SOut <= _io_SOut_T_2
         |""".stripMargin
    val essentSim = SimulatorWrapper(source)
    val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
    val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
    val testValues = for {_ <- 1 to numInputs} yield BigInt(64, rand)
    runTestUnary(dut, testValues)
  }
}
