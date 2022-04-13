package javabackend

import essent.{IsSimulator, SimulatorWrapper}
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

class PrimOpTest extends AnyFreeSpec {
  private val numInputs = 50
  private val rand = new scala.util.Random(0)

  private def runTestUnary(dut: IsSimulator, testValues: Iterable[BigInt]) : Unit = {
    for (i <- testValues) {
      dut.poke("io_UArg", i)
      dut.poke("io_SArg", i)
      dut.step(true)
    }
  }

  private def runTestBinary(
      dut: IsSimulator,
      testValues: Iterable[(BigInt, BigInt)]
  ): Unit = {
    for ((i, j) <- testValues) {
      dut.poke("io_UArg1", i)
      dut.poke("io_UArg2", j)
      dut.poke("io_SArg1", i)
      dut.poke("io_SArg2", j)
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
           |    node _io_UOut_T = add(io.UArg1, io.UArg2)
           |    node _io_UOut_T_1 = tail(_io_UOut_T, 1)
           |    io.UOut <= _io_UOut_T_1
           |    node _io_SOut_T = add(io.SArg1, io.SArg2)
           |    node _io_SOut_T_1 = tail(_io_SOut_T, 1)
           |    node _io_SOut_T_2 = asSInt(_io_SOut_T_1)
           |    io.SOut <= _io_SOut_T_2
           |""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTestBinary(dut, testValues)
    }
  }

  "sub" in {
    for (w <- List(1, 2, 32, 33, 63, 64, 65, 70)) {
      val source =
        s"""
          |circuit PrimOpTester :
          |  module PrimOpTester :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<$w>, SOut : SInt<$w>}
          |
          |    node _io_UOut_T = sub(io.UArg1, io.UArg2)
          |    node _io_UOut_T_1 = tail(_io_UOut_T, 1)
          |    io.UOut <= _io_UOut_T_1
          |    node _io_SOut_T = sub(io.SArg1, io.SArg2)
          |    node _io_SOut_T_1 = tail(_io_SOut_T, 1)
          |    node _io_SOut_T_2 = asSInt(_io_SOut_T_1)
          |    io.SOut <= _io_SOut_T_2"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTestBinary(dut, testValues)
    }
  }

  "mul" in {
    for (w <- List(1, 2, 32, 33, 63, 64, 65, 70)) {
      val source =
        s"""
          |circuit PrimOpTester :
          |  module PrimOpTester :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<$w>, SOut : SInt<$w>}
          |
          |    node _io_UOut_T = mul(io.UArg1, io.UArg2)
          |    io.UOut <= _io_UOut_T
          |    node _io_SOut_T = mul(io.SArg1, io.SArg2)
          |    io.SOut <= _io_SOut_T"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTestBinary(dut, testValues)
    }
  }

  "div" in {
    for (w <- List(1, 2, 32, 33, 63, 64, 65, 70)) {
      val source =
        s"""
          |circuit PrimOpTester :
          |  module PrimOpTester :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<$w>, SOut : SInt<$w>}
          |
          |    node _io_UOut_T = div(io.UArg1, io.UArg2)
          |    io.UOut <= _io_UOut_T
          |    node _io_SOut_T = div(io.SArg1, io.SArg2)
          |    io.SOut <= _io_SOut_T"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTestBinary(dut, testValues)
    }
  }

  "rem" in {
    for (w <- List(1, 2, 32, 33, 63, 64, 65, 70)) {
      val source =
        s"""
          |circuit PrimOpTester :
          |  module PrimOpTester :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<$w>, SOut : SInt<$w>}
          |
          |    node _io_UOut_T = rem(io.UArg1, io.UArg2)
          |    io.UOut <= _io_UOut_T
          |    node _io_SOut_T = rem(io.SArg1, io.SArg2)
          |    io.SOut <= _io_SOut_T"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTestBinary(dut, testValues)
    }
  }

  "lt" in {
    for (w <- List(1, 2, 32, 33, 63, 64, 65, 70)) {
      val source =
        s"""
          |circuit PrimOpTester :
          |  module PrimOpTester :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<1>, SOut : UInt<1>}
          |
          |    node _io_UOut_T = lt(io.UArg1, io.UArg2)
          |    io.UOut <= _io_UOut_T
          |    node _io_SOut_T = lt(io.SArg1, io.SArg2)
          |    io.SOut <= _io_SOut_T"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTestBinary(dut, testValues)
    }
  }

  "leq" in {
    for (w <- List(1, 2, 32, 33, 63, 64, 65, 70)) {
      val source =
        s"""
          |circuit PrimOpTester :
          |  module PrimOpTester :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<1>, SOut : UInt<1>}
          |
          |    node _io_UOut_T = leq(io.UArg1, io.UArg2)
          |    io.UOut <= _io_UOut_T
          |    node _io_SOut_T = leq(io.SArg1, io.SArg2)
          |    io.SOut <= _io_SOut_T"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTestBinary(dut, testValues)
    }
  }

  "gt" in {
    for (w <- List(1, 2, 32, 33, 63, 64, 65, 70)) {
      val source =
        s"""
          |circuit PrimOpTester :
          |  module PrimOpTester :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<1>, SOut : UInt<1>}
          |
          |    node _io_UOut_T = gt(io.UArg1, io.UArg2)
          |    io.UOut <= _io_UOut_T
          |    node _io_SOut_T = gt(io.SArg1, io.SArg2)
          |    io.SOut <= _io_SOut_T"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTestBinary(dut, testValues)
    }
  }

  "geq" in {
    for (w <- List(1, 2, 32, 33, 63, 64, 65, 70)) {
      val source =
        s"""
          |circuit PrimOpTester :
          |  module PrimOpTester :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<1>, SOut : UInt<1>}
          |
          |    node _io_UOut_T = geq(io.UArg1, io.UArg2)
          |    io.UOut <= _io_UOut_T
          |    node _io_SOut_T = geq(io.SArg1, io.SArg2)
          |    io.SOut <= _io_SOut_T"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTestBinary(dut, testValues)
    }
  }

  "eq" in {
    for (w <- List(1, 2, 32, 33, 63, 64, 65, 70)) {
      val source =
        s"""
          |circuit PrimOpTester :
          |  module PrimOpTester :
          |    input clock : Clock
          |    input reset : UInt<1>
          |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<1>, SOut : UInt<1>}
          |
          |    node _io_UOut_T = eq(io.UArg1, io.UArg2)
          |    io.UOut <= _io_UOut_T
          |    node _io_SOut_T = eq(io.SArg1, io.SArg2)
          |    io.SOut <= _io_SOut_T"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTestBinary(dut, testValues)
    }
  }

  "pad" in {

  }

  "asUInt" in {

  }

  "asSInt" in {

  }

  "shl" in {

  }

  "shr" in {

  }

  "dshl" in {

  }

  "dshr" in {

  }

  "not" in {

  }

  "neg" in {

  }

  "andr" in {

  }

  "orr" in {

  }

  "xorr" in {

  }

  "cat" in {

  }

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
