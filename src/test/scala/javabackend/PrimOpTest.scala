package javabackend

import essent.{IsSimulator, SimulatorWrapper}
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

class PrimOpTest extends AnyFreeSpec {
  private val numInputs = 50
  private val rand = new scala.util.Random(0)

  private def runTest(
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
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTest(dut, testValues)
    }
  }

  "addw" in {}

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
          |    node _io_UOut_T = sub(io.UArg1, io.UArg2) @[main.scala 17:23]
          |    node _io_UOut_T_1 = tail(_io_UOut_T, 1) @[main.scala 17:23]
          |    io.UOut <= _io_UOut_T_1 @[main.scala 17:11]
          |    node _io_SOut_T = sub(io.SArg1, io.SArg2) @[main.scala 18:23]
          |    node _io_SOut_T_1 = tail(_io_SOut_T, 1) @[main.scala 18:23]
          |    node _io_SOut_T_2 = asSInt(_io_SOut_T_1) @[main.scala 18:23]
          |    io.SOut <= _io_SOut_T_2 @[main.scala 18:11]"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTest(dut, testValues)
    }
  }

  "subw" in {}

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
          |    node _io_UOut_T = mul(io.UArg1, io.UArg2) @[main.scala 17:23]
          |    io.UOut <= _io_UOut_T @[main.scala 17:11]
          |    node _io_SOut_T = mul(io.SArg1, io.SArg2) @[main.scala 18:23]
          |    io.SOut <= _io_SOut_T @[main.scala 18:11]"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTest(dut, testValues)
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
          |    node _io_UOut_T = div(io.UArg1, io.UArg2) @[main.scala 17:23]
          |    io.UOut <= _io_UOut_T @[main.scala 17:11]
          |    node _io_SOut_T = div(io.SArg1, io.SArg2) @[main.scala 18:23]
          |    io.SOut <= _io_SOut_T @[main.scala 18:11]"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTest(dut, testValues)
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
          |    node _io_UOut_T = rem(io.UArg1, io.UArg2) @[main.scala 17:23]
          |    io.UOut <= _io_UOut_T @[main.scala 17:11]
          |    node _io_SOut_T = rem(io.SArg1, io.SArg2) @[main.scala 18:23]
          |    io.SOut <= _io_SOut_T @[main.scala 18:11]"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTest(dut, testValues)
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
          |    node _io_UOut_T = lt(io.UArg1, io.UArg2) @[main.scala 17:23]
          |    io.UOut <= _io_UOut_T @[main.scala 17:11]
          |    node _io_SOut_T = lt(io.SArg1, io.SArg2) @[main.scala 18:23]
          |    io.SOut <= _io_SOut_T @[main.scala 18:11]"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTest(dut, testValues)
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
          |    node _io_UOut_T = leq(io.UArg1, io.UArg2) @[main.scala 17:23]
          |    io.UOut <= _io_UOut_T @[main.scala 17:11]
          |    node _io_SOut_T = leq(io.SArg1, io.SArg2) @[main.scala 18:23]
          |    io.SOut <= _io_SOut_T @[main.scala 18:11]"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTest(dut, testValues)
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
          |    node _io_UOut_T = gt(io.UArg1, io.UArg2) @[main.scala 17:23]
          |    io.UOut <= _io_UOut_T @[main.scala 17:11]
          |    node _io_SOut_T = gt(io.SArg1, io.SArg2) @[main.scala 18:23]
          |    io.SOut <= _io_SOut_T @[main.scala 18:11]"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTest(dut, testValues)
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
          |    node _io_UOut_T = geq(io.UArg1, io.UArg2) @[main.scala 17:23]
          |    io.UOut <= _io_UOut_T @[main.scala 17:11]
          |    node _io_SOut_T = geq(io.SArg1, io.SArg2) @[main.scala 18:23]
          |    io.SOut <= _io_SOut_T @[main.scala 18:11]"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTest(dut, testValues)
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
          |    node _io_UOut_T = eq(io.UArg1, io.UArg2) @[main.scala 17:23]
          |    io.UOut <= _io_UOut_T @[main.scala 17:11]
          |    node _io_SOut_T = eq(io.SArg1, io.SArg2) @[main.scala 18:23]
          |    io.SOut <= _io_SOut_T @[main.scala 18:11]"""
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (BigInt(w, rand), BigInt(w, rand))
      runTest(dut, testValues)
    }
  }

  "neq" in {}

  "tail" in {
    for (w <- List(1, 2, 32, 33, 63, 64, 65, 70)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<$w>, SOut : SInt<$w>}
           |
           |    node _io_UOut_T = tail(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = tail(io.SArg1, io.SArg2)
           |    node _io_SOut_T_1 = asSInt(_io_SOut_T_1)
           |    io.SOut <= _io_SOut_T_1
           |""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for { _ <- 1 to numInputs } yield (
          BigInt(w, rand),
          BigInt(rand.nextInt(w + 1))
        )
      runTest(dut, testValues)
    }
  }
}
