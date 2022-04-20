package javabackend

import essent.{IsSimulator, SimulatorWrapper}
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

class PrimOpTest extends AnyFreeSpec {
  private val numInputs = 50
  private val rand = new scala.util.Random(0)

  private def runTestUnary(dut: IsSimulator,
                            UIntValues: Iterable[BigInt],
                            SIntValues: Iterable[BigInt]
                          ): Unit = {
    for (i <- UIntValues; j <- SIntValues) {
      dut.poke("io_UArg", i)
      dut.poke("io_SArg", j)
      dut.step(true)
    }
  }

  private def runTestBinary(dut: IsSimulator,
                             UIntValues: Iterable[(BigInt, BigInt)],
                             SIntValues: Iterable[(BigInt, BigInt)]
                           ): Unit = {
    for ((i, j) <- UIntValues; (k, l) <- SIntValues) {
      dut.poke("io_UArg1", i)
      dut.poke("io_UArg2", j)
      dut.poke("io_SArg1", k)
      dut.poke("io_SArg2", l)
      dut.step(true)
    }
  }

  private def UIntGenerator(w : Int) : Iterable[BigInt] = {
    for {_ <- 1 to numInputs} yield BigInt(w, rand)
  }

  private def SIntGenerator(w : Int) : Iterable[BigInt] = {
    val bias = BigInt(1) << (w - 1)
    for {_ <- 1 to numInputs} yield BigInt(w, rand) - bias
  }

  private def UIntTupleGenerator(w : Int, v : Int) : Iterable[(BigInt, BigInt)] = {
    for {_ <- 1 to numInputs} yield (BigInt(w, rand), BigInt(v, rand))
  }

  private def SIntTupleGenerator(w : Int, v: Int) : Iterable[(BigInt, BigInt)] = {
    val wBias = BigInt(1) << (w - 1)
    val vBias = BigInt(1) << (v - 1)
    for {_ <- 1 to numInputs} yield (BigInt(w, rand) - wBias, BigInt(v, rand) - vBias)
  }

  "add" in {
    for (w <- List(1, 2, 63, 64); v <- List(1, 2,63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$v>, UOut : UInt<${scala.math.max(w, v) + 1}>, SOut : SInt<${scala.math.max(w, v) + 1}> }
           |
           |    node _io_UOut_T = add(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = add(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = UIntTupleGenerator(w, v)
      val SIntValues = SIntTupleGenerator(w, v)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "sub" in {
    for (w <- List(1, 2, 63, 64); v <- List(1, 2, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$v>, UOut : UInt<${scala.math.max(w, v) + 1}>, SOut : SInt<${scala.math.max(w, v) + 1}> }
           |
           |    node _io_UOut_T = sub(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = sub(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = UIntTupleGenerator(w, v)
      val SIntValues = SIntTupleGenerator(w, v)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "mul" in {
    for (w <- List(1, 2, 32, 63, 64); v <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$v>, UOut : UInt<${w+v}>, SOut : SInt<${w+v}> }
           |
           |    node _io_UOut_T = mul(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = mul(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut", "io_SArg1", "io_SArg2"))
      val UIntValues = UIntTupleGenerator(w, v)
      val SIntValues = SIntTupleGenerator(w, v)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "div" in {
    for (w <- List(1, 2, 32, 63, 64, 65); v <- List(1, 2, 32, 63, 64, 65)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$v>, UOut : UInt<$w>, SOut : SInt<${w + 1}> }
           |
           |    node _io_UOut_T = div(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = div(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = UIntTupleGenerator(w, v).filter(_._2 != 0)
      val SIntValues = SIntTupleGenerator(w, v).filter(_._2 != 0)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "rem" in {
    for (w <- List(1, 2, 32, 63, 64, 65); v <- List(1, 2, 32, 63, 64, 65)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$v>, UOut : UInt<${scala.math.min(w, v)}>, SOut : SInt<${scala.math.min(w, v)}> }
           |
           |    node _io_UOut_T = rem(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = rem(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = UIntTupleGenerator(w, v).filter(_._2 != 0)
      val SIntValues = SIntTupleGenerator(w, v).filter(_._2 != 0)
      runTestBinary(dut, UIntValues, SIntValues)
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
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for {_ <- 1 to numInputs} yield (BigInt(w, rand), BigInt(w, rand))
      //runTestBinary(dut, )
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
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for {_ <- 1 to numInputs} yield (BigInt(w, rand), BigInt(w, rand))
      //runTestBinary(dut, )
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
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for {_ <- 1 to numInputs} yield (BigInt(w, rand), BigInt(w, rand))
      //runTestBinary(dut, )
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
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for {_ <- 1 to numInputs} yield (BigInt(w, rand), BigInt(w, rand))
      //runTestBinary(dut, )
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
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for {_ <- 1 to numInputs} yield (BigInt(w, rand), BigInt(w, rand))
      //runTestBinary(dut, )
    }
  }

  "neq" in {
    for (w <- List(1, 2, 32, 33, 63, 64, 65, 70)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$w>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$w>, UOut : UInt<1>, SOut : UInt<1>}
           |
           |    node _io_UOut_T = neq(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = neq(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for {_ <- 1 to numInputs} yield (BigInt(w, rand), BigInt(w, rand))
      //runTestBinary(dut, )
    }
  }

  "pad" in {}

  "asUInt" in {
    for (w <- List(1, 2, 32, 33, 63)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<$w>, SOut : UInt<$w>}
           |
           |    io.UOut <= io.UArg @[main.scala 15:11]
           |    node _io_SOut_T = asUInt(io.SArg) @[main.scala 16:29]
           |    io.SOut <= _io_SOut_T @[main.scala 16:11]""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for {_ <- 1 to numInputs} yield BigInt(w, rand)
      //runTestUnary(dut, testValues)
    }
  }

  "asSInt" in {
    for (w <- List(2, 32, 33, 63)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : SInt<$w>, SOut : SInt<$w>}
           |
           |    node _io_UOut_T = asSInt(io.UArg) @[main.scala 15:28]
           |    io.UOut <= _io_UOut_T @[main.scala 15:11]
           |    io.SOut <= io.SArg @[main.scala 16:11]""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = UIntGenerator(w)
      val SIntValues = SIntGenerator(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "shl" in {}

  "shr" in {}

  "dshl" in {}

  "dshr" in {}

  "cvt" in {}

  "neg" in {}

  "not" in {}

  "and" in {}

  "or" in {}

  "xor" in {}

  "andr" in {
    for (w <- List(2, 10, 16)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<$w>, SOut : UInt<$w>}
           |
           |    node _io_UOut_T = andr(io.UArg) @[main.scala 15:22]
           |    io.UOut <= _io_UOut_T @[main.scala 15:11]
           |    node _io_SOut_T = asUInt(io.SArg) @[main.scala 16:28]
           |    node _io_SOut_T_1 = andr(_io_SOut_T) @[main.scala 16:31]
           |    io.SOut <= _io_SOut_T_1 @[main.scala 16:11]
           |""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for {_ <- 1 to numInputs} yield BigInt(w, rand)
      //runTestUnary(dut, testValues)
    }
  }


  "orr" in {
    for (w <- List(2, 10, 16)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<$w>, SOut : UInt<$w>}
           |
           |    node _io_UOut_T = orr(io.UArg) @[main.scala 15:22]
           |    io.UOut <= _io_UOut_T @[main.scala 15:11]
           |    node _io_SOut_T = asUInt(io.SArg) @[main.scala 16:28]
           |    node _io_SOut_T_1 = orr(_io_SOut_T) @[main.scala 16:31]
           |    io.SOut <= _io_SOut_T_1 @[main.scala 16:11]""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for {_ <- 1 to numInputs} yield BigInt(w, rand)
      //runTestUnary(dut, testValues)
    }
  }

  "xorr" in {
    for (w <- List(2, 10, 16)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<$w>, SOut : UInt<$w>}
           |
           |    node _io_UOut_T = xorr(io.UArg) @[main.scala 15:22]
           |    io.UOut <= _io_UOut_T @[main.scala 15:11]
           |    node _io_SOut_T = asUInt(io.SArg) @[main.scala 16:28]
           |    node _io_SOut_T_1 = xorr(_io_SOut_T) @[main.scala 16:31]
           |    io.SOut <= _io_SOut_T_1 @[main.scala 16:11]""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut =
        new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val testValues =
        for {_ <- 1 to numInputs} yield BigInt(w, rand)
      //runTestUnary(dut, testValues)
    }
  }

  "cat" in {}

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
    val testValues = for { _ <- 1 to numInputs } yield BigInt(64, rand)
    //runTestUnary(dut, testValues)
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
    val testValues = for { _ <- 1 to numInputs } yield BigInt(64, rand)
    //runTestUnary(dut, testValues)
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
    val testValues = for { _ <- 1 to numInputs } yield BigInt(64, rand)
    //runTestUnary(dut, testValues)
  }
}
