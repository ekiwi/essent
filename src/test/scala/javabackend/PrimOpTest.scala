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

  private def generateUInts(w : Int) : Iterable[BigInt] = {
    for {_ <- 1 to numInputs} yield BigInt(w, rand)
  }

  private def generateSInts(w : Int) : Iterable[BigInt] = {
    val bias = BigInt(1) << (w - 1)
    for {_ <- 1 to numInputs} yield BigInt(w, rand) - bias
  }

  private def generateUIntTuples(w : Int, v : Int) : Iterable[(BigInt, BigInt)] = {
    for {_ <- 1 to numInputs} yield (BigInt(w, rand), BigInt(v, rand))
  }

  private def generateSIntTuples(w : Int, v: Int) : Iterable[(BigInt, BigInt)] = {
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
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
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
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
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
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
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
      val UIntValues = generateUIntTuples(w, v).filter(_._2 != 0)
      val SIntValues = generateSIntTuples(w, v).filter(_._2 != 0)
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
      val UIntValues = generateUIntTuples(w, v).filter(_._2 != 0)
      val SIntValues = generateSIntTuples(w, v).filter(_._2 != 0)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "lt" in {
    for (w <- List(1, 2, 32, 63, 64); v <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$v>, UOut : UInt<1>, SOut : UInt<1>}
           |
           |    node _io_UOut_T = lt(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = lt(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut", "io_SArg1", "io_SArg2"))
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
      runTestBinary(dut, UIntValues, SIntValues)

    }
  }

  "leq" in {
    for (w <- List(1, 2, 32, 63, 64); v <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$v>, UOut : UInt<1>, SOut : UInt<1>}
           |
           |    node _io_UOut_T = leq(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = leq(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut", "io_SArg1", "io_SArg2"))
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "gt" in {
    for (w <- List(1, 2, 32, 63, 64); v <- List(1, 2, 32, 63, 64)) {
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
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut", "io_SArg1", "io_SArg2"))
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "geq" in {
    for (w <- List(1, 2, 32, 63, 64); v <- List(1, 2, 32, 63, 64)) {
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
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut", "io_SArg1", "io_SArg2"))
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "eq" in {
    for (w <- List(1, 2, 32, 63, 64); v <- List(1, 2, 32, 63, 64)) {
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
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut", "io_SArg1", "io_SArg2"))
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "neq" in {
    for (w <- List(1, 2, 32, 63, 64); v <- List(1, 2, 32, 63, 64)) {
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
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut", "io_SArg1", "io_SArg2"))
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "pad" in {
    for (w <- List(1, 32, 63, 64, 65); n <- List(1, 2, 32, 63, 64, 65, 70)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<${scala.math.max(w, n)}>, SOut : SInt<${scala.math.max(w, n)}>}
           |
           |    node _io_UOut_T = pad(io.UArg, $n)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = pad(io.SArg, $n)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "asUInt" in {
    for (w <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<$w>, SOut : UInt<$w>}
           |
           |    node _io_UOut_T = asUInt(io.UArg)
           |    node _io_SOut_T = asUInt(io.SArg)
           |    io.UOut <= _io_UOut_T
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "asSInt" in {
    for (w <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : SInt<$w>, SOut : SInt<$w>}
           |
           |    node _io_UOut_T = asSInt(io.UArg)
           |    node _io_SOut_T = asSInt(io.SArg)
           |    io.UOut <= _io_UOut_T
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "shl" in {
    for (w <- List(1, 32, 63, 64, 65); n <- List(0, 1, 2, 4, 8, 16, 32)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<${w+n}>, SOut : SInt<${w+n}>}
           |
           |    node _io_UOut_T = shl(io.UArg, $n)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = shl(io.SArg, $n)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "shr" in {
    for (w <- List(1, 16, 17, 32, 63, 64); n <- List(0, 1, 2, 4, 8, 16, 32)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<${scala.math.max(w-n, 1)}>, SOut : SInt<${scala.math.max(w-n, 1)}>}
           |
           |    node _io_UOut_T = shr(io.UArg, $n)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = shr(io.SArg, $n)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "dshl" in {
    for (w <- List(1, 2, 32, 63, 64); v <- List(1, 2, 3, 4, 5)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : UInt<$v>, UOut : UInt<${w+(1<<v)-1}>, SOut : SInt<${w+(1<<v)-1}> }
           |
           |    node _io_UOut_T = dshl(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = dshl(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut", "io_SArg1", "io_SArg2"))
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = for {_ <- 1 to numInputs} yield (BigInt(w, rand) - (BigInt(1) << (w - 1)), BigInt(v, rand))
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "dshr" in {
    for (w <- List(1, 2, 32, 63, 64); v <- List(1, 2, 3, 4, 5, 6)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : UInt<$v>, UOut : UInt<$w>, SOut : SInt<$w> }
           |
           |    node _io_UOut_T = dshr(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = dshr(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = for {_ <- 1 to numInputs} yield (BigInt(w, rand) - (BigInt(1) << (w - 1)), BigInt(v, rand))
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "cvt" in {
    for (w <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : SInt<${w+1}>, SOut : SInt<$w>}
           |
           |    node _io_UOut_T = cvt(io.UArg)
           |    node _io_SOut_T = cvt(io.SArg)
           |    io.UOut <= _io_UOut_T
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "neg" in {
    for (w <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : SInt<${w+1}>, SOut : SInt<${w+1}>}
           |
           |    node _io_UOut_T = neg(io.UArg)
           |    node _io_SOut_T = neg(io.SArg)
           |    io.UOut <= _io_UOut_T
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "not" in {
    for (w <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<$w>, SOut : UInt<$w>}
           |
           |    node _io_UOut_T = not(io.UArg)
           |    node _io_SOut_T = not(io.SArg)
           |    io.UOut <= _io_UOut_T
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "and" in {
    for (w <- List(1, 2, 32, 63, 64); v <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$v>, UOut : UInt<${scala.math.max(w, v)}>, SOut : UInt<${scala.math.max(w, v)}> }
           |
           |    node _io_UOut_T = and(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = and(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "or" in {
    for (w <- List(1, 2, 32, 63, 64); v <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$v>, UOut : UInt<${scala.math.max(w, v)}>, SOut : UInt<${scala.math.max(w, v)}> }
           |
           |    node _io_UOut_T = or(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = or(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "xor" in {
    for (w <- List(1, 2, 32, 63, 64); v <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$v>, UOut : UInt<${scala.math.max(w, v)}>, SOut : UInt<${scala.math.max(w, v)}> }
           |
           |    node _io_UOut_T = xor(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = xor(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "andr" in {
    for (w <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<1>, SOut : UInt<1>}
           |
           |    node _io_UOut_T = andr(io.UArg)
           |    node _io_SOut_T = andr(io.SArg)
           |    io.UOut <= _io_UOut_T
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }


  "orr" in {
    for (w <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<1>, SOut : UInt<1>}
           |
           |    node _io_UOut_T = orr(io.UArg)
           |    node _io_SOut_T = orr(io.SArg)
           |    io.UOut <= _io_UOut_T
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "xorr" in {
    for (w <- List(1, 2, 32, 63, 64)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<1>, SOut : UInt<1>}
           |
           |    node _io_UOut_T = xorr(io.UArg)
           |    node _io_SOut_T = xorr(io.SArg)
           |    io.UOut <= _io_UOut_T
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "cat" in {
    for (w <- List(1, 4, 16, 32, 33); v <- List(1, 2, 31, 32, 33)) {
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg1 : UInt<$w>, flip UArg2 : UInt<$v>, flip SArg1 : SInt<$w>, flip SArg2 : SInt<$v>, UOut : UInt<${w+v}>, SOut : UInt<${w+v}> }
           |
           |    node _io_UOut_T = cat(io.UArg1, io.UArg2)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = cat(io.SArg1, io.SArg2)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUIntTuples(w, v)
      val SIntValues = generateSIntTuples(w, v)
      runTestBinary(dut, UIntValues, SIntValues)
    }
  }

  "bits" in {
    val l = List((1, 0, 0), (2, 1, 0), (3, 2, 2), (16, 15, 5), (63, 60, 0), (70, 68, 1))
    for (tup <- l) {
      val w = tup._1
      val hi = tup._2
      val lo = tup._3
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<${hi - lo + 1}>, SOut : UInt<${hi - lo + 1}> }
           |
           |    node _io_UOut_T = bits(io.UArg, $hi, $lo)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = bits(io.SArg, $hi, $lo)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "head" in {
    val l = List((1, 1), (2, 1), (3, 3), (16, 8), (65, 60), (70, 68))
    for (tup <- l) {
      val w = tup._1
      val n = tup._2
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<$n>, SOut : UInt<$n>}
           |
           |    node _io_UOut_T = head(io.UArg, $n)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = head(io.SArg, $n)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }

  "tail" in {
    val l = List((1, 0), (2, 1), (3, 0), (16, 8), (65, 60), (70, 68))
    for (tup <- l) {
      val w = tup._1
      val n = tup._2
      val source =
        s"""
           |circuit PrimOpTester :
           |  module PrimOpTester :
           |    input clock : Clock
           |    input reset : UInt<1>
           |    output io : { flip UArg : UInt<$w>, flip SArg : SInt<$w>, UOut : UInt<${w-n}>, SOut : UInt<${w-n}>}
           |
           |    node _io_UOut_T = tail(io.UArg, $n)
           |    io.UOut <= _io_UOut_T
           |    node _io_SOut_T = tail(io.SArg, $n)
           |    io.SOut <= _io_SOut_T""".stripMargin
      val essentSim = SimulatorWrapper(source)
      val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(source)))
      val dut = new DeltaTester(treadleSim, essentSim, Seq("io_UOut", "io_SOut"))
      val UIntValues = generateUInts(w)
      val SIntValues = generateSInts(w)
      runTestUnary(dut, UIntValues, SIntValues)
    }
  }
}
