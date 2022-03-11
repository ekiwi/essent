import chiseltest.simulator.SimulatorContext
import essent.Driver
import firrtl.stage.FirrtlFileAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.{TreadleTester, WriteVcdAnnotation}

class DecoupledTest extends AnyFreeSpec {
  private def computeGcd(a: BigInt, b: BigInt): BigInt = a.gcd(b)

  private def runEssent(dut: SimulatorWrapper, testValues: Iterable[(BigInt, BigInt, BigInt)]): Long = {
    var cycles = 0L
    dut.poke("reset", 1)
    dut.step(true)
    dut.step(true)
    cycles += 2
    dut.poke("reset", 0)

    dut.poke("output_ready", 1)
    for((i, j, expected) <- testValues) {
      dut.poke("input_bits_value1", i)
      dut.poke("input_bits_value2", j)
      dut.poke("input_valid", 1)
      dut.step(true)
      cycles += 1

      while(dut.peek("output_valid") == 0) {
        dut.step(true)
        cycles += 1
      }
      assert(dut.peek("output_bits_gcd") == expected)
      assert(dut.peek("output_valid") == 1)
    }

    cycles
  }

  private def runTreadle(dut: TreadleTester, testValues: Iterable[(BigInt, BigInt, BigInt)]): Long = {
    var cycles = 0L
    dut.poke("reset", 1)
    dut.step(2)
    cycles += 2
    dut.poke("reset", 0)

    dut.poke("output_ready", 1)
    for((i, j, expected) <- testValues) {
      dut.poke("input_bits_value1", i)
      dut.poke("input_bits_value2", j)
      dut.poke("input_valid", 1)
      dut.step(1)
      cycles += 1

      while(dut.peek("output_valid") == 0) {
        dut.step(1)
        cycles += 1
      }
      assert(dut.peek("output_bits_gcd") == expected)
      assert(dut.peek("output_valid") == 1)
    }

    cycles
  }

  "decoupledEssent" in {
    val startTimeCompile = System.nanoTime
    Driver.main(Array("-O0", "-java", System.getProperty("user.dir") + "/examples/DecoupledGCD.fir"))
    val dut : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile(System.getProperty("user.dir") + "/examples/DecoupledGCD.java"))
    val endTimeCompile = System.nanoTime
    println(s"Essent Compilation Time: ${(endTimeCompile - startTimeCompile)/1000000} milliseconds")

    val repetitions = 6
    val numMax = 200
    val testValues = for {x <- 2 to numMax; y <- 2 to numMax} yield (BigInt(x), BigInt(y), computeGcd(x, y))
    var cycles = 0L

    val startTimeExecute = System.nanoTime
    (0 until repetitions).foreach { _ =>
      cycles += runEssent(dut, testValues)
    }
    val endTimeExecute = System.nanoTime
    println(s"Essent Execution Time: ${(endTimeExecute - startTimeExecute)/1000000} milliseconds")
    println(s"Essent Execution Cycles: $cycles cycles")
    println(s"Essent Execution Time per Cycle: ${(endTimeExecute - startTimeExecute)/cycles} nanoseconds/cycle")
    println(s"Essent Total Time: ${(endTimeExecute + endTimeCompile - startTimeExecute - startTimeCompile)/1000000} milliseconds")
    println
  }

  "decoupledTreadle" in {
    val startTimeCompile = System.nanoTime
    val dut = TreadleTester(Seq(FirrtlFileAnnotation(System.getProperty("user.dir") + "/examples/DecoupledGCD.fir")))
    val endTimeCompile = System.nanoTime
    println(s"Treadle Compilation Time: ${(endTimeCompile - startTimeCompile)/1000000} milliseconds")

    val repetitions = 6
    val numMax = 200
    val testValues = for {x <- 2 to numMax; y <- 2 to numMax} yield (BigInt(x), BigInt(y), computeGcd(x, y))
    var cycles = 0L

    val startTimeExecute = System.nanoTime
    (0 until repetitions).foreach { _ =>
      cycles += runTreadle(dut, testValues)
    }
    val endTimeExecute = System.nanoTime
    println(s"Treadle Execution Time: ${(endTimeExecute - startTimeExecute)/1000000} milliseconds")
    println(s"Treadle Execution Cycles: $cycles cycles")
    println(s"Treadle Execution Time per Cycle: ${(endTimeExecute - startTimeExecute)/cycles} nanoseconds/cycle")
    println(s"Treadle Total Time: ${(endTimeExecute + endTimeCompile - startTimeExecute - startTimeCompile)/1000000} milliseconds")
    println
    dut.finish
  }
}
