import essent.Driver
import org.scalatest.freespec.AnyFreeSpec

class DecoupledTest extends AnyFreeSpec {
  private def computeGcd(a: BigInt, b: BigInt): BigInt = a.gcd(b)

  private def runTest(dut: SimulatorWrapper, testValues: Iterable[(BigInt, BigInt, BigInt)]): Long = {
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

  "decoupled" in {
    Driver.main(Array("-O0", "-java", System.getProperty("user.dir") + "/examples/DecoupledGCD.fir"))
    val dut : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile(System.getProperty("user.dir") + "/examples/DecoupledGCD.java"))

    val repetitions = 6
    val numMax = 200
    val testValues = for {x <- 2 to numMax; y <- 2 to numMax} yield (BigInt(x), BigInt(y), computeGcd(x, y))
    var cycles = 0L
    val startTime = System.nanoTime
    (0 until repetitions).foreach { _ =>
      cycles += runTest(dut, testValues)
    }
    val endTime = System.nanoTime
    println(s"${(endTime - startTime)/1000000} milliseconds")
    println(s"$cycles cycles")
  }
}
