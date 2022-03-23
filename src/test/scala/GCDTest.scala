import essent.{Driver, IsSimulator, JavaRuntimeCompiler, SimulatorWrapper}
import firrtl.stage.FirrtlFileAnnotation
import javabackend.DeltaTester
import org.scalatest.freespec.AnyFreeSpec
import treadle.{TreadleTester, WriteVcdAnnotation}

// sbt "testOnly *GCDTest"
class GCDTest extends AnyFreeSpec{
  // sbt "testOnly *GCDTest -- -z testZero"
  def testBehavior(sim : IsSimulator, a : Int, b : Int, gcd : Int) : Unit = {
    sim.poke("io_e", 1)
    sim.poke("io_a", a)
    sim.poke("io_b", b)
    sim.step(true)
    sim.poke("io_e", 0)
    sim.step(false)
    while (sim.peek("io_v") == 0) {
      sim.step(true)
    }
    assert(sim.peek("x") == gcd)
    assert(sim.peek("y") == 0)
  }

  "smallNumbers" in {
    Driver.main(Array("-O0", "-java", System.getProperty("user.dir") + "/examples/GCD.fir"))
    val essentSim : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile(os.pwd / "examples" / "GCD.java"))
    val treadleSim = TreadleTester(Seq(FirrtlFileAnnotation(System.getProperty("user.dir") + "/examples/GCD.fir")))
    val sim = new DeltaTester(treadleSim, essentSim, Seq("x", "y"))
    testBehavior(sim, 9, 6, 3)
    testBehavior(sim, 4, 12, 4)
    testBehavior(sim, 771, 880, 1)
    testBehavior(sim, 212, 756, 4)
    testBehavior(sim, 1, 1, 1)
    testBehavior(sim, 1, 80, 1)
    testBehavior(sim, 50, 1, 1)
    testBehavior(sim, 11, 11, 11)
  }

  private def computeGcd(a: BigInt, b: BigInt): BigInt = a.gcd(b)

  private def runTest(dut: IsSimulator, testValues: Iterable[(BigInt, BigInt, BigInt)]): Long = {
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
    val essentSim : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile(os.pwd / "examples" / "DecoupledGCD.java"))
    val treadleSim = TreadleTester(Seq(FirrtlFileAnnotation(System.getProperty("user.dir") + "/examples/DecoupledGCD.fir")))
    val dut = new DeltaTester(treadleSim, essentSim, Seq("x", "y", "xInitial", "yInitial", "busy", "resultValid"))

    val repetitions = 1
    val numMax = 200
    val testValues = for {x <- 2 to numMax; y <- 2 to numMax} yield (BigInt(x), BigInt(y), computeGcd(x, y))
    var cycles = 0L
    val startTime = System.nanoTime
    (0 until repetitions).foreach { _ =>
      cycles += runTest(dut, testValues)
    }
    val endTime = System.nanoTime
    //println(s"${(endTime - startTime)/1000000} milliseconds")
    //println(s"$cycles cycles")
  }
}
