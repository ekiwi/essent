import essent.Driver
import org.scalatest.freespec.AnyFreeSpec

// sbt "testOnly *GCDTest"
class GCDTest extends AnyFreeSpec{
  // sbt "testOnly *GCDTest -- -z testZero"
  def testBehavior(sim : SimulatorWrapper, a : Int, b : Int, gcd : Int) : Unit = {
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
    val sim : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile(System.getProperty("user.dir") + "/examples/GCD.java"))
    testBehavior(sim, 9, 6, 3)
    testBehavior(sim, 4, 12, 4)
    testBehavior(sim, 771, 880, 1)
    testBehavior(sim, 212, 756, 4)
    testBehavior(sim, 1, 1, 1)
    testBehavior(sim, 1, 80, 1)
    testBehavior(sim, 50, 1, 1)
    testBehavior(sim, 11, 11, 11)
  }

  "decoupled" in {
    Driver.main(Array("-O0", "-java", System.getProperty("user.dir") + "/examples/DecoupledGCD.fir"))
    val sim : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile(System.getProperty("user.dir") + "/examples/DecoupledGCD.java"))
    sim.step(false)
    sim.poke("input_valid", 1)
    sim.poke("input_bits_value1", 4)
    sim.poke("input_bits_value1", 12)
    sim.step(true)
    sim.poke("input_valid", 0)
    sim.step(false)
    sim.poke("output_ready", 1)
    while (sim.peek("output_valid") == 0) {
      sim.step(true)
    }
    println(sim.peek("output_bits_value1"))
    println(sim.peek("output_bits_value2"))
  }
}
