import essent.Driver
import org.scalatest.freespec.AnyFreeSpec

// sbt "testOnly *GCDTest"
class GCDTest extends AnyFreeSpec{
  // sbt "testOnly *GCDTest -- -z testZero"
  "testZero" in {
    Driver.main(Array("-O0", "-java", "/Users/haku/essent/examples/GCD.fir"))
    val sim : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile("/Users/haku/essent/examples/GCD.java"))
    sim.poke("io_a", 6)
    sim.poke("io_b", 9)
    sim.poke("io_e", 1)
    sim.step()
    sim.poke("io_e", 0)
    for (i <- 0 to 4) {
      println(s"i is ${i}")
      println(s"x is ${sim.peek("x")}, y is ${sim.peek("y")}")
      println(s"io_v is ${sim.peek("io_v")}")
      sim.step()
    }
  }
}
