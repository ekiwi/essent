import essent.Driver

/**
 * This is a sample testbench that someone would write
 * to test a hardware design generated by Java Essent.
 */

object SampleTestBench {
  def main(args : Array[String]): Unit = {
    Driver.main(Array("-O0", "-java", args(0)))
    val sim : SimulatorWrapper = new SimulatorWrapper(JavaRuntimeCompiler.compile(args(0).split('.')(0) + ".java"))
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

class SimulatorWrapper(sim : Simulator) {
  def peek(signal: String): BigInt = sim.peek(signal)

  def poke(signal: String, value: BigInt): Unit = sim.poke(signal, value.bigInteger)

  def step(): Unit = sim.step()
}