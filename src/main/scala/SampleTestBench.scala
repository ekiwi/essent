/**
 * This is a sample testbench that someone would write
 * to test a hardware design generated by Java Essent.
 */

object SampleTestBench {
  def main(args : Array[String]): Unit = {
    val sim = JavaRuntimeCompiler.compile(args(0))
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
