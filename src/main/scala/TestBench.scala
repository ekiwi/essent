import essent.Simulator

object TestBench {
  def main(args: Array[String]) {
    val sim = new Simulator(args(0))
    //sim.poke(“io_a”, 123)
    //sim.step()
    //assert(sim.peek(“io_result”) == 2)
  }
}
