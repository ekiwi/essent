package essent

class SimulatorWrapper(sim : Simulator) extends IsSimulator {
  private var stale = true
  def peek(signal: String): BigInt = {
    if (stale) {
      sim.step(false)
      stale = false
    }
    sim.peek(signal)
  }

  def poke(signal: String, value: BigInt): Unit = sim.poke(signal, value.bigInteger)

  def step(update_registers: Boolean, checkSignal: Boolean = true): Unit = {
    stale = true
    sim.step(update_registers)
  }

  def getStopCodes: List[Int] = {
    sim.getStopCodes.toList
  }
}

trait IsSimulator {
  def peek(signal: String): BigInt
  def poke(signal: String, value: BigInt): Unit
  def step(update_registers: Boolean, checkSignal: Boolean = true): Unit
}

/** Work in progress. Only takes in strings for now. */
object SimulatorWrapper {
  def apply(source : String, optimization : String = "O3", partCutoff: Int = 8): SimulatorWrapper = {
    val startTimeCompile = System.nanoTime
    val path = Driver.generateTester(source, optimization, partCutoff)
    val endTimeCompile = System.nanoTime
    println(s"ESSENT time: ${(endTimeCompile - startTimeCompile)/1000000}")
    val startTimeCompile2 = System.nanoTime
    val si = new SimulatorWrapper(JavaRuntimeCompiler.compile(path))
    val endTimeCompile2 = System.nanoTime
    println(s"javac time: ${(endTimeCompile2 - startTimeCompile2)/1000000}")
    si
  }
}