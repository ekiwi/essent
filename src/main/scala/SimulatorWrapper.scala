package essent

class SimulatorWrapper(sim : Simulator) extends IsSimulator {
  private var stale = true
  def peek(signal: String): BigInt = {
    if(stale) {
      sim.step(false)
      stale = false
    }
    sim.peek(signal)
  }

  def poke(signal: String, value: BigInt): Unit = sim.poke(signal, value.bigInteger)

  def step(update_registers: Boolean): Unit = {
    stale = true
    sim.step(update_registers)
  }
}

trait IsSimulator {
  def peek(signal: String): BigInt
  def poke(signal: String, value: BigInt): Unit
  def step(update_registers: Boolean): Unit
}