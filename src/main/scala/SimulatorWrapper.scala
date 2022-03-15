package essent

class SimulatorWrapper(sim : Simulator) {
  def peek(signal: String): BigInt = sim.peek(signal)

  def poke(signal: String, value: BigInt): Unit = sim.poke(signal, value.bigInteger)

  def step(update_registers: Boolean): Unit = sim.step(update_registers)
}