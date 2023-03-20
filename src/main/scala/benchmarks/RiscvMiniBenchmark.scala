package benchmarks

import essent.SimulatorWrapper

object RiscvMiniBenchmark extends App{
  val circuitSource = os.read(os.pwd / "src" / "test" / "resources" / "NoCChiselTester.lo.fir")
  val sim = SimulatorWrapper(circuitSource)
  sim.poke("reset", 1)
  sim.step(true)
  sim.poke("reset", 0)
  while (sim.step(true)) {}
}
