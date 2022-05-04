package benchmarks

import essent.SimulatorWrapper

object RiscvMiniBenchmark extends App{
  var cycles = 0L
  val input = os.read(os.pwd / "src" / "test" / "resources" / "core-simple.lo.fir")
  val startTimeCompile = System.nanoTime
  val essentSim = SimulatorWrapper(input)
  val endTimeCompile = System.nanoTime

  val startTimeExecute = System.nanoTime
  while (essentSim.peek("cycle") != BigInt(100000)) {
    essentSim.step(true)
    cycles += 1
  }
  val endTimeExecute = System.nanoTime
}
