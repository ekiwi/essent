package benchmarks

import essent.SimulatorWrapper

object RiscvMiniBenchmark extends App{
  val circuitSource = os.read(os.pwd / "src" / "test" / "resources" / "TLRAMStandalone.fir")
  val startTimeCompile = System.nanoTime
  val sim = SimulatorWrapper(circuitSource, optimization="O1")
  val endTimeCompile = System.nanoTime
  println(s"Essent Compilation Time: ${(endTimeCompile - startTimeCompile) / 1000000} milliseconds")
  val inputSource = os.pwd / "src" / "test" / "resources" / "TLRAMStandalone.txt"
  val inputs = os.read.lines(inputSource)

  val startTime = System.nanoTime
  for (line <- inputs) {
    val vals = line.split(" ")
    sim.poke("reset", BigInt(vals(0), 16))
    sim.poke("in_a_valid", BigInt(vals(1), 16))
    sim.poke("in_a_bits_opcode", BigInt(vals(2), 16))
    sim.poke("in_a_bits_param", BigInt(vals(3), 16))
    sim.poke("in_a_bits_size", BigInt(vals(4), 16))
    sim.poke("in_a_bits_source", BigInt(vals(5), 16))
    sim.poke("in_a_bits_address", BigInt(vals(6), 16))
    sim.poke("in_a_bits_mask", BigInt(vals(7), 16))
    sim.poke("in_a_bits_data", BigInt(vals(8), 16))
    sim.poke("in_a_bits_corrupt", BigInt(vals(9), 16))
    sim.poke("in_d_ready", BigInt(vals(10), 16))
    sim.step(update_registers = true)
  }
  val endTime = System.nanoTime
  println(s"TLRAMStandalone: ${(endTime - startTime) / 1000000} milliseconds")
}
