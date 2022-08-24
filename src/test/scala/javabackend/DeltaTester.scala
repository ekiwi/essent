package javabackend

import essent.IsSimulator
import treadle.TreadleTester


class DeltaTester(reference: TreadleTester, sim: IsSimulator, checkSignals: Seq[String] = Seq()) extends IsSimulator {
  private var cycle = 0

  override def peek(signal: String): BigInt = {
    val expected = reference.peek(signal)
    val actual = sim.peek(signal)
    if(expected != actual) {
      reference.finish // make sure VCD is written to disk
      println("Internal Signals:")
      checkSignals.foreach { signal =>
        val expected = reference.peek(signal)
        val actual = sim.peek(signal)
        println(s"Treadle: $signal=$expected, ESSENT: $signal=$actual")
      }
    }
    assert(expected == actual, s"@$cycle Treadle says $signal=$expected, but the ESSENT sim returned $signal=$actual")
    actual
  }

  override def poke(signal: String, value: BigInt): Unit = {
    reference.poke(signal, value)
    sim.poke(signal, value)
  }

  override def step(update_registers: Boolean, checkSignal: Boolean = true): Unit = {
    if(update_registers) {
      reference.step()
      cycle += 1
    }
    sim.step(update_registers)
    if (checkSignal) {
      checkSignals.foreach { signal =>
        peek(signal)
      }
    }
  }
}
