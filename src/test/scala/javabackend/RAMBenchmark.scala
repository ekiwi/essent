package javabackend

import essent.SimulatorWrapper
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

/**
 *  Benchmark for TLRAMStandalone.fir and ServTopWithRam.fir.
 *  Must have TLRAMStandalone.txt and ServTopWithRam.txt
 *  that contains the inputs downloaded separately.
 */
class RAMBenchmark extends AnyFreeSpec {
  "ServTopWithRamESSENT" in {
    val stream = getClass.getResourceAsStream("/ServTopWithRam.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val sim = SimulatorWrapper(circuitSource)
    val inputSource = os.pwd / "src" / "test" / "resources" / "ServTopWithRam.txt"
    val inputs = os.read.lines(inputSource)

    sim.poke("reset", BigInt(0))
    sim.poke("io_timerInterrupt", BigInt(0))
    sim.poke("io_ibus_rdt", BigInt(0))
    sim.poke("io_ibus_ack", BigInt(0))
    sim.poke("io_dbus_rdt", BigInt(0))
    sim.poke("io_dbus_ack", BigInt(0))

    val startTime = System.nanoTime
    for (line <- inputs) {
      val vals = line.split(" ")
      sim.poke("reset", BigInt(vals(0), 16))
      sim.poke("io_timerInterrupt", BigInt(vals(1), 16))
      sim.poke("io_ibus_rdt", BigInt(vals(2), 16))
      sim.poke("io_ibus_ack", BigInt(vals(3), 16))
      sim.poke("io_dbus_rdt", BigInt(vals(4), 16))
      sim.poke("io_dbus_ack", BigInt(vals(5), 16))
      sim.step(true)
    }
    val endTime = System.nanoTime
    println(s"ServTopWithRam: ${(endTime - startTime) / 1000000} milliseconds")
  }

  "ServTopWithRamTreadle" in {
    val stream = getClass.getResourceAsStream("/ServTopWithRam.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val sim = TreadleTester(Seq(FirrtlSourceAnnotation(circuitSource)))
    val inputSource = os.pwd / "src" / "test" / "resources" / "ServTopWithRam.txt"
    val inputs = os.read.lines(inputSource)

    sim.poke("reset", BigInt(0))
    sim.poke("io_timerInterrupt", BigInt(0))
    sim.poke("io_ibus_rdt", BigInt(0))
    sim.poke("io_ibus_ack", BigInt(0))
    sim.poke("io_dbus_rdt", BigInt(0))
    sim.poke("io_dbus_ack", BigInt(0))

    val startTime = System.nanoTime
    for (line <- inputs) {
      val vals = line.split(" ")
      sim.poke("reset", BigInt(vals(0), 16))
      sim.poke("io_timerInterrupt", BigInt(vals(1), 16))
      sim.poke("io_ibus_rdt", BigInt(vals(2), 16))
      sim.poke("io_ibus_ack", BigInt(vals(3), 16))
      sim.poke("io_dbus_rdt", BigInt(vals(4), 16))
      sim.poke("io_dbus_ack", BigInt(vals(5), 16))
      sim.step(1)
    }
    val endTime = System.nanoTime
    println(s"ServTopWithRam: ${(endTime - startTime) / 1000000} milliseconds")
  }

  "ServTopWithRamDelta" in {
    val stream = getClass.getResourceAsStream("/ServTopWithRam.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val eSim = SimulatorWrapper(circuitSource)
    val tSim = TreadleTester(Seq(FirrtlSourceAnnotation(circuitSource)))
    val checkSignals = Seq("io_ibus_adr", "io_ibus_cyc", "io_dbus_adr", "io_dbus_cyc",
      "io_dbus_dat", "io_dbus_sel", "io_dbus_we")
    val sim = new DeltaTester(tSim, eSim, checkSignals)
    val inputSource = os.pwd / "src" / "test" / "resources" / "ServTopWithRam.txt"
    val inputs = os.read.lines(inputSource)

    sim.poke("reset", BigInt(0))
    sim.poke("io_timerInterrupt", BigInt(0))
    sim.poke("io_ibus_rdt", BigInt(0))
    sim.poke("io_ibus_ack", BigInt(0))
    sim.poke("io_dbus_rdt", BigInt(0))
    sim.poke("io_dbus_ack", BigInt(0))

    for (line <- inputs) {
      val vals = line.split(" ")
      sim.poke("reset", BigInt(vals(0), 16))
      sim.poke("io_timerInterrupt", BigInt(vals(1), 16))
      sim.poke("io_ibus_rdt", BigInt(vals(2), 16))
      sim.poke("io_ibus_ack", BigInt(vals(3), 16))
      sim.poke("io_dbus_rdt", BigInt(vals(4), 16))
      sim.poke("io_dbus_ack", BigInt(vals(5), 16))
      sim.step(true)
    }
  }

  "TLRAMStandaloneESSENT" in {
    val stream = getClass.getResourceAsStream("/TLRAMStandalone.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val sim = SimulatorWrapper(circuitSource)
    val inputSource = os.pwd / "src" / "test" / "resources" / "TLRAMStandalone.txt"
    val inputs = os.read.lines(inputSource)

    sim.poke("reset", BigInt(0))
    sim.poke("in_a_valid", BigInt(0))
    sim.poke("in_a_bits_opcode", BigInt(0))
    sim.poke("in_a_bits_param", BigInt(0))
    sim.poke("in_a_bits_size", BigInt(0))
    sim.poke("in_a_bits_source", BigInt(0))
    sim.poke("in_a_bits_address", BigInt(0))
    sim.poke("in_a_bits_mask", BigInt(0))
    sim.poke("in_a_bits_data", BigInt(0))
    sim.poke("in_a_bits_corrupt", BigInt(0))
    sim.poke("in_d_ready", BigInt(0))

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
      sim.step(true)
    }
    val endTime = System.nanoTime
    println(s"TLRAMStandalone: ${(endTime - startTime) / 1000000} milliseconds")
  }

  "TLRAMStandaloneTreadle" in {
    val stream = getClass.getResourceAsStream("/TLRAMStandalone.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val sim = TreadleTester(Seq(FirrtlSourceAnnotation(circuitSource)))
    val inputSource = os.pwd / "src" / "test" / "resources" / "TLRAMStandalone.txt"
    val inputs = os.read.lines(inputSource)

    sim.poke("reset", BigInt(0))
    sim.poke("in_a_valid", BigInt(0))
    sim.poke("in_a_bits_opcode", BigInt(0))
    sim.poke("in_a_bits_param", BigInt(0))
    sim.poke("in_a_bits_size", BigInt(0))
    sim.poke("in_a_bits_source", BigInt(0))
    sim.poke("in_a_bits_address", BigInt(0))
    sim.poke("in_a_bits_mask", BigInt(0))
    sim.poke("in_a_bits_data", BigInt(0))
    sim.poke("in_a_bits_corrupt", BigInt(0))
    sim.poke("in_d_ready", BigInt(0))

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
      sim.step(1)
    }
    val endTime = System.nanoTime
    println(s"TLRAMStandalone: ${(endTime - startTime) / 1000000} milliseconds")
  }

  "TLRAMStandaloneDelta" in {
    val stream = getClass.getResourceAsStream("/TLRAMStandalone.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val tSim = TreadleTester(Seq(FirrtlSourceAnnotation(circuitSource)))
    val eSim = SimulatorWrapper(circuitSource)
    val checkSignals = Seq("in_a_ready", "in_d_valid", "in_d_bits_opcode", "in_d_bits_param", "in_d_bits_size",
      "in_d_bits_source", "in_d_bits_sink", "in_d_bits_denied", "in_d_bits_data", "in_d_bits_corrupt")
    val sim = new DeltaTester(tSim, eSim, checkSignals)
    val inputSource = os.pwd / "src" / "test" / "resources" / "TLRAMStandalone.txt"
    val inputs = os.read.lines(inputSource)

    sim.poke("reset", BigInt(0))
    sim.poke("in_a_valid", BigInt(0))
    sim.poke("in_a_bits_opcode", BigInt(0))
    sim.poke("in_a_bits_param", BigInt(0))
    sim.poke("in_a_bits_size", BigInt(0))
    sim.poke("in_a_bits_source", BigInt(0))
    sim.poke("in_a_bits_address", BigInt(0))
    sim.poke("in_a_bits_mask", BigInt(0))
    sim.poke("in_a_bits_data", BigInt(0))
    sim.poke("in_a_bits_corrupt", BigInt(0))
    sim.poke("in_d_ready", BigInt(0))

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
      sim.step(true)
    }
  }
}
