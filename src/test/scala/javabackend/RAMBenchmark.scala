package javabackend

import essent.{IsSimulator, JavaRuntimeCompiler, SimulatorWrapper}
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.{RandomSeedAnnotation, RandomizeAtStartupAnnotation, TreadleTester}

import java.io.{FileOutputStream, PrintStream}

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

    //    val startTime = System.nanoTime
    for (i <- 0 to 9) {
      val line = inputs(i)
      val vals = line.split(" ")
      sim.poke("reset", BigInt(vals(0), 16))
      sim.poke("io_timerInterrupt", BigInt(vals(1), 16))
      sim.poke("io_ibus_rdt", BigInt(vals(2), 16))
      sim.poke("io_ibus_ack", BigInt(vals(3), 16))
      sim.poke("io_dbus_rdt", BigInt(vals(4), 16))
      sim.poke("io_dbus_ack", BigInt(vals(5), 16))

      println(sim.peek("reset"))
      println(sim.peek("io_timerInterrupt"))
      println(sim.peek("io_ibus_adr"))
      println(sim.peek("io_ibus_cyc"))
      println(sim.peek("io_ibus_rdt"))
      println(sim.peek("io_ibus_ack"))
      println(sim.peek("io_dbus_adr"))
      println(sim.peek("io_dbus_cyc"))
      println(sim.peek("io_dbus_rdt"))
      println(sim.peek("io_dbus_ack"))
      println(sim.peek("io_dbus_dat"))
      println(sim.peek("io_dbus_sel"))
      println(sim.peek("io_dbus_we"))
      println()
      sim.step(true)
    }
    //    val endTime = System.nanoTime
//    println(s"ServTopWithRam: ${(endTime - startTime) / 1000000} milliseconds")
  }

  "ServTopWithRamTreadle" in {
    val stream = getClass.getResourceAsStream("/ServTopWithRam.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val sim = TreadleTester(Seq(FirrtlSourceAnnotation(circuitSource), RandomSeedAnnotation(33), RandomizeAtStartupAnnotation))
    val inputSource = os.pwd / "src" / "test" / "resources" / "ServTopWithRam.txt"
    val inputs = os.read.lines(inputSource)

//    val startTime = System.nanoTime
    for (i <- 0 to 9) {
      val line = inputs(i)
      val vals = line.split(" ")
      sim.poke("reset", BigInt(vals(0), 16))
      sim.poke("io_timerInterrupt", BigInt(vals(1), 16))
      sim.poke("io_ibus_rdt", BigInt(vals(2), 16))
      sim.poke("io_ibus_ack", BigInt(vals(3), 16))
      sim.poke("io_dbus_rdt", BigInt(vals(4), 16))
      sim.poke("io_dbus_ack", BigInt(vals(5), 16))

      println(sim.peek("reset"))
      println(sim.peek("io_timerInterrupt"))
      println(sim.peek("io_ibus_adr"))
      println(sim.peek("io_ibus_cyc"))
      println(sim.peek("io_ibus_rdt"))
      println(sim.peek("io_ibus_ack"))
      println(sim.peek("io_dbus_adr"))
      println(sim.peek("io_dbus_cyc"))
      println(sim.peek("io_dbus_rdt"))
      println(sim.peek("io_dbus_ack"))
      println(sim.peek("io_dbus_dat"))
      println(sim.peek("io_dbus_sel"))
      println(sim.peek("io_dbus_we"))
      println()
      sim.step(1)
    }
//    val endTime = System.nanoTime
//    println(s"ServTopWithRam: ${(endTime - startTime) / 1000000} milliseconds")
  }

  "ServTopWithRamDelta" in {
    val stream = getClass.getResourceAsStream("/ServTopWithRam.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val eSim = SimulatorWrapper(circuitSource)
    val tSim = TreadleTester(Seq(FirrtlSourceAnnotation(circuitSource)))
    val checkSignals = Seq("reset", "io_timerInterrupt", "io_ibus_adr", "io_ibus_cyc", "io_ibus_rdt", "io_ibus_ack",
      "io_dbus_adr", "io_dbus_cyc", "io_dbus_rdt", "io_dbus_ack", "io_dbus_dat", "io_dbus_sel",
      "io_dbus_we", "top.state.init", "top.state.countDone", "top.state.countEnabled", "top.state.count",
      "top.state.countR", "top.state.stageTwoRequest", "top.state.stageTwoPending", "top.state.controlJump",
      "top.state.pendingIrq", "top.state.REG", "top.state.REG_1", "top.decode.r", "top.decode.rs1Address",
      "top.decode.r_1", "top.decode.funct3", "top.decode.imm30", "top.decode.opcode", "top.decode.op20",
      "top.decode.op21", "top.decode.op22", "top.decode.op26", "top.decode.signbit", "top.decode.imm19_12_20",
      "top.decode.imm7", "top.decode.imm30_25", "top.decode.imm24_20", "top.decode.imm11_7",
      "top.bufreg.carry", "top.bufreg.data", "top.bufreg.lsb_1", "top.bufreg.lsb_0", "top.control.enablePc",
      "top.control.pc", "top.control.pcPlusOffsetCarry", "top.control.pcPlus4Carry", "top.alu.negativeBCarry",
      "top.alu.addCarry", "top.alu.shiftAmount", "top.alu.shiftAmountMSB", "top.alu.equalBuf", "top.alu.ltBuf",
      "top.alu.resultLtBuf", "top.alu.shift.cnt", "top.alu.shift.signbit", "top.alu.shift.wrapped",
      "top.mem.data", "top.mem.signBit", "top.mem.REG", "top.csr.mStatusMie", "top.csr.mStatusMpie",
      "top.csr.mieMtie", "top.csr.mStatus", "top.csr.mCause3_0", "top.csr.mCause31", "top.csr.REG",
      "ramInterface.readRequestBuffer", "ramInterface.rgnt", "ramInterface.writeCount", "ramInterface.writeGo",
      "ramInterface.writeData0Buffer", "ramInterface.writeData1Buffer", "ramInterface.writeEnable0Buffer",
      "ramInterface.writeEnable1Buffer", "ramInterface.writeRequestBuffer", "ramInterface.readCount",
      "ramInterface.readTrigger1", "ramInterface.readData0Buffer", "ramInterface.readData1Buffer",
      "ram.memory_MPORT_1_en_pipe_0", "ram.memory_MPORT_1_addr_pipe_0")
    val sim = new DeltaTester(tSim, eSim, checkSignals)
    val inputSource = os.pwd / "src" / "test" / "resources" / "ServTopWithRam.txt"
    val inputs = os.read.lines(inputSource)

    for (line <- inputs) {
      val vals = line.split(" ")
      sim.poke("reset", BigInt(vals(0), 16))
      sim.poke("io_timerInterrupt", BigInt(vals(1), 16))
      sim.poke("io_ibus_rdt", BigInt(vals(2), 16))
      sim.poke("io_ibus_ack", BigInt(vals(3), 16))
      sim.poke("io_dbus_rdt", BigInt(vals(4), 16))
      sim.poke("io_dbus_ack", BigInt(vals(5), 16))
      println(sim.peek("top.alu.shift.cnt"))
      sim.step(update_registers=true)
    }
  }

  "ServTopWithRamDelta2" in {
    val stream = getClass.getResourceAsStream("/ServTopWithRam.java")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val eSim = new SimulatorWrapper(JavaRuntimeCompiler.compile("ServTopWithRam", circuitSource))
    val inputSource = os.pwd / "src" / "test" / "resources" / "ServTopWithRam.txt"
    val inputs = os.read.lines(inputSource)

    for (i <- 0 to 9) {
      val line = inputs(i)
      val vals = line.split(" ")
      eSim.poke("reset", BigInt(vals(0), 16))
      eSim.poke("io_timerInterrupt", BigInt(vals(1), 16))
      eSim.poke("io_ibus_rdt", BigInt(vals(2), 16))
      eSim.poke("io_ibus_ack", BigInt(vals(3), 16))
      eSim.poke("io_dbus_rdt", BigInt(vals(4), 16))
      eSim.poke("io_dbus_ack", BigInt(vals(5), 16))
      println(i, eSim.peek("top.alu.shift.cnt"))
      eSim.step(update_registers = true)
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

  "TLRAMStandaloneDelta2" in {
    val stream = getClass.getResourceAsStream("/TLRAMStandalone.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val stream2 = getClass.getResourceAsStream("/TLRAMStandalone.java")
    val circuitSource2 = scala.io.Source.fromInputStream(stream2).getLines().mkString("\n")
    val tSim = TreadleTester(Seq(FirrtlSourceAnnotation(circuitSource)))
    val eSim = new SimulatorWrapper(JavaRuntimeCompiler.compile("TLRAMStandalone", circuitSource2))
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
