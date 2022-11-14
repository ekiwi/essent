package javabackend

import essent.SimulatorWrapper
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

class TileTesterBenchmark extends AnyFreeSpec {
  "TileTesterESSENT" in {
    val stream = getClass.getResourceAsStream("/TileTester.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val startTimeCompile = System.nanoTime
    val sim = SimulatorWrapper(circuitSource)
    val endTimeCompile = System.nanoTime
    println(s"Essent Compilation Time: ${(endTimeCompile - startTimeCompile) / 1000000} milliseconds")

    val iterations = 100
    val startTime = System.nanoTime
    for (_ <- 1 to iterations) {
      for (_ <- 0 to 43303) {
        sim.step(update_registers = true)
      }
      sim.poke("reset", 1)
      sim.step(update_registers = true)
      sim.poke("reset", 0)
    }
    val endTime = System.nanoTime
    println(s"TileTester: ${(endTime - startTime) / 1000000} milliseconds")
    println(s"ns per cycle: ${(endTime - startTime) / (43304 * iterations)}")
  }

  "TileTesterTreadle" in {
    val stream = getClass.getResourceAsStream("/TileTester.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val startTimeCompile = System.nanoTime
    val sim = TreadleTester(Seq(FirrtlSourceAnnotation(circuitSource)))
    val endTimeCompile = System.nanoTime
    println(s"Treadle Compilation Time: ${(endTimeCompile - startTimeCompile) / 1000000} milliseconds")

    val startTime = System.nanoTime
    sim.step(43304)
    val endTime = System.nanoTime
    println(s"TileTester: ${(endTime - startTime) / 1000000} milliseconds")
  }

  "TileTesterDelta" in {
    val stream = getClass.getResourceAsStream("/TileTester.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val eSim = SimulatorWrapper(circuitSource)
    val tSim = TreadleTester(Seq(FirrtlSourceAnnotation(circuitSource)))
    val checkSignals = Seq("state", "cycle", "cntr", "addr", "off", "reset", "dut.core.dpath.fe_inst", "dut.core.dpath.fe_pc",
      "dut.core.dpath.ew_inst", "dut.core.dpath.ew_pc", "dut.core.dpath.ew_alu", "dut.core.dpath.csr_in",
      "dut.core.dpath.st_type", "dut.core.dpath.ld_type", "dut.core.dpath.wb_sel", "dut.core.dpath.wb_en",
      "dut.core.dpath.csr_cmd", "dut.core.dpath.illegal", "dut.core.dpath.pc_check", "dut.core.dpath.started",
      "dut.core.dpath.pc", "dut.core.dpath.csr.time", "dut.core.dpath.csr.timeh", "dut.core.dpath.csr.cycle",
      "dut.core.dpath.csr.cycleh", "dut.core.dpath.csr.instret", "dut.core.dpath.csr.instreth", "dut.core.dpath.csr.PRV",
      "dut.core.dpath.csr.PRV1", "dut.core.dpath.csr.IE", "dut.core.dpath.csr.IE1", "dut.core.dpath.csr.MTIP",
      "dut.core.dpath.csr.MTIE", "dut.core.dpath.csr.MSIP", "dut.core.dpath.csr.MSIE", "dut.core.dpath.csr.mtimecmp",
      "dut.core.dpath.csr.mscratch", "dut.core.dpath.csr.mepc", "dut.core.dpath.csr.mcause", "dut.core.dpath.csr.mbadaddr",
      "dut.core.dpath.csr.mtohost", "dut.core.dpath.csr.mfromhost", "dut.icache.metaMem_tag_rmeta_en_pipe_0",
      "dut.icache.metaMem_tag_rmeta_addr_pipe_0", "dut.icache.dataMem_0_0__T_22_en_pipe_0",
      "dut.icache.dataMem_0_0__T_22_addr_pipe_0", "dut.icache.dataMem_0_1__T_22_en_pipe_0",
      "dut.icache.dataMem_0_1__T_22_addr_pipe_0", "dut.icache.dataMem_0_2__T_22_en_pipe_0",
      "dut.icache.dataMem_0_2__T_22_addr_pipe_0", "dut.icache.dataMem_0_3__T_22_en_pipe_0",
      "dut.icache.dataMem_0_3__T_22_addr_pipe_0", "dut.icache.dataMem_1_0__T_29_en_pipe_0",
      "dut.icache.dataMem_1_0__T_29_addr_pipe_0", "dut.icache.dataMem_1_1__T_29_en_pipe_0",
      "dut.icache.dataMem_1_1__T_29_addr_pipe_0", "dut.icache.dataMem_1_2__T_29_en_pipe_0",
      "dut.icache.dataMem_1_2__T_29_addr_pipe_0", "dut.icache.dataMem_1_3__T_29_en_pipe_0",
      "dut.icache.dataMem_1_3__T_29_addr_pipe_0", "dut.icache.dataMem_2_0__T_36_en_pipe_0",
      "dut.icache.dataMem_2_0__T_36_addr_pipe_0", "dut.icache.dataMem_2_1__T_36_en_pipe_0",
      "dut.icache.dataMem_2_1__T_36_addr_pipe_0", "dut.icache.dataMem_2_2__T_36_en_pipe_0",
      "dut.icache.dataMem_2_2__T_36_addr_pipe_0", "dut.icache.dataMem_2_3__T_36_en_pipe_0",
      "dut.icache.dataMem_2_3__T_36_addr_pipe_0", "dut.icache.dataMem_3_0__T_43_en_pipe_0",
      "dut.icache.dataMem_3_0__T_43_addr_pipe_0", "dut.icache.dataMem_3_1__T_43_en_pipe_0",
      "dut.icache.dataMem_3_1__T_43_addr_pipe_0", "dut.icache.dataMem_3_2__T_43_en_pipe_0",
      "dut.icache.dataMem_3_2__T_43_addr_pipe_0", "dut.icache.dataMem_3_3__T_43_en_pipe_0",
      "dut.icache.dataMem_3_3__T_43_addr_pipe_0", "dut.icache.state", "dut.icache.v", "dut.icache.d", "dut.icache.addr_reg",
      "dut.icache.cpu_data", "dut.icache.cpu_mask", "dut.icache.read_count", "dut.icache.write_count",
      "dut.icache.is_alloc_reg", "dut.icache.ren_reg", "dut.icache.rdata_buf", "dut.icache.refill_buf_0",
      "dut.icache.refill_buf_1", "dut.dcache.metaMem_tag_rmeta_en_pipe_0", "dut.dcache.metaMem_tag_rmeta_addr_pipe_0",
      "dut.dcache.dataMem_0_0__T_22_en_pipe_0", "dut.dcache.dataMem_0_0__T_22_addr_pipe_0",
      "dut.dcache.dataMem_0_1__T_22_en_pipe_0", "dut.dcache.dataMem_0_1__T_22_addr_pipe_0",
      "dut.dcache.dataMem_0_2__T_22_en_pipe_0", "dut.dcache.dataMem_0_2__T_22_addr_pipe_0",
      "dut.dcache.dataMem_0_3__T_22_en_pipe_0", "dut.dcache.dataMem_0_3__T_22_addr_pipe_0",
      "dut.dcache.dataMem_1_0__T_29_en_pipe_0", "dut.dcache.dataMem_1_0__T_29_addr_pipe_0",
      "dut.dcache.dataMem_1_1__T_29_en_pipe_0", "dut.dcache.dataMem_1_1__T_29_addr_pipe_0",
      "dut.dcache.dataMem_1_2__T_29_en_pipe_0", "dut.dcache.dataMem_1_2__T_29_addr_pipe_0",
      "dut.dcache.dataMem_1_3__T_29_en_pipe_0", "dut.dcache.dataMem_1_3__T_29_addr_pipe_0",
      "dut.dcache.dataMem_2_0__T_36_en_pipe_0", "dut.dcache.dataMem_2_0__T_36_addr_pipe_0",
      "dut.dcache.dataMem_2_1__T_36_en_pipe_0", "dut.dcache.dataMem_2_1__T_36_addr_pipe_0",
      "dut.dcache.dataMem_2_2__T_36_en_pipe_0", "dut.dcache.dataMem_2_2__T_36_addr_pipe_0",
      "dut.dcache.dataMem_2_3__T_36_en_pipe_0", "dut.dcache.dataMem_2_3__T_36_addr_pipe_0",
      "dut.dcache.dataMem_3_0__T_43_en_pipe_0", "dut.dcache.dataMem_3_0__T_43_addr_pipe_0",
      "dut.dcache.dataMem_3_1__T_43_en_pipe_0", "dut.dcache.dataMem_3_1__T_43_addr_pipe_0",
      "dut.dcache.dataMem_3_2__T_43_en_pipe_0", "dut.dcache.dataMem_3_2__T_43_addr_pipe_0",
      "dut.dcache.dataMem_3_3__T_43_en_pipe_0", "dut.dcache.dataMem_3_3__T_43_addr_pipe_0",
      "dut.dcache.state", "dut.dcache.v", "dut.dcache.d", "dut.dcache.addr_reg", "dut.dcache.cpu_data",
      "dut.dcache.cpu_mask", "dut.dcache.read_count", "dut.dcache.write_count", "dut.dcache.is_alloc_reg",
      "dut.dcache.ren_reg", "dut.dcache.rdata_buf", "dut.dcache.refill_buf_0", "dut.dcache.refill_buf_1",
      "dut.arb.state")
    val dut = new DeltaTester(tSim, eSim, checkSignals)

    for (_ <- 0 to 43303) {
      dut.step(true)
    }
  }
}