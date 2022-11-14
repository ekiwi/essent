package javabackend

import logger.LazyLogging
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import treadle.executable.ClockInfo
import treadle.{ClockInfoAnnotation, TreadleTester}
import essent.SimulatorWrapper

class RiscVMiniTest extends AnyFreeSpec with Matchers with LazyLogging {
  val cycles = 400
  "deltaTest" in {
    val stream = getClass.getResourceAsStream("/core-simple.lo.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val essentSim = SimulatorWrapper(input)
    val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(input),ClockInfoAnnotation(Seq(ClockInfo("clock", period = 10, initialOffset = 1)))))
    val signals = Seq("state", "cntr", "_T_386", "_T_389", "dut.dpath.fe_inst", "dut.dpath.fe_pc", "dut.dpath.ew_inst",
      "dut.dpath.ew_pc", "dut.dpath.ew_alu", "dut.dpath.csr_in", "dut.dpath.st_type", "dut.dpath.ld_type", "dut.dpath.wb_sel",
      "dut.dpath.wb_en", "dut.dpath.csr_cmd", "dut.dpath.illegal", "dut.dpath.pc_check", "dut.dpath.started", "dut.dpath.pc",
      "dut.dpath.csr.time", "dut.dpath.csr.timeh", "dut.dpath.csr.cycle", "dut.dpath.csr.cycleh", "dut.dpath.csr.instret",
      "dut.dpath.csr.instreth", "dut.dpath.csr.PRV", "dut.dpath.csr.PRV1", "dut.dpath.csr.IE", "dut.dpath.csr.IE1", "dut.dpath.csr.MTIP",
      "dut.dpath.csr.MTIE", "dut.dpath.csr.MSIP", "dut.dpath.csr.MSIE", "dut.dpath.csr.mtimecmp", "dut.dpath.csr.mscratch",
      "dut.dpath.csr.mepc", "dut.dpath.csr.mcause", "dut.dpath.csr.mbadaddr", "dut.dpath.csr.mtohost", "dut.dpath.csr.mfromhost")
    val dut = new DeltaTester(treadleSim, essentSim, signals)
    for (_ <- 1 to cycles) {
      dut.step(true)
    }
  }

  "benchmarkEssent" in {
    val stream = getClass.getResourceAsStream("/core-simple.lo.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val startTimeCompile = System.nanoTime
    val essentSim = SimulatorWrapper(input)
    val endTimeCompile = System.nanoTime
    println(s"Essent Compilation Time: ${(endTimeCompile - startTimeCompile)/1000000} milliseconds")

    val startTimeExecute = System.nanoTime
    for (_ <- 1 to cycles) {
      essentSim.step(true)
    }
    val endTimeExecute = System.nanoTime

    println(s"Essent Execution Time: ${(endTimeExecute - startTimeExecute)/1000000} milliseconds")
    println(s"Essent Execution Cycles: $cycles cycles")
    println(s"Essent Execution Time per Cycle: ${(endTimeExecute - startTimeExecute)/cycles} nanoseconds/cycle")
    println(s"Essent Total Time: ${(endTimeExecute + endTimeCompile - startTimeExecute - startTimeCompile)/1000000} milliseconds")
  }

  "benchmarkTreadle" in {
    val stream = getClass.getResourceAsStream("/core-simple.lo.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val startTimeCompile = System.nanoTime
    val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(input),ClockInfoAnnotation(Seq(ClockInfo("clock", period = 10, initialOffset = 1)))))
    val endTimeCompile = System.nanoTime
    println(s"Treadle Compilation Time: ${(endTimeCompile - startTimeCompile)/1000000} milliseconds")

    val startTimeExecute = System.nanoTime
    for (i <- 1 to cycles) {
      treadleSim.step(1)
      println(i)
    }
    val endTimeExecute = System.nanoTime

    println(s"Treadle Execution Time: ${(endTimeExecute - startTimeExecute)/1000000} milliseconds")
    println(s"Treadle Execution Cycles: $cycles cycles")
    println(s"Treadle Execution Time per Cycle: ${(endTimeExecute - startTimeExecute)/cycles} nanoseconds/cycle")
    println(s"Treadle Total Time: ${(endTimeExecute + endTimeCompile - startTimeExecute - startTimeCompile)/1000000} milliseconds")
  }
}