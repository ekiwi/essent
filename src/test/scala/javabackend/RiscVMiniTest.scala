package javabackend

import logger.LazyLogging
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import treadle.executable.{ClockInfo, StopException}
import treadle.{ClockInfoAnnotation, TreadleTester}
import essent.{IsSimulator, SimulatorWrapper}

class RiscVMiniTest extends AnyFreeSpec with Matchers with LazyLogging {
  "deltaTest" in {
    val stream = getClass.getResourceAsStream("/core-simple.lo.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val essentSim = SimulatorWrapper(input)
    val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(input),ClockInfoAnnotation(Seq(ClockInfo("clock", period = 10, initialOffset = 1)))))
    val dut = new DeltaTester(treadleSim, essentSim, Seq("state", "cycle", "cntr", "_T_386", "_T_389"))
    for (_ <- 0 to 400) {
      dut.step(true)
    }
  }

  "benchmarkEssent" in {
    var cycles = 0L
    val stream = getClass.getResourceAsStream("/core-simple.lo.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val startTimeCompile = System.nanoTime
    val essentSim = SimulatorWrapper(input)
    val endTimeCompile = System.nanoTime
    println(s"Essent Compilation Time: ${(endTimeCompile - startTimeCompile)/1000000} milliseconds")

    val startTimeExecute = System.nanoTime
    while (essentSim.peek("cycle") != BigInt(100000)) {
      essentSim.step(true)
      cycles += 1
    }
    val endTimeExecute = System.nanoTime

    println(s"Essent Execution Time: ${(endTimeExecute - startTimeExecute)/1000000} milliseconds")
    println(s"Essent Execution Cycles: $cycles cycles")
    println(s"Essent Execution Time per Cycle: ${(endTimeExecute - startTimeExecute)/cycles} nanoseconds/cycle")
    println(s"Essent Total Time: ${(endTimeExecute + endTimeCompile - startTimeExecute - startTimeCompile)/1000000} milliseconds")
  }

  "benchmarkTreadle" in {
    var cycles = 0L
    val stream = getClass.getResourceAsStream("/core-simple.lo.fir")
    val input = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val startTimeCompile = System.nanoTime
    val treadleSim = TreadleTester(Seq(FirrtlSourceAnnotation(input),ClockInfoAnnotation(Seq(ClockInfo("clock", period = 10, initialOffset = 1)))))
    val endTimeCompile = System.nanoTime
    println(s"Treadle Compilation Time: ${(endTimeCompile - startTimeCompile)/1000000} milliseconds")

    val startTimeExecute = System.nanoTime
    while (treadleSim.peek("cycle") != BigInt(100000)) {
      treadleSim.step(1)
      cycles += 1
    }
    val endTimeExecute = System.nanoTime

    println(s"Treadle Execution Time: ${(endTimeExecute - startTimeExecute)/1000000} milliseconds")
    println(s"Treadle Execution Cycles: $cycles cycles")
    println(s"Treadle Execution Time per Cycle: ${(endTimeExecute - startTimeExecute)/cycles} nanoseconds/cycle")
    println(s"Treadle Total Time: ${(endTimeExecute + endTimeCompile - startTimeExecute - startTimeCompile)/1000000} milliseconds")
  }
}