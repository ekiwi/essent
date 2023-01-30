package javabackend

import essent.SimulatorWrapper
import firrtl.stage.FirrtlSourceAnnotation
import org.scalatest.freespec.AnyFreeSpec
import treadle.TreadleTester

class ConstellationBenchmark extends AnyFreeSpec{
  "TestConfig00" in {
    val stream = getClass.getResourceAsStream("/NoCChiselTester.lo.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val startTimeCompile = System.nanoTime
    val sim = SimulatorWrapper(circuitSource)
    val endTimeCompile = System.nanoTime
    println(s"Compilation Time: ${(endTimeCompile - startTimeCompile) / 1000000} milliseconds")

    val startTime = System.nanoTime
    sim.poke("reset", 1)
    sim.step(true)
    sim.poke("reset", 0)
    while (sim.step(true)) {}
    val endTime = System.nanoTime
    println(s"Simulation Time: ${(endTime - startTime) / 1000000} milliseconds")
  }

  "AXITestConfig03" in {
    val stream = getClass.getResourceAsStream("/AXI4NoCChiselTester.lo.fir")
    val circuitSource = scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
    val startTimeCompile = System.nanoTime
    val sim = SimulatorWrapper(circuitSource)
    val endTimeCompile = System.nanoTime
    println(s"Compilation Time: ${(endTimeCompile - startTimeCompile) / 1000000} milliseconds")

    val startTime = System.nanoTime
    sim.poke("reset", 1)
    sim.step(true)
    sim.poke("reset", 0)
    while (sim.step(true)) {}
    val endTime = System.nanoTime
    println(s"Simulation Time: ${(endTime - startTime) / 1000000} milliseconds")
  }
}
