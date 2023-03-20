package javabackend

import essent.VCD
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

class VCDTest extends AnyFreeSpec {

  "writeHeader" in {
    val vcdGen = VCD;
    vcdGen.setFile("vcd_files/writeHeader.vcd");
    vcdGen.writeHeader();
  }

  "basicVCDGenerator" in {
    val filename = "vcd_files/basicVCDGenerator.vcd"

    val vcdGen = VCD;
    vcdGen.setFile(filename)
    vcdGen.writeHeader();

    vcdGen.addSignal("one", 1)
    vcdGen.addSignal("two", 1)
    vcdGen.addSignal("three", 1)
    vcdGen.addSignal("four", 1)
    vcdGen.addSignal("five", 1)
    vcdGen.addSignal("six", 1)
    vcdGen.addSignal("seven", 1)
    vcdGen.addSignal("eight", 1)
    vcdGen.addSignal("nine", 1)
    vcdGen.addSignal("ten", 1)
    vcdGen.addSignal("eleven", 1)

    vcdGen.definitionCreatorNew()

    vcdGen.addValToStack("one", 1)
    vcdGen.addValToStack("two", 1)
    vcdGen.addValToStack("three", 1)
    vcdGen.addValToStack("five", 1)

    vcdGen.timeStep()

    vcdGen.addValToStack("one", 1) // value change
    vcdGen.addValToStack("two", 0) // value change
    vcdGen.addValToStack("four", 1) // new entry

    vcdGen.timeStep()

    vcdGen.footer()
  }
}
