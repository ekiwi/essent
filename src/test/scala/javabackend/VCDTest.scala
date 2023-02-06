package javabackend

import essent.VCD
import org.scalatest.freespec.AnyFreeSpec
import scala.util.Random

class VCDTest extends AnyFreeSpec{

  "writeHeader" in {
    val vcd_gen = VCD;
    vcd_gen.setFile("vcd_files/writeHeader.vcd");
    vcd_gen.writeHeader();
  }

  "writeSignals" in {
    def generateRandomTuples(length: Int): List[(Int, Int, Int)] = {
      val random = new Random()
      (1 to length).map(_ => (random.nextInt(2), random.nextInt(2), random.nextInt(2))).toList
    }

    val vcd_gen = VCD;
    vcd_gen.setFile("vcd_files/writeSignals.vcd")
    vcd_gen.writeHeader();

    val signal = ("first", "second", "third")
    val signal_nickname = ("a", "b", "c")
    val signal_bits = (1, 1, 1)

    vcd_gen.addToStack("$scope module top $end")
    for (i <- 0 until signal_nickname.productArity) {
      vcd_gen.addToStack(s"$$var wire ${signal_bits.productElement(i)} ${signal_nickname.productElement(i)} ${signal.productElement(i)} $$end")
    }

    vcd_gen.addToStack("$upscope $end")
    vcd_gen.addToStack("$enddefinitions $end")
    vcd_gen.addToStack("$dumpvars")

    val signalChanges = generateRandomTuples(10000)
    vcd_gen.signalChange(signalChanges)

    vcd_gen.addToStack(s"$$end")
    vcd_gen.writeVCD()

  }

  "final" in {
    def generateRandomTuples(length: Int): List[(Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int)] = {
      val random = new Random()
      (1 to length).map(_ => (random.nextInt(2), random.nextInt(2), random.nextInt(2), random.nextInt(2), random.nextInt(2), random.nextInt(2), random.nextInt(2), random.nextInt(2), random.nextInt(2), random.nextInt(2), random.nextInt(2))).toList
    }

    val filename = "vcd_files/final.vcd"
    val signal = ("FIRST", "second", "third", "sd", "Asdasd", "Asdkasopd", "adkopsakda", "asdoaisdjas", "ajdsaidj", "asdkoasdjas", "Adkoasjdsao")
//    val signal_nickname = ("a", "b", "c") //TODO auto generated unique nicknames
    val signal_bits = (1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) // length
    val signalChanges = generateRandomTuples(10000)

    val vcd_gen = VCD;
    vcd_gen.setFile(filename)
    vcd_gen.writeHeader();
    vcd_gen.definitionCreator(signal = signal, signalBits = signal_bits)

    for(signal <- signalChanges){
      vcd_gen.valueDumper(List(signal))
    }

    vcd_gen.footer()


  }
}
