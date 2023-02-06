package essent

import os.list

import scala.collection.mutable.ListBuffer

import java.io._
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.util.Random

object VCD {

  var to_be_written = ListBuffer[String]()
  var filename = "test.vcd"
  var last_signal = List[Product]()
  var time = 0
  var signal_symbol = List[String]()

  def setFile(f: String = "test.vcd"): Unit = {
    filename = f
  }

  def writeHeader(v: String = "0.1", t: String = "1ns"): Unit={
    val curr_time = LocalDateTime.now
    val version = v
    val timescale = t

    val header = s"""$$version $version $$end
      |$$date $curr_time $$end
      |$$timescale $timescale $$end""".stripMargin

    addToStack(header)
    writeVCD()
  }

  def addToStack(s: String): Unit = {
    to_be_written.append(s)
  }

  def writeVCD(): Unit = {
    val to_be_written_copy = to_be_written.clone()
    to_be_written.clear()
    writeFile(filename, to_be_written_copy)
  }

  def signalChange(signalChanges: List[Product]) {
//    val signalNames = signal_symbol.productIterator.toList
    for (signalChange <- signalChanges) {
      var timeChanged = false
      var changedValues = ""

      for (i <- 0 until signalChange.productArity) {
        if (last_signal.isEmpty || signalChange.productElement(i) != last_signal.head.productElement(i)) {
          if(!timeChanged){
            changedValues += s"#$time "
            timeChanged = true
          }
          changedValues += s"${signalChange.productElement(i)}${signal_symbol(i)} "
        }
      }
      if(!changedValues.equals("")){
        addToStack(changedValues)
      }
    }
    time += 1
    last_signal = signalChanges
  }

  def writeFile(filename: String, lines: Seq[String]): Unit = {
    val nonEmptyLines = lines.filter(_.trim.nonEmpty)
    if(nonEmptyLines.isEmpty) {
      return
    }
    val pw = new PrintWriter(new FileOutputStream(new File(filename), true))
    try {
      nonEmptyLines.foreach(pw.println)
    } finally {
      pw.close()
    }
  }

  def writeFile(filename: String, s: String): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(s)
    bw.close()
  }

  def definitionCreator(signal: Product, signalBits: Product): Unit = {

    def generateUniqueChar(n: Int, existingChars: List[String]): String = {
      val alphabet = "!#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~"
      val randomChars = Random.shuffle(alphabet.toList).take(n).mkString
      if (existingChars.contains(randomChars)) {
        generateUniqueChar(n+1, existingChars)
      } else {
        randomChars
      }
    }

    var signal_reference_names = List[String]()
    for (i<-0 until signal.productArity){
      signal_reference_names = signal_reference_names :+ generateUniqueChar(1, signal_reference_names)
    }

    signal_symbol = signal_reference_names

    addToStack("$scope module top $end")
    for (i <- 0 until signal.productArity) {
      addToStack(s"$$var wire ${signalBits.productElement(i)} ${signal_symbol(i)} ${signal.productElement(i)} $$end")
    }
    addToStack("$upscope $end")
    addToStack("$enddefinitions $end")
    addToStack("$dumpvars")
    writeVCD()
  }

  def valueDumper(signalChanges: List[Product]): Unit = {
    signalChange(signalChanges)
  }

  def footer(): Unit = {
    addToStack(s"$$end")
    writeVCD()
  }
}