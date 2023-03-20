package essent

import scala.collection.mutable.ListBuffer
import java.io._
import java.time.LocalDateTime

object VCD {

  var toBeWritten = ListBuffer[String]()
  var filename = "test.vcd"
  var time = 0
  var allSignals = List.empty[(String, Int, String)]
  var currSignalNum = 0
  var currSignalVals = List.empty[(String, Int)]
  var prevSignalVals = List.empty[(String, Int)]

  def setFile(f: String = "test.vcd"): Unit = {
    filename = f
  }

  def writeHeader(v: String = "0.1", t: String = "1ns"): Unit = {
    val currTime = LocalDateTime.now
    val version = v
    val timescale = t

    val header = s"""$$version $version $$end
      |$$date $currTime $$end
      |$$timescale $timescale $$end""".stripMargin

    addToStack(header)
    writeVCD()
  }

  def addToStack(s: String): Unit = {
    toBeWritten.append(s)
  }

  def writeVCD(): Unit = {
    val toBeWrittenCopy = toBeWritten.clone()
    toBeWritten.clear()
    writeFile(filename, toBeWrittenCopy)
  }

  def writeFile(filename: String, lines: Seq[String]): Unit = {
    val nonEmptyLines = lines.filter(_.trim.nonEmpty)
    if (nonEmptyLines.isEmpty) {
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

  def footer(): Unit = {
    addToStack(s"$$end")
    writeVCD()
  }

  def nextCombination(value: BigInt): String = {
    val alphabet =
      "!#$%&'()*+,-./:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{|}~"
    if (value < 82) {
      alphabet(value.toInt).toString
    } else {
      val digit = nextCombination(value % 82)
      val upper = nextCombination(value / 82)
      upper + digit
    }
  }

  def addSignal(signalName: String, signalWidth: Int): Unit = {
    allSignals =
      allSignals :+ (signalName, signalWidth, nextCombination(currSignalNum))
    currSignalNum = currSignalNum + 1
  }

  def definitionCreatorNew(): Unit = {
    for ((signalName, signalWidth, signalNick) <- allSignals) {
      addToStack(s"$$var wire $signalWidth $signalNick $signalName $$end")
    }
    addToStack("$upscope $end")
    addToStack("$enddefinitions $end")
    addToStack("$dumpvars")
    writeVCD()
  }

  def signalDumper(): Unit = {
    var changedValues = List.empty[(String, Int)]
    for ((signalName, signalValue) <- currSignalVals) {
      val signalSymbol =
        allSignals.find(_._1 == signalName).map(_._3).get.toString
      if (prevSignalVals.exists { case (value, _) => value == signalSymbol }) {
        if (prevSignalVals.exists(_ == (signalSymbol, signalValue))) {
          // Nothing since value hasn't changed
        } else {
          val updatedList = prevSignalVals.map {
            case (name, value) if name == signalSymbol => (name, signalValue)
            case other                                 => other
          }
          prevSignalVals = updatedList
          changedValues = changedValues :+ (signalSymbol, signalValue)
        }
      } else {
        prevSignalVals = prevSignalVals :+ (signalSymbol, signalValue)
        changedValues = changedValues :+ (signalSymbol, signalValue)
      }
    }
    if (changedValues.nonEmpty) {
      var changedString = s"#${time}"
      for ((signalSymbol, signalValue) <- changedValues) {
        changedString += s" ${signalValue}${signalSymbol}"
      }
      addToStack(changedString)
      writeVCD()
    }
    currSignalVals = List.empty[(String, Int)]
  }

  def timeStep(): Unit = {
    signalDumper()
    time = time + 1
  }

  def addValToStack(name: String, value: Int): Unit = {
    currSignalVals = currSignalVals :+ (name, value)
  }

}
