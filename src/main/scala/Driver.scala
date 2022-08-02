package essent

import scala.io.Source
import scala.sys.process._
import logger._


object Driver {
  def main(args: Array[String]): Unit = {
    (new ArgsParser).getConfig(args) match {
      case Some(config) => generate(config)
      case None =>
    }
  }

  def generate(opt: OptFlags): Unit = {
    Logger.setClassLogLevels(Map("essent" -> logger.LogLevel(opt.essentLogLevel)))
    Logger.setClassLogLevels(Map("firrtl" -> logger.LogLevel(opt.firrtlLogLevel)))
    val sourceReader = Source.fromFile(opt.firInputFile)
    val circuit = firrtl.Parser.parse(sourceReader.getLines, firrtl.Parser.IgnoreInfo)
    sourceReader.close()
    if (opt.java) {
      val compiler = new JavaCompiler(opt)
      compiler.compileAndEmit(circuit)
    } else {
      val compiler = new EssentCompiler(opt)
      compiler.compileAndEmit(circuit)
    }
  }

  /** Work in progress */
  def generateTester(source: String, optimize: String): os.Path = {
    val opt =
      if (optimize == "O2") OptFlags(
        java=true,
        useCondParts=false)
      else if (optimize == "O1") OptFlags(
        java=true,
        conditionalMuxes = false,
        useCondParts=false)
      else if (optimize == "O0") OptFlags(
        java=true,
        removeFlatConnects = false,
        regUpdates = false,
        conditionalMuxes = false,
        useCondParts=false)
      else OptFlags(java=true)
    Logger.setClassLogLevels(Map("essent" -> logger.LogLevel(opt.essentLogLevel)))
    Logger.setClassLogLevels(Map("firrtl" -> logger.LogLevel(opt.firrtlLogLevel)))
    val sourceReader = Source.fromString(source)
    val circuit = firrtl.Parser.parse(sourceReader.getLines, firrtl.Parser.IgnoreInfo)
    sourceReader.close()
    val compiler = new JavaCompiler(opt)
    compiler.compileAndEmitTemp(circuit)
  }

  def compileCPP(dutName: String, buildDir: String): ProcessBuilder = {
    Seq("g++", "-O3", "-std=c++11", s"-I$buildDir", s"-Ifirrtl-sig",
      s"$buildDir/$dutName-harness.cc", "-o", s"$buildDir/$dutName")
  }
}
