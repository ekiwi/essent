package essent

import java.io.{File, FileWriter, Writer}

import essent.JavaEmitter._
import essent.Extract._
import essent.ir._
import firrtl._
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.stage.TransformManager.TransformDependency
import firrtl.stage.transforms

import logger._

class EssentJavaEmitter(opt: OptFlags, writer: Writer) extends LazyLogging {
  val tabs = "  "
  val flagVarName = "PARTflags"
  val actVarName = "ACTcounts"
  val sigTrackName = "SIGcounts"
  val sigActName = "SIGact"
  val sigExtName = "SIGext"
  var sigNameToID: Map[String, Int] = Map[String, Int]()

  implicit val rn: Renamer = new Renamer

  def writeLines(indentLevel: Int, lines: String): Unit = writeLines(indentLevel, Seq(lines))

  def writeLines(indentLevel: Int, lines: Seq[String]): Unit = {
    lines.foreach { s => writer.write(tabs * indentLevel + s + "\n") }
  }

  def declareModule(m: Module, topName: String): Unit = {
    val registers = findInstancesOf[DefRegister](m.body)
    val memories = findInstancesOf[DefMemory](m.body)
    val registerDecs = registers flatMap { d: DefRegister => {
      val typeStr = "public " + genJavaType(d.tpe)
      val regName = d.name
      if (typeStr.contains("BigInteger")) Seq(s"$typeStr $regName = BigInteger.valueOf(0);")
      else if (typeStr.contains("boolean")) Seq(s"$typeStr $regName = false;")
      else Seq(s"$typeStr $regName = 0L;")
    }}
    val memDecs = memories map { m: DefMemory => {
      s"public ${genJavaType(m.dataType)} ${m.name}[${m.depth}];"
    }}
    val modulesAndPrefixes = findModuleInstances(m.body)
    val moduleDecs = modulesAndPrefixes map { case (module, fullName) =>
      val instanceName = fullName.split("\\.").last
      s"$module $instanceName;"
    }
    val moduleDecs2 = modulesAndPrefixes map { case (module, fullName) =>
      val instanceName = fullName.split("\\.").last
      s"$instanceName = new $module();"
    }

    if (m.name == topName) {
      writeLines(0, s"public class ${m.name} extends Simulator {")
      writeLines(1, registerDecs)
      writeLines(1, memDecs)
      writeLines(1, m.ports flatMap emitPort(topLevel = true))
      writeLines(1, moduleDecs)
      writeLines(1, s"public ${m.name}() {")
      writeLines(2, moduleDecs2)
      writeLines(1, "}")
    }
    else {
      writeLines(0, s"class ${m.name} {")
      writeLines(1, registerDecs)
      writeLines(1, memDecs)
      writeLines(1, m.ports flatMap emitPort(topLevel = false))
      writeLines(1, moduleDecs)
      writeLines(1, s"public ${m.name}() {")
      writeLines(2, moduleDecs2)
      writeLines(1, "}")
      writeLines(0, "}")
      writeLines(0, "")
    }
  }

  def writeBodyInner(indentLevel: Int, sg: StatementGraph, opt: OptFlags,
                     keepAvail: Set[String] = Set()): Unit = {
    if (opt.conditionalMuxes)
      MakeCondMux(sg, rn, keepAvail)
    val noMoreMuxOpts = opt.copy(conditionalMuxes = false)
    sg.stmtsOrdered foreach {
      case cm: CondMux =>
        if (rn.nameToMeta(cm.name).decType == MuxOut)
          writeLines(indentLevel, s"${genJavaType(cm.mux.tpe)} ${rn.emit(cm.name)};")
        val muxCondRaw = emitExpr(cm.mux.cond)
        val muxCond = if (muxCondRaw == "reset") s"UNLIKELY($muxCondRaw)" else muxCondRaw
        writeLines(indentLevel, s"if ($muxCond) {")
        writeBodyInner(indentLevel + 1, StatementGraph(cm.tWay), noMoreMuxOpts)
        writeLines(indentLevel, "} else {")
        writeBodyInner(indentLevel + 1, StatementGraph(cm.fWay), noMoreMuxOpts)
        writeLines(indentLevel, "}")
      case stmt =>
        writeLines(indentLevel, emitStmt(stmt))
        if (opt.trackSigs) emitSigTracker(stmt, indentLevel, opt)
    }
  }

  def writePeek(m: Module, topName: String) : Unit = {
    if (m.name != topName) return
    val registers = findInstancesOf[DefRegister](m.body)
    val registerDecs = registers flatMap { d: DefRegister => {
      Seq(s"""case "${d.name}": return ${asBigInt(Reference(d))};""")
    }}
    val modulesAndPrefixes = findModuleInstances(m.body)
    val moduleDecs = modulesAndPrefixes map { case (module, fullName) =>
    val instanceName = fullName.split("\\.").last
    s"$module $instanceName;"
  }
    writeLines(2, "switch (var) {")
    val modName = m.name
    writeLines(3, registerDecs)
    writeLines(3, m.ports flatMap returnName(modName == topName))
    writeLines(3, "default: return null;")
    writeLines(2, "}")
  }

  def returnName(topLevel: Boolean)(p: Port): Seq[String] = p.tpe match {
    case ClockType => Seq()
    case _ => if (!topLevel) Seq()
    else {
      Seq(s"""case "${p.name}": return ${asBigInt(Reference(p))};""")
    }
  }

  def setName(topLevel: Boolean)(p: Port): Seq[String] = p.tpe match {
    case ClockType => Seq()
    case _ => if (!topLevel) Seq()
    else {
      Seq(s"""case "${p.name}": ${p.name} = ${fromBigInt(p.tpe, "val")}; return;""")
    }
  }

  def writePoke(m: Module, topName: String) : Unit = {
    if (m.name != topName) return
    val registers = findInstancesOf[DefRegister](m.body)
    val registerDecs = registers flatMap { d: DefRegister => {
      Seq(s"""case "${d.name}": ${d.name} = ${fromBigInt(d.tpe, "val")}; return;""")
    }}
    val modulesAndPrefixes = findModuleInstances(m.body)
    val moduleDecs = modulesAndPrefixes map { case (module, fullName) =>
      val instanceName = fullName.split("\\.").last
      s"$module $instanceName;"
    }
    writeLines(2, "switch (var) {")
    val modName = m.name
    writeLines(3, registerDecs)
    writeLines(3, m.ports flatMap setName(modName == topName))
    writeLines(2, "}")
  }

  def emitSigTracker(stmt: Statement, indentLevel: Int, opt: OptFlags): Unit = {
    stmt match {
      case mw: MemWrite =>
      case _ =>
        val resultName = findResultName(stmt)
        resultName match {
          case Some(name) =>
            val cleanName = name.replace('.', '$')
            writeLines(indentLevel, s"$sigTrackName[${sigNameToID(name)}] += $name != old::$cleanName ? 1 : 0;")
            if (opt.trackExts) {
              writeLines(indentLevel, s"$sigActName[${sigNameToID(name)}] = $name != old::$cleanName;")
              val depNames = findDependencesStmt(stmt).head.deps
              val trackedDepNames = depNames filter sigNameToID.contains
              val depTrackers = trackedDepNames map { name => s"$sigActName[${sigNameToID(name)}]" }
              val anyDepActive = depTrackers.mkString(" || ")
              if (anyDepActive.nonEmpty)
                writeLines(indentLevel, s"$sigExtName[${sigNameToID(name)}] += !$sigActName[${sigNameToID(name)}] && ($anyDepActive) ? 1 : 0;")
            }
            writeLines(indentLevel, s"old::$cleanName = $name;")
          case None =>
        }
    }
  }

  def execute(circuit: Circuit): Unit = {
    val topName = circuit.main
    val sg = StatementGraph(circuit, opt.removeFlatConnects)

    logger.info(sg.makeStatsString())
    val extIOMap = findExternalPorts(circuit)
    val condPartWorker = MakeCondPart(sg, rn, extIOMap)
    rn.populateFromSG(sg, extIOMap)
    if (opt.useCondParts) {
      condPartWorker.doOpt(opt.partCutoff)
    } else {
      if (opt.regUpdates)
        OptElideRegUpdates(sg)
    }

    writeLines(0, "import java.math.BigInteger;")
    writeLines(0, "import essent.Simulator;")
    writeLines(0, "")

    circuit.modules foreach {
      case m: Module => declareModule(m, topName)
    }

    writeLines(1, s"public void eval(boolean update_registers, boolean verbose, boolean done_reset) {")
    writeBodyInner(2, sg, opt)
    writeLines(1, "}")
    writeLines(0, "")
    writeLines(1, " @Override public BigInteger peek(String var) {")
    circuit.modules foreach {
      case m: Module => writePeek(m, topName)
    }
    writeLines(1, "}")
    writeLines(0, "")
    writeLines(1, " @Override public void poke(String var, BigInteger val) {")
    circuit.modules foreach {
      case m: Module => writePoke(m, topName)
    }
    writeLines(1, "}")
    writeLines(0, "")
    writeLines(1, "@Override public void step(boolean update_registers) {")
    writeLines(2, "eval(update_registers, false, false);")
    writeLines(1, "}")
    writeLines(0, "}")
  }
}

class JavaCompiler(opt: OptFlags) {
  val readyForEssent: Seq[TransformDependency] =
    firrtl.stage.Forms.LowFormOptimized ++
      Seq(
        Dependency(essent.passes.ReplaceAsyncRegs),
        Dependency(essent.passes.NoClockConnects),
        Dependency(essent.passes.RegFromMem1),
        Dependency(essent.passes.FactorMemReads),
        Dependency(essent.passes.FactorMemWrites),
        Dependency(essent.passes.SplitRegUpdates),
        Dependency(essent.passes.FixMulResultWidth),
        Dependency(essent.passes.DistinctTypeInstNames),
        Dependency(essent.passes.RemoveAsAsyncReset),
        Dependency(essent.passes.ReplaceRsvdKeywords)
      )

  def compileAndEmit(circuit: Circuit): Unit = {
    val topName = circuit.main
    val firrtlCompiler = new transforms.Compiler(readyForEssent)
    val resultState = firrtlCompiler.execute(CircuitState(circuit, Seq()))
    val dutWriter = new FileWriter(new File(opt.outputDir(), s"$topName.java"))
    val emitter = new EssentJavaEmitter(opt, dutWriter)
    emitter.execute(resultState.circuit)
    dutWriter.close()
  }
}