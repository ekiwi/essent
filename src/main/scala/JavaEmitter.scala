package essent

import essent.Emitter.{emitExpr, emitStmt}
import essent.Extract.{findDependencesStmt, findExternalPorts, findInstancesOf, findModule, findModuleInstances, findResultName, logger}
import essent.ir.{CondMux, MemWrite}
import firrtl.ir.{AsyncResetType, Circuit, ClockType, DefMemory, DefRegister, IntWidth, Module, Port, SIntType, Statement, Stop, Type, UIntType}

import java.io.Writer
import logger._

class JavaEmitter(initialOpt: OptFlags, writer: Writer) extends LazyLogging {
  val tabs = "  "
  val flagVarName = "PARTflags"
  val actVarName = "ACTcounts"
  val sigTrackName = "SIGcounts"
  val sigActName = "SIGact"
  val sigExtName = "SIGext"
  var sigNameToID = Map[String,Int]()
  implicit val rn = new Renamer

  def writeLines(indentLevel: Int, lines: String): Unit = writeLines(indentLevel, Seq(lines))

  def writeLines(indentLevel: Int, lines: Seq[String]): Unit = {
    lines.foreach { s => writer.write(tabs*indentLevel + s + "\n") }
  }

  def genJavaType(tpe: Type) = tpe match {
    case UIntType(IntWidth(w)) => if (w == 1) "public boolean" else "public int"
    case SIntType(IntWidth(w)) => if (w == 1) "public boolean" else "public int"
    case AsyncResetType => "public boolean"
    case _ => throw new Exception(s"No Java type implemented for $tpe")
  }

  def emitPort(topLevel: Boolean)(p: Port): Seq[String] = p.tpe match {
    case ClockType => if (!topLevel) Seq()
    else Seq(genJavaType(UIntType(IntWidth(1))) + " " + p.name + " = false;")
    // FUTURE: suppress generation of clock field if not making harness (or used)?
    case _ => if (!topLevel) Seq()
    else
      if (p.tpe == UIntType(IntWidth(1)))
        Seq(genJavaType(p.tpe) + " " + p.name + " = false;")
      else
        Seq(genJavaType(p.tpe) + " " + p.name + " = 0;")
  }

  def declareModule(m: Module, topName: String) {
    val registers = findInstancesOf[DefRegister](m.body)
    val memories = findInstancesOf[DefMemory](m.body)
    val registerDecs = registers flatMap {d: DefRegister => {
      val typeStr = genJavaType(d.tpe)
      val regName = d.name
      Seq(s"$typeStr $regName = 0;")
    }}
    val memDecs = memories map {m: DefMemory => {
      s"${genJavaType(m.dataType)} ${m.name}[${m.depth}];"
    }}
    val modulesAndPrefixes = findModuleInstances(m.body)
    val moduleDecs = modulesAndPrefixes map { case (module, fullName) => {
      val instanceName = fullName.split("\\.").last
      s"$module $instanceName;"
    }} // Need to check what this does

    val modName = m.name
    writeLines(0, s"class ${modName} {")
    writeLines(1, registerDecs)
    writeLines(1, memDecs)
    writeLines(1, m.ports flatMap emitPort(modName == topName))
    writeLines(1, moduleDecs)
  }

  def emitSigTracker(stmt: Statement, indentLevel: Int, opt: OptFlags) {
    stmt match {
      case mw: MemWrite =>
      case _ => {
        val resultName = findResultName(stmt)
        resultName match {
          case Some(name) => {
            val cleanName = name.replace('.','$')
            writeLines(indentLevel, s"$sigTrackName[${sigNameToID(name)}] += $name != old::$cleanName ? 1 : 0;")
            if (opt.trackExts) {
              writeLines(indentLevel, s"$sigActName[${sigNameToID(name)}] = $name != old::$cleanName;")
              val depNames = findDependencesStmt(stmt).head.deps
              val trackedDepNames = depNames filter sigNameToID.contains
              val depTrackers = trackedDepNames map {name => s"$sigActName[${sigNameToID(name)}]"}
              val anyDepActive = depTrackers.mkString(" || ")
              if (anyDepActive.nonEmpty)
                writeLines(indentLevel, s"$sigExtName[${sigNameToID(name)}] += !$sigActName[${sigNameToID(name)}] && ($anyDepActive) ? 1 : 0;")
            }
            writeLines(indentLevel, s"old::$cleanName = $name;")
          }
          case None =>
        }
      }
    }
  }


  def writeBodyInner(indentLevel: Int, sg: StatementGraph, opt: OptFlags,
                     keepAvail: Set[String] = Set()): Unit = {

    if (opt.conditionalMuxes)
      MakeCondMux(sg, rn, keepAvail)
    val noMoreMuxOpts = opt.copy(conditionalMuxes = false)
    sg.stmtsOrdered foreach { stmt => stmt match {
      case cm: CondMux => {
        if (rn.nameToMeta(cm.name).decType == MuxOut)
          writeLines(indentLevel, s"${genJavaType(cm.mux.tpe)} ${rn.emit(cm.name)};")
        val muxCondRaw = emitExpr(cm.mux.cond)
        val muxCond = if (muxCondRaw == "reset") s"UNLIKELY($muxCondRaw)" else muxCondRaw
        writeLines(indentLevel, s"if ($muxCond) {")
        writeBodyInner(indentLevel + 1, StatementGraph(cm.tWay), noMoreMuxOpts)
        writeLines(indentLevel, "} else {")
        writeBodyInner(indentLevel + 1, StatementGraph(cm.fWay), noMoreMuxOpts)
        writeLines(indentLevel, "}")
      }
      case _ => {
        writeLines(indentLevel, emitStmt(stmt))
        if (opt.trackSigs) emitSigTracker(stmt, indentLevel, opt)
      }
    }}

  }

  def execute(circuit: Circuit): Unit = {
    val topName = circuit.main
    val opt = initialOpt
    val sg = StatementGraph(circuit, opt.removeFlatConnects)

    logger.info(sg.makeStatsString)
    val containsAsserts = sg.containsStmtOfType[Stop]()
    val extIOMap = findExternalPorts(circuit)
    val condPartWorker = MakeCondPart(sg, rn, extIOMap)
    rn.populateFromSG(sg, extIOMap)
    if (opt.useCondParts) {
      condPartWorker.doOpt(opt.partCutoff)
    } else {
      if (opt.regUpdates)
        OptElideRegUpdates(sg)
    }

    circuit.modules foreach {
      case m: Module => declareModule(m, topName)
    }
    val topModule = findModule(topName, circuit) match {case m: Module => m}

    writeLines(1, s"public void eval(boolean update_registers, boolean verbose, boolean done_reset) {")

    if(initialOpt.useCondParts)
      writeLines(0, "not implemented")
    else
      writeBodyInner(2, sg, opt)

    writeLines(1, "}")
    writeLines(0, "}")
  }
}