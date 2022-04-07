package essent

import java.io.{File, FileWriter, Writer}
import essent.JavaEmitter._
import essent.Extract._
import essent.Util.selectFromMap
import essent.ir._
import firrtl._
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.stage.TransformManager.TransformDependency
import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlStage, RunFirrtlTransformAnnotation, transforms}
import _root_.logger._

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
      s"public ${genJavaType(m.dataType)}[] ${m.name} = new ${genJavaType(m.dataType)}[${m.depth}];"
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

  def genEvalFuncName(partID: Int): String = "EVAL_" + partID

  def genDepPartTriggers(consumerIDs: Seq[Int], condition: String): Seq[String] = {
    consumerIDs.sorted map { consumerID => s"$flagVarName[$consumerID] |= $condition;" }
  }

  def genAllTriggers(signalNames: Seq[String], outputConsumers: Map[String, Seq[Int]],
                     suffix: String): Seq[String] = {
    selectFromMap(signalNames, outputConsumers).toSeq flatMap { case (name, consumerIDs) => {
      genDepPartTriggers(consumerIDs, s"${rn.emit(name)} != ${rn.emit(name + suffix)}")
    }}
  }

  // TODO
  def writeZoningBody(sg: StatementGraph, condPartWorker: MakeCondPart, opt: OptFlags) {
    writeLines(2, "if (reset || !done_reset) {")
    writeLines(3, "sim_cached = false;")
    writeLines(3, "regs_set = false;")
    writeLines(2, "}")
    writeLines(2, "if (!sim_cached) {")
    writeLines(3, s"$flagVarName.fill(true);")
    writeLines(2, "}")
    writeLines(2, "sim_cached = regs_set;")
    writeLines(2, "this->update_registers = update_registers;")
    writeLines(2, "this->done_reset = done_reset;")
    writeLines(2, "this->verbose = verbose;")
    val outputConsumers = condPartWorker.getPartInputMap()
    val externalPartInputNames = condPartWorker.getExternalPartInputNames()
    // do activity detection on other inputs (external IOs and resets)
    writeLines(2, genAllTriggers(externalPartInputNames, outputConsumers, condPartWorker.cacheSuffix))
    // cache old versions
    val extIOCaches = externalPartInputNames map {
      sigName => s"${rn.emit(sigName + condPartWorker.cacheSuffix)} = ${rn.emit(sigName)};"
    }
    writeLines(2, extIOCaches.toSeq)
    sg.stmtsOrdered foreach { stmt => stmt match {
      case cp: CondPart => {
        if (!cp.alwaysActive)
          writeLines(2, s"if ($flagVarName[${cp.id}]) ${genEvalFuncName(cp.id)}();")
        else
          writeLines(2, s"${genEvalFuncName(cp.id)}();")
      }
      case _ => writeLines(2, emitStmt(stmt))
    }}
    // writeLines(2,  "#ifdef ALL_ON")
    // writeLines(2, s"$flagVarName.fill(true);" )
    // writeLines(2,  "#endif")
    writeLines(2, "regs_set = true;")
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

    writeLines(0, "import java.math.BigInteger;")
    writeLines(0, "import essent.Simulator;")
    writeLines(0, "")

    circuit.modules foreach {
      case m: Module => declareModule(m, topName)
    }

    val topModule = findModule(topName, circuit) match {case m: Module => m}
    if (opt.writeHarness) {
      writeLines(0, "")
      writeLines(1, s"void connect_harness(CommWrapper<struct $topName> *comm) {")
      writeLines(2, HarnessGenerator.harnessConnections(topModule))
      writeLines(1, "}")
      writeLines(0, "")
    }
    if (containsAsserts) {
      writeLines(1, "boolean assert_triggered = false;")
      writeLines(1, "int assert_exit_code;")
      writeLines(0, "")
    }
    if (opt.useCondParts) {
      // TODO writeZoningPredecs(sg, condPartWorker, circuit.main, extIOMap, opt)
    }
    writeLines(1, "public void eval(boolean update_registers, boolean verbose, boolean done_reset) {")
    if (opt.trackParts || opt.trackSigs)
      writeLines(2, "cycle_count++;")
    if (opt.useCondParts)
      writeZoningBody(sg, condPartWorker, opt)
    else
      writeBodyInner(2, sg, opt)
    if (containsAsserts)
      writeLines(2, "if (done_reset && update_registers && assert_triggered) System.exit(assert_exit_code);")
    // TODO writeRegResetOverrides(sg)
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
      Seq(
        Dependency(firrtl.passes.memlib.VerilogMemDelays),
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
    val firrtlCompiler = new FirrtlStage
    val args = Array("-ll", "error", "-E", "low-opt")
    val annos = Seq(FirrtlCircuitAnnotation(circuit)) ++ readyForEssent.map(RunFirrtlTransformAnnotation(_))
    val resultState = firrtlCompiler.execute(args, annos)
    val dutWriter = new FileWriter(new File(opt.outputDir(), s"$topName.java"))
    val emitter = new EssentJavaEmitter(opt, dutWriter)
    emitter.execute(resultState.collectFirst{case FirrtlCircuitAnnotation(c) => c}.get)
    dutWriter.close()
  }

  def compileAndEmitTemp(circuit: Circuit): os.Path = {
    val topName = circuit.main
    val firrtlCompiler = new FirrtlStage
    val tempPath = os.temp.dir()
    val args = Array("-ll", "error", "-E", "low-opt")
    val annos = Seq(FirrtlCircuitAnnotation(circuit), firrtl.options.TargetDirAnnotation(tempPath.toString)) ++ readyForEssent.map(RunFirrtlTransformAnnotation(_))
    val resultState = firrtlCompiler.execute(args, annos)
    val dutWriter = new FileWriter(new File(tempPath.toString, s"$topName.java"))
    val emitter = new EssentJavaEmitter(opt, dutWriter)
    emitter.execute(resultState.collectFirst{case FirrtlCircuitAnnotation(c) => c}.get)
    dutWriter.close()
    tempPath / s"$topName.java"
  }
}