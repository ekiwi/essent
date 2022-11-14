package essent

import java.io.{File, FileWriter, Writer}

import essent.JavaEmitter._
import essent.Extract._
import essent.ir._
import essent.Util._
import firrtl._
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.stage.TransformManager.TransformDependency
import firrtl.stage.{FirrtlCircuitAnnotation, FirrtlStage, RunFirrtlTransformAnnotation, transforms}

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

  // Writing To File
  //----------------------------------------------------------------------------
  def writeLines(indentLevel: Int, lines: String): Unit = writeLines(indentLevel, Seq(lines))

  def writeLines(indentLevel: Int, lines: Seq[String]): Unit = {
    lines.foreach { s => writer.write(tabs * indentLevel + s + "\n") }
  }


  // Declaring Modules
  //----------------------------------------------------------------------------
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
    val memInstantiation = memories filter(x => genJavaType(x.dataType) == "BigInteger") map { m: DefMemory => {
      s"Arrays.fill(${m.name}, BigInteger.ZERO);"
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
      writeLines(2, memInstantiation)
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
      writeLines(2, memInstantiation)
      writeLines(1, "}")
      writeLines(0, "}")
      writeLines(0, "")
    }
  }

  def declareExtModule(m: ExtModule): Unit = {
    val modName = m.name
    writeLines(0, "")
    writeLines(0, s"class $modName {")
    writeLines(1, m.ports flatMap emitPort(topLevel = true))
    writeLines(1, s"public ${m.name}() {")
    writeLines(1, "}")
    writeLines(0, s"} $modName;")
  }


  // Write General-purpose Eval
  //----------------------------------------------------------------------------
  def writeBodyInner(indentLevel: Int, sg: StatementGraph, opt: OptFlags,
                     keepAvail: Set[String] = Set()): Unit = {
    if (opt.conditionalMuxes)
      MakeCondMux(sg, rn, keepAvail)
    val noMoreMuxOpts = opt.copy(conditionalMuxes = false)
    sg.stmtsOrdered foreach {
      case cm: CondMux =>
        if (rn.nameToMeta(cm.name).decType == MuxOut)
          writeLines(indentLevel, s"${genJavaType(cm.mux.tpe)} ${rn.emit(cm.name)};")
        val muxCond = emitExpr(cm.mux.cond)
        writeLines(indentLevel, s"if ($muxCond) {")
        writeBodyInner(indentLevel + 1, StatementGraph(cm.tWay), noMoreMuxOpts)
        writeLines(indentLevel, "} else {")
        writeBodyInner(indentLevel + 1, StatementGraph(cm.fWay), noMoreMuxOpts)
        writeLines(indentLevel, "}")
      case stmt =>
        writeLines(indentLevel, emitStmt(stmt))
    }
  }

  def writePeek(circuit: Circuit) : Unit = {
    writeLines(1, " @Override public BigInteger peek(String var) {")
    writeLines(2, "switch (var) {")
    val modules = findAllModuleInstances(circuit: Circuit)
    for ((modName, fullName) <- modules) {
      val m = findModule(modName, circuit)
      m match {
        case m : Module =>
          val registers = findInstancesOf[DefRegister](m.body)
          val registerDecs = registers flatMap {
            d: DefRegister => Seq(s"""case "$fullName${d.name}": return ${asBigInt(Reference(d), fullName)};""")
          }
          val portDecs = m.ports flatMap {
            p: Port => p.tpe match {
              case ClockType => Seq()
              case _ =>
                if (m.name != circuit.main) Seq()
                else Seq(s"""case "$fullName${p.name}": return ${asBigInt(Reference(p), fullName)};""")
            }
          }
          writeLines(3, registerDecs)
          writeLines(3, portDecs)
        case m : ExtModule =>
        // NOT IMPLEMENTED
      }
    }
    writeLines(3, "default: return null;")
    writeLines(2, "}")
    writeLines(1, "}")
  }

  def writePoke(circuit: Circuit) : Unit = {
    writeLines(1, " @Override public void poke(String var, BigInteger val) {")
    writeLines(2, "switch (var) {")
    val modules = findAllModuleInstances(circuit: Circuit)
    for ((modName, fullName) <- modules) {
      val m = findModule(modName, circuit)
      m match {
        case m: Module =>
          val registers = findInstancesOf[DefRegister](m.body)
          val registerDecs = registers flatMap {
            d: DefRegister => Seq(s"""case "$fullName${d.name}": $fullName${d.name} = ${fromBigInt(Reference(d), "val")}; return;""")
          }
          val portDecs = m.ports flatMap {
            p: Port =>
              p.tpe match {
                case ClockType => Seq()
                case _ =>
                  if (m.name != circuit.main) Seq()
                  else Seq(s"""case "$fullName${p.name}": $fullName${p.name} = ${fromBigInt(Reference(p), "val")}; return;""")
              }
          }
          writeLines(3, registerDecs)
          writeLines(3, portDecs)
        case m: ExtModule =>
        // NOT IMPLEMENTED
      }
    }
    writeLines(3, "default: return;")
    writeLines(2, "}")
    writeLines(1, "}")
  }

  // Write Zoning Optimized Eval
  //----------------------------------------------------------------------------
  def genEvalFuncName(partID: Int): String = "EVAL_" + partID

  def genDepPartTriggers(consumerIDs: Seq[Int], condition: String): Seq[String] = {
    consumerIDs.sorted map { consumerID => s"$flagVarName[$consumerID] |= $condition;" }
  }

  def genAllTriggers(signalNames: Seq[String], outputConsumers: Map[String, Seq[Int]],
                     suffix: String): Seq[String] = {
    selectFromMap(signalNames, outputConsumers).toSeq flatMap { case (name, consumerIDs) =>
      genDepPartTriggers(consumerIDs, s"${rn.emit(name)} != ${rn.emit(name + suffix)}")
    }
  }

  def writeZoningPredecs(sg: StatementGraph, condPartWorker: MakeCondPart, topName: String,
                         extIOtypes: Map[String, Type], opt: OptFlags): Unit = {
    // predeclare part outputs
    val outputPairs = condPartWorker.getPartOutputsToDeclare()
    val outputConsumers = condPartWorker.getPartInputMap()
    writeLines(1, outputPairs map {case (name, tpe) => s"${genJavaType(tpe)} ${rn.emit(name)};"})
    val extIOCacheDecs = condPartWorker.getExternalPartInputTypes(extIOtypes) map {
      case (name, tpe) => s"${genJavaType(tpe)} ${rn.emit(name + condPartWorker.cacheSuffix)};"
    }
    writeLines(1, extIOCacheDecs)
    writeLines(1, s"boolean[] $flagVarName = new boolean[${condPartWorker.getNumParts()}];")
    // FUTURE: worry about namespace collisions with user variables
    writeLines(1, s"boolean sim_cached = false;")
    writeLines(1, s"boolean regs_set = false;")
    writeLines(1, s"boolean update_registers;")
    writeLines(1, s"boolean done_reset;")
    writeLines(1, s"boolean verbose;")
    writeLines(0, "")
    sg.stmtsOrdered foreach {
      case cp: CondPart =>
        writeLines(1, s"private void ${genEvalFuncName(cp.id)}() {")
        if (!cp.alwaysActive)
          writeLines(2, s"$flagVarName[${cp.id}] = false;")
        if (opt.trackParts)
          writeLines(2, s"$actVarName[${cp.id}] += 1;")

        val cacheOldOutputs = cp.outputsToDeclare.toSeq map {
          case (name, tpe) =>
            s"${genJavaType(tpe)} ${rn.emit(name + condPartWorker.cacheSuffix)} = ${rn.emit(name)};"
        }
        writeLines(2, cacheOldOutputs)
        val (regUpdates, noRegUpdates) = partitionByType[RegUpdate](cp.memberStmts)
        val keepAvail = cp.outputsToDeclare.keySet
        writeBodyInner(2, StatementGraph(noRegUpdates), opt, keepAvail)
        writeLines(2, genAllTriggers(cp.outputsToDeclare.keys.toSeq, outputConsumers, condPartWorker.cacheSuffix))
        val regUpdateNamesInPart = regUpdates flatMap findResultName
        writeLines(2, genAllTriggers(regUpdateNamesInPart, outputConsumers, "$next"))
        writeLines(2, regUpdates flatMap emitStmt)
        // triggers for MemWrites
        val memWritesInPart = cp.memberStmts collect { case mw: MemWrite => mw }
        val memWriteTriggers = memWritesInPart flatMap { mw => {
          val condition = s"${emitExprWrap(mw.wrEn)} && ${emitExprWrap(mw.wrMask)}"
          genDepPartTriggers(outputConsumers.getOrElse(mw.memName, Seq()), condition)
        }
        }
        writeLines(2, memWriteTriggers)
        writeLines(1, "}")
      case stmt => throw new Exception(s"Statement at top-level is not a CondPart (${stmt.serialize})")
    }
    writeLines(0, "")
  }

  def writeZoningBody(sg: StatementGraph, condPartWorker: MakeCondPart, opt: OptFlags): Unit = {
    writeLines(2, "if (reset || !done_reset) {")
    writeLines(3, "sim_cached = false;")
    writeLines(3, "regs_set = false;")
    writeLines(2, "}")
    writeLines(2, "if (!sim_cached) {")
    writeLines(3, s"Arrays.fill($flagVarName, true);")
    writeLines(2, "}")
    writeLines(2, "sim_cached = regs_set;")
    writeLines(2, "this.update_registers = update_registers;")
    writeLines(2, "this.done_reset = done_reset;")
    writeLines(2, "this.verbose = verbose;")
    val outputConsumers = condPartWorker.getPartInputMap()
    val externalPartInputNames = condPartWorker.getExternalPartInputNames()
    // do activity detection on other inputs (external IOs and resets)
    writeLines(2, genAllTriggers(externalPartInputNames, outputConsumers, condPartWorker.cacheSuffix))
    // cache old versions
    val extIOCaches = externalPartInputNames map {
      sigName => s"${rn.emit(sigName + condPartWorker.cacheSuffix)} = ${rn.emit(sigName)};"
    }
    writeLines(2, extIOCaches)
    sg.stmtsOrdered foreach {
      case cp: CondPart =>
        if (!cp.alwaysActive)
          writeLines(2, s"if ($flagVarName[${cp.id}]) ${genEvalFuncName(cp.id)}();")
        else
          writeLines(2, s"${genEvalFuncName(cp.id)}();")
      case stmt => writeLines(2, emitStmt(stmt))
    }
//    writeLines(2, "if (!update_registers) {")
//    writeLines(3, "Arrays.fill(PARTflags, true);")
//    writeLines(2, "}")
    writeLines(2, "regs_set = true;")
  }

  def execute(circuit: Circuit): Unit = {
    val topName = circuit.main
    writeLines(0, "import java.math.BigInteger;")
    writeLines(0, "import essent.Simulator;")
    writeLines(0, "import java.util.Arrays;")
    writeLines(0, "")
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

    circuit.modules foreach {
      case m: Module => declareModule(m, topName)
      case m: ExtModule => declareExtModule(m)
    }
    val topModule = findModule(topName, circuit) match {case m: Module => m}
    if (containsAsserts) {
      writeLines(1, "boolean assert_triggered = false;")
      writeLines(1, "int assert_exit_code;")
      writeLines(0, "")
    }
    if (opt.useCondParts) {
      writeZoningPredecs(sg, condPartWorker, circuit.main, extIOMap, opt)
    }
    writeLines(1, "public void eval(boolean update_registers, boolean verbose, boolean done_reset) {")
    if (opt.trackParts || opt.trackSigs)
      writeLines(2, "cycle_count++;")
    if (opt.useCondParts)
      writeZoningBody(sg, condPartWorker, opt)
    else
      writeBodyInner(2, sg, opt)
    if (containsAsserts)
      writeLines(2, "if (done_reset && update_registers && assert_triggered) {System.out.println(assert_exit_code); System.exit(assert_exit_code);}")
    writeLines(1, "}")
    writeLines(0, "")
    writeLines(1, JavaEmitter.cache)
    writeLines(0, "")
    writePeek(circuit)
    writeLines(0, "")
    writePoke(circuit)
    writeLines(0, "")
    writeLines(1, "@Override public void step(boolean update_registers) {")
    writeLines(2, "eval(update_registers, true, true);")
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