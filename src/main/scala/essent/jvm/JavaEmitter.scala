package essent.jvm

import essent.Extract.{findExternalPorts, findInstancesOf, findModuleInstances}
import firrtl._
import firrtl.ir._
import essent.ir._
import essent.{MakeCondPart, OptElideRegUpdates, OptFlags, Renamer, StatementGraph}

class JavaEmitter(initialOpt: OptFlags, filename: os.Path) {
  private val out = new java.io.PrintStream(os.write.outputStream(filename))
  private def writeLines(indentLevel: Int, lines: String): Unit = writeLines(indentLevel, Seq(lines))
  private def writeLines(indentLevel: Int, lines: Seq[String]): Unit =
    lines.foreach(l => out.println(("  " * indentLevel) + l))

  implicit val rn = new Renamer

  import JavaExpressionCompiler._

  private def emitPort(topLevel: Boolean)(p: Port): Seq[String] = p.tpe match {
    case ClockType =>
      if (!topLevel) { Seq() } else { Seq(javaTpe(UIntType(IntWidth(1))) + " " + p.name + ";") }
    // FUTURE: suppress generation of clock field if not making harness (or used)?
    case _ => if (!topLevel) { Seq() } else { Seq(javaTpe(p.tpe) + " " + p.name + ";") }
  }

  private def initializeVals(topLevel: Boolean)(m: Module, registers: Seq[DefRegister], memories: Seq[DefMemory]) = {
    def initVal(name: String, tpe: Type) = s"$name = ${random(tpe)};"
    val regInits = registers.map(r => initVal(r.name, r.tpe))
    val memInits = memories flatMap { m =>
      if ((m.depth > 1000) && bitWidth(m.dataType) <= 64) {
        Seq(s"${m.name}[0] = ${random(m.dataType)};",
          s"for (size_t a=0; a < ${m.depth}; a++) ${m.name}[a] = ${m.name}[0] + a;")
      } else
        Seq(s"for (size_t a=0; a < ${m.depth}; a++) ${m.name}[a].rand_init();")
    }
    val portInits = m.ports.flatMap { p => p.tpe match {
      case ClockType => Seq()
      case _ => if (!topLevel) Seq()
      else Seq(initVal(p.name, p.tpe))
    }}
    regInits ++ memInits ++ portInits
  }

  // Declaring Modules
  //----------------------------------------------------------------------------
  private def declareModule(m: Module, topName: String) {
    val registers = findInstancesOf[DefRegister](m.body)
    val memories = findInstancesOf[DefMemory](m.body)
    val registerDecs = registers.flatMap{ d =>
      val typeStr = javaTpe(d.tpe)
      val regName = d.name
      Seq(s"$typeStr $regName;")
    }
    val memDecs = memories.map( m => s"${javaTpe(m.dataType)} ${m.name}[];")
    val modulesAndPrefixes = findModuleInstances(m.body)
    val moduleDecs = modulesAndPrefixes.map { case (module, fullName) => {
      val instanceName = fullName.split("\\.").last
      s"$module $instanceName;"
    }}
    val modName = m.name
    writeLines(0, "")
    writeLines(0, s"public class $modName {")
    writeLines(1, registerDecs)
    writeLines(1, memDecs)
    writeLines(1, m.ports.flatMap(emitPort(modName == topName)))
    writeLines(1, moduleDecs)
    writeLines(0, "")
    writeLines(1, s"$modName() {")
    writeLines(2, initializeVals(modName == topName)(m, registers, memories))
    writeLines(1, "}")
    if (modName == topName) {
      writeLines(0, "")
      // writeLines(1, s"void connect_harness(CommWrapper<struct $modName> *comm);")
    } else {
      writeLines(0, s"} $modName;")
    }
  }

  private def declareExtModule(m: ExtModule) {
    val modName = m.name
    writeLines(0, "")
    writeLines(0, s"public class $modName {")
    writeLines(1, m.ports.flatMap(emitPort(true)))
    writeLines(0, s"}")
  }


  def execute(circuit: ir.Circuit): Unit = {
    val opt = initialOpt

    // header
    imports()

    // magic ?
    val sg = StatementGraph(circuit, opt.removeFlatConnects)
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

    out.close()
  }

  private def imports(): Unit = {
    // currently there are no imports
  }
}


/*
import java.io.Writer

object JavaEmitter {
  case class HyperedgeDep(name: String, deps: Seq[String], stmt: Statement)

  // Type Declaration & Initialization
  //----------------------------------------------------------------------------
  def genJavaType(tpe: Type): String = {
    val bits = bitWidth(tpe)
    if(bits < 8) "byte"
    else if(bits < 64) "long"
    else "BigInteger"
  }

  def initializeVals(topLevel: Boolean)(m: Module, registers: Seq[DefRegister], memories: Seq[DefMemory]) = {
    def initVal(name: String, tpe:Type) = s"rand_init($name, ${bitWidth(tpe)});"
    val regInits = registers map {
      r: DefRegister => initVal(r.name, r.tpe)
    }
    val memInits = memories flatMap { m: DefMemory => {
      if ((m.depth > 1000) && (bitWidth(m.dataType)) <= 64) {
        Seq(s"${m.name}[0].rand_init();",
          s"for (size_t a=0; a < ${m.depth}; a++) ${m.name}[a] = ${m.name}[0].as_single_word() + a;")
      } else
        Seq(s"for (size_t a=0; a < ${m.depth}; a++) ${m.name}[a].rand_init();")
    }}
    val portInits = m.ports flatMap { p => p.tpe match {
      case ClockType => Seq()
      case _ => if (!topLevel) Seq()
      else Seq(initVal(p.name, p.tpe))
    }}
    regInits ++ memInits ++ portInits
  }


  // Prefixing & Replacement
  //----------------------------------------------------------------------------
  def addPrefixToNameStmt(prefix: String)(s: Statement): Statement = {
    val replaced = s match {
      case n: DefNode => n.copy(name = prefix + n.name)
      case r: DefRegister => r.copy(name = prefix + r.name)
      case m: DefMemory => m.copy(name = prefix + m.name)
      case w: DefWire => w.copy(name = prefix + w.name)
      case mw: MemWrite => mw.copy(memName = prefix + mw.memName)
      case _ => s
    }
    replaced.mapStmt(addPrefixToNameStmt(prefix)).mapExpr(addPrefixToNameExpr(prefix))
  }

  def addPrefixToNameExpr(prefix: String)(e: Expression): Expression = {
    val replaced = e match {
      case w: Reference => w.copy(name = prefix + w.name)
      case _ => e
    }
    replaced.mapExpr(addPrefixToNameExpr(prefix))
  }

  def replaceNamesStmt(renames: Map[String, String])(s: Statement): Statement = {
    val nodeReplaced = s match {
      case n: DefNode if (renames.contains(n.name)) => n.copy(name = renames(n.name))
      case cm: CondMux if (renames.contains(cm.name)) => cm.copy(name = renames(cm.name))
      case mw: MemWrite if (renames.contains(mw.memName)) => mw.copy(memName = renames(mw.memName))
      case _ => s
    }
    nodeReplaced.mapStmt(replaceNamesStmt(renames)).mapExpr(replaceNamesExpr(renames))
  }

  def replaceNamesExpr(renames: Map[String, String])(e: Expression): Expression = {
    def findRootKind(e: Expression): Kind = e match {
      case w: Reference => w.kind
      case w: SubField => findRootKind(w.expr)
    }
    e match {
      case w: Reference => {
        if (renames.contains(w.name)) w.copy(name = renames(w.name))
        else w
      }
      case w: SubField => {
        val fullName = emitExpr(w)
        // flattens out nested WSubFields
        if (renames.contains(fullName)) WRef(renames(fullName), w.tpe, findRootKind(w), w.flow)
        else w
      }
      case _ => e.mapExpr(replaceNamesExpr(renames))
    }
  }


  // Emission
  //----------------------------------------------------------------------------
  def emitPort(topLevel: Boolean)(p: Port): Seq[String] = p.tpe match {
    case ClockType => if (!topLevel) Seq()
    else Seq(genJavaType(UIntType(IntWidth(1))) + " " + p.name + ";")
    // FUTURE: suppress generation of clock field if not making harness (or used)?
    case _ => if (!topLevel) Seq()
    else Seq(genJavaType(p.tpe) + " " + p.name + ";")
  }

  def chunkLitString(litStr: String, chunkWidth:Int = 16): Seq[String] = {
    if (litStr.size < chunkWidth) Seq(litStr)
    else chunkLitString(litStr.dropRight(chunkWidth)) ++ Seq(litStr.takeRight(chunkWidth))
  }

  // NOTE: assuming no large UIntLiteral is negative
  def splatLargeLiteralIntoRawArray(value: BigInt, width: BigInt): String = {
    val rawHexStr = value.toString(16)
    val isNeg = value < 0
    val asHexStr = if (isNeg) rawHexStr.tail else rawHexStr
    val arrStr = chunkLitString(asHexStr) map { "0x" + _} mkString(",")
    val leadingNegStr = if (isNeg) "(uint64_t) -" else ""
    val numWords = (width + 63) / 64
    s"std::array<uint64_t,$numWords>({$leadingNegStr$arrStr})"
  }

  def emitExpr(e: Expression)(implicit rn: Renamer = null): String = e match {
    case w: Reference => if (rn != null) rn.emit(w.name) else w.name
    case u: UIntLiteral => {
      val maxIn64Bits = (BigInt(1) << 64) - 1
      val width = bitWidth(u.tpe)
      val asHexStr = u.value.toString(16)
      if ((width <= 64) || (u.value <= maxIn64Bits)) s"UInt<$width>(0x$asHexStr)"
      else s"UInt<$width>(${splatLargeLiteralIntoRawArray(u.value, width)})"
    }
    case u: SIntLiteral => {
      val width = bitWidth(u.tpe)
      if (width <= 64) s"SInt<$width>(${u.value.toString(10)})"
      else s"SInt<$width>(${splatLargeLiteralIntoRawArray(u.value, width)})"
    }
    case m: Mux => {
      val condName = emitExprWrap(m.cond)
      val tvalName = emitExprWrap(m.tval)
      val fvalName = emitExprWrap(m.fval)
      s"$condName ? $tvalName : $fvalName"
    }
    case w: WSubField => {
      val result = s"${emitExpr(w.expr)(null)}.${w.name}"
      if (rn != null)
        rn.emit(result)
      else
        result
    }
    case w: WSubAccess => s"${emitExpr(w.expr)}[${emitExprWrap(w.index)}.as_single_word()]"
    case p: DoPrim => p.op match {
      case Add => p.args map emitExprWrap mkString(" + ")
      case Addw => s"${emitExprWrap(p.args(0))}.addw(${emitExprWrap(p.args(1))})"
      case Sub => p.args map emitExprWrap mkString(" - ")
      case Subw => s"${emitExprWrap(p.args(0))}.subw(${emitExprWrap(p.args(1))})"
      case Mul => p.args map emitExprWrap mkString(" * ")
      case Div => p.args map emitExprWrap mkString(" / ")
      case Rem => p.args map emitExprWrap mkString(" % ")
      case Lt  => p.args map emitExprWrap mkString(" < ")
      case Leq => p.args map emitExprWrap mkString(" <= ")
      case Gt  => p.args map emitExprWrap mkString(" > ")
      case Geq => p.args map emitExprWrap mkString(" >= ")
      case Eq => p.args map emitExprWrap mkString(" == ")
      case Neq => p.args map emitExprWrap mkString(" != ")
      case Pad => s"${emitExprWrap(p.args.head)}.pad<${bitWidth(p.tpe)}>()"
      case AsUInt => s"${emitExprWrap(p.args.head)}.asUInt()"
      case AsSInt => s"${emitExprWrap(p.args.head)}.asSInt()"
      case AsClock => throw new Exception("AsClock unimplemented!")
      case AsAsyncReset => emitExpr(p.args.head) // TODO: make async
      case Shl => s"${emitExprWrap(p.args.head)}.shl<${p.consts.head.toInt}>()"
      // case Shlw => s"${emitExprWrap(p.args.head)}.shlw<${p.consts.head.toInt}>()"
      case Shr => s"${emitExprWrap(p.args.head)}.shr<${p.consts.head.toInt}>()"
      case Dshl => p.args map emitExprWrap mkString(" << ")
      case Dshlw => s"${emitExprWrap(p.args(0))}.dshlw(${emitExpr(p.args(1))})"
      case Dshr => p.args map emitExprWrap mkString(" >> ")
      case Cvt => s"${emitExprWrap(p.args.head)}.cvt()"
      case Neg => s"-${emitExprWrap(p.args.head)}"
      case Not => s"~${emitExprWrap(p.args.head)}"
      case And => p.args map emitExprWrap mkString(" & ")
      case Or => p.args map emitExprWrap mkString(" | ")
      case Xor => p.args map emitExprWrap mkString(" ^ ")
      case Andr => s"${emitExprWrap(p.args.head)}.andr()"
      case Orr => s"${emitExprWrap(p.args.head)}.orr()"
      case Xorr => s"${emitExprWrap(p.args.head)}.xorr()"
      case Cat => s"${emitExprWrap(p.args(0))}.cat(${emitExpr(p.args(1))})"
      case Bits => s"${emitExprWrap(p.args.head)}.bits<${p.consts(0).toInt},${p.consts(1).toInt}>()"
      case Head => s"${emitExprWrap(p.args.head)}.head<${p.consts.head.toInt}>()"
      case Tail => s"${emitExprWrap(p.args.head)}.tail<${p.consts.head.toInt}>()"
    }
    case _ => throw new Exception(s"Don't yet support $e")
  }

  def emitExprWrap(e: Expression)(implicit rn: Renamer): String = e match {
    case DoPrim(_,_,_,_) | Mux(_,_,_,_) => s"(${emitExpr(e)})"
    case _ => emitExpr(e)
  }

  def emitStmt(s: Statement)(implicit rn: Renamer): Seq[String] = s match {
    case b: Block => b.stmts flatMap emitStmt
    case d: DefNode => {
      val lhs_orig = d.name
      val lhs = rn.emit(lhs_orig)
      val rhs = emitExpr(d.value)
      if (rn.decLocal(lhs_orig)) Seq(s"${genCppType(d.value.tpe)} $lhs = $rhs;")
      else Seq(s"$lhs = $rhs;")
    }
    case c: Connect => {
      val lhs_orig = emitExpr(c.loc)(null)
      val lhs = rn.emit(lhs_orig)
      val rhs = emitExpr(c.expr)
      if (rn.decLocal(lhs_orig)) Seq(s"${genCppType(c.loc.tpe)} $lhs = $rhs;")
      else Seq(s"$lhs = $rhs;")
    }
    case p: Print => {
      val formatters = "(%h)|(%x)|(%d)|(%ld)".r.findAllIn(p.string.serialize).toList
      val argWidths = p.args map {e: Expression => bitWidth(e.tpe)}
      if (!(argWidths forall { _ <= 64 })) throw new Exception(s"Can't print wide signals")
      val replacements = formatters zip argWidths map { case(format, width) =>
        if (format == "%h" || format == "%x") {
          val printWidth = math.ceil(width.toDouble/4).toInt
          (format, s"""%0${printWidth}" PRIx64 """")
        } else {
          val printWidth = math.ceil(math.log10((1l<<width.toInt).toDouble)).toInt
          (format, s"""%${printWidth}" PRIu64 """")
        }
      }
      val formatString = replacements.foldLeft(p.string.serialize){
        case (str, (searchFor, replaceWith)) => str.replaceFirst(searchFor, replaceWith)
      }
      val printfArgs = Seq(s""""$formatString"""") ++
        (p.args map {arg => s"${emitExprWrap(arg)}.as_single_word()"})
      Seq(s"if (UNLIKELY(done_reset && update_registers && verbose && ${emitExprWrap(p.en)})) printf(${printfArgs mkString(", ")});")
    }
    case st: Stop => {
      Seq(s"if (UNLIKELY(${emitExpr(st.en)})) {assert_triggered = true; assert_exit_code = ${st.ret};}")
    }
    case mw: MemWrite => {
      Seq(s"if (update_registers && ${emitExprWrap(mw.wrEn)} && ${emitExprWrap(mw.wrMask)}) ${mw.memName}[${emitExprWrap(mw.wrAddr)}.as_single_word()] = ${emitExpr(mw.wrData)};")
    }
    case ru: RegUpdate => Seq(s"if (update_registers) ${emitExpr(ru.regRef)} = ${emitExpr(ru.expr)};")
    case r: DefRegister => Seq()
    case w: DefWire => Seq()
    case m: DefMemory => Seq()
    case i: WDefInstance => Seq()
    case _ => throw new Exception(s"Don't yet support $s")
  }
}


class JavaEmitter(initialOpt: OptFlags, writer: Writer) {
  import JavaEmitter._

  private val tabs = "  "
  private val flagVarName = "PARTflags"
  private val actVarName = "ACTcounts"
  private val sigTrackName = "SIGcounts"
  private val sigActName = "SIGact"
  private val sigExtName = "SIGext"
  private var sigNameToID = Map[String,Int]()

  private implicit val rn = new Renamer

  // Writing To File
  //----------------------------------------------------------------------------
  private def writeLines(indentLevel: Int, lines: String) {
    writeLines(indentLevel, Seq(lines))
  }

  private def writeLines(indentLevel: Int, lines: Seq[String]) {
    lines.foreach { s => writer.write(tabs*indentLevel + s + "\n") }
  }


  // Declaring Modules
  //----------------------------------------------------------------------------
  def declareModule(m: Module, topName: String) {
    val registers = findInstancesOf[DefRegister](m.body)
    val memories = findInstancesOf[DefMemory](m.body)
    val registerDecs = registers flatMap {d: DefRegister => {
      val typeStr = genCppType(d.tpe)
      val regName = d.name
      Seq(s"$typeStr $regName;")
    }}
    val memDecs = memories map {m: DefMemory => {
      s"${genCppType(m.dataType)} ${m.name}[${m.depth}];"
    }}
    val modulesAndPrefixes = findModuleInstances(m.body)
    val moduleDecs = modulesAndPrefixes map { case (module, fullName) => {
      val instanceName = fullName.split("\\.").last
      s"$module $instanceName;"
    }}
    val modName = m.name
    writeLines(0, "")
    writeLines(0, s"typedef struct $modName {")
    writeLines(1, registerDecs)
    writeLines(1, memDecs)
    writeLines(1, m.ports flatMap emitPort(modName == topName))
    writeLines(1, moduleDecs)
    writeLines(0, "")
    writeLines(1, s"$modName() {")
    writeLines(2, initializeVals(modName == topName)(m, registers, memories))
    writeLines(1, "}")
    if (modName == topName) {
      writeLines(0, "")
      // writeLines(1, s"void connect_harness(CommWrapper<struct $modName> *comm);")
    } else {
      writeLines(0, s"} $modName;")
    }
  }

  def declareExtModule(m: ExtModule) {
    val modName = m.name
    writeLines(0, "")
    writeLines(0, s"typedef struct $modName {")
    writeLines(1, m.ports flatMap emitPort(true))
    writeLines(0, s"} $modName;")
  }


  // Write General-purpose Eval
  //----------------------------------------------------------------------------
  // TODO: move specialized CondMux emitter elsewhere?
  def writeBodyInner(indentLevel: Int, sg: StatementGraph, opt: OptFlags,
    keepAvail: Set[String] = Set()) {
    // ng.stmtsOrdered foreach { stmt => writeLines(indentLevel, emitStmt(stmt)) }
    if (opt.conditionalMuxes)
      MakeCondMux(sg, rn, keepAvail)
    val noMoreMuxOpts = opt.copy(conditionalMuxes = false)
    sg.stmtsOrdered foreach { stmt => stmt match {
      case cm: CondMux => {
        if (rn.nameToMeta(cm.name).decType == MuxOut)
          writeLines(indentLevel, s"${genCppType(cm.mux.tpe)} ${rn.emit(cm.name)};")
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

  def writeRegResetOverrides(sg: StatementGraph) {
    val updatesWithResets = sg.allRegDefs filter { r => emitExpr(r.reset) != "UInt<1>(0x0)" }
    assert(updatesWithResets.isEmpty)
    //    val resetGroups = updatesWithResets.groupBy(r => emitExpr(r.reset))
    //    val overridesToWrite = resetGroups.toSeq flatMap {
    //      case (resetName, regDefs) => {
    //        val body = regDefs map {
    //          r => s"$tabs${rn.emit(r.name)} = ${emitExpr(r.init)};"
    //        }
    //        Seq(s"if ($resetName) {") ++ body ++ Seq("}")
    //      }
    //    }
    //    if (overridesToWrite.nonEmpty) {
    //      writeLines(2, "if (update_registers) {")
    //      // FUTURE: will overrides need triggers if partitioned?
    //      writeLines(3, overridesToWrite)
    //      writeLines(2, "}")
    //    }
  }


  // Write Zoning Optimized Eval
  //----------------------------------------------------------------------------
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

  def writeZoningPredecs(
    sg: StatementGraph,
    condPartWorker: MakeCondPart,
    topName: String,
    extIOtypes: Map[String, Type],
    opt: OptFlags) {
    // predeclare part outputs
    val outputPairs = condPartWorker.getPartOutputsToDeclare()
    val outputConsumers = condPartWorker.getPartInputMap()
    writeLines(1, outputPairs map {case (name, tpe) => s"${genCppType(tpe)} ${rn.emit(name)};"})
    val extIOCacheDecs = condPartWorker.getExternalPartInputTypes(extIOtypes) map {
      case (name, tpe) => s"${genCppType(tpe)} ${rn.emit(name + condPartWorker.cacheSuffix)};"
    }
    writeLines(1, extIOCacheDecs)
    writeLines(1, s"std::array<bool,${condPartWorker.getNumParts()}> $flagVarName;")
    // FUTURE: worry about namespace collisions with user variables
    writeLines(1, s"bool sim_cached = false;")
    writeLines(1, s"bool regs_set = false;")
    writeLines(1, s"bool update_registers;")
    writeLines(1, s"bool done_reset;")
    writeLines(1, s"bool verbose;")
    writeLines(0, "")
    sg.stmtsOrdered foreach { stmt => stmt match {
      case cp: CondPart => {
        writeLines(1, s"void ${genEvalFuncName(cp.id)}() {")
        if (!cp.alwaysActive)
          writeLines(2, s"$flagVarName[${cp.id}] = false;")
        if (opt.trackParts)
          writeLines(2, s"$actVarName[${cp.id}]++;")
        val cacheOldOutputs = cp.outputsToDeclare.toSeq map {
          case (name, tpe) => { s"${genCppType(tpe)} ${rn.emit(name + condPartWorker.cacheSuffix)} = ${rn.emit(name)};"
          }}
        writeLines(2, cacheOldOutputs)
        val (regUpdates, noRegUpdates) = partitionByType[RegUpdate](cp.memberStmts)
        val keepAvail = (cp.outputsToDeclare map { _._1 }).toSet
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
        }}
        writeLines(2, memWriteTriggers)
        writeLines(1, "}")
      }
      case _ => throw new Exception(s"Statement at top-level is not a CondPart (${stmt.serialize})")
    }}
    writeLines(0, "")
  }

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


  def declareSigTracking(sg: StatementGraph, topName: String, opt: OptFlags) {
    val allNamesAndTypes = sg.collectValidStmts(sg.nodeRange) flatMap findStmtNameAndType
    sigNameToID = (allNamesAndTypes map { _._1 }).zipWithIndex.toMap
    writeLines(0, "")
    writeLines(0, s"std::array<uint64_t,${sigNameToID.size}> $sigTrackName{};")
    if (opt.trackExts) {
      writeLines(0, s"std::array<bool,${sigNameToID.size}> $sigActName{};")
      writeLines(0, s"std::array<uint64_t,${sigNameToID.size}> $sigExtName{};")
    }
    writeLines(0, "namespace old {")
    writeLines(1, allNamesAndTypes map {
      case (name, tpe) => s"${genCppType(tpe)} ${name.replace('.','$')};"
    })
    writeLines(0, "}")
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

  def emitJsonWriter(opt: OptFlags, numParts: Int) {
    writeLines(0, "void writeActToJson() {")
    writeLines(1, "std::fstream file(\"activities.json\", std::ios::out | std::ios::binary);")
    writeLines(1, "JSON all_data;")
    if (opt.trackSigs) {
      writeLines(1, "JSON sig_acts;")
      writeLines(1, s"for (int i=0; i<${sigNameToID.size}; i++) {")
      writeLines(2, s"""sig_acts[i] = JSON({"id", i, "acts", $sigTrackName[i]});""")
      writeLines(1, "}")
      writeLines(1, "all_data[\"signal-activities\"] = sig_acts;")
    }
    if (opt.trackParts) {
      writeLines(1, "JSON part_acts;")
      writeLines(1, s"for (int i=0; i<$numParts; i++) {")
      writeLines(2, s"""part_acts[i] = JSON({"id", i, "acts", $actVarName[i]});""")
      writeLines(1, "}")
      writeLines(1, "all_data[\"part-activities\"] = part_acts;")
    }
    if (opt.trackExts) {
      writeLines(1, "JSON sig_exts;")
      writeLines(1, s"for (int i=0; i<${sigNameToID.size}; i++) {")
      writeLines(2, s"""sig_exts[i] = JSON({"id", i, "exts", $sigExtName[i]});""")
      writeLines(1, "}")
      writeLines(1, "all_data[\"sig-extinguishes\"] = sig_exts;")
    }
    writeLines(1, "all_data[\"cycles\"] = cycle_count;")
    writeLines(1, "file << all_data << std::endl;")
    writeLines(1, "file.close();")
    writeLines(0, "}")
  }


  // General Structure (and Compiler Boilerplate)
  //----------------------------------------------------------------------------
  def execute(circuit: Circuit) {
    val opt = initialOpt
    val topName = circuit.main
    val headerGuardName = topName.toUpperCase + "_H_"
    writeLines(0, s"#ifndef $headerGuardName")
    writeLines(0, s"#define $headerGuardName")
    writeLines(0, "")
    writeLines(0, "#include <array>")
    writeLines(0, "#include <cstdint>")
    writeLines(0, "#include <cstdlib>")
    writeLines(0, "#include <uint.h>")
    writeLines(0, "#include <sint.h>")
    writeLines(0, "#define UNLIKELY(condition) __builtin_expect(static_cast<bool>(condition), 0)")
    if (opt.trackParts || opt.trackSigs) {
      writeLines(0, "#include <fstream>")
      writeLines(0, "#include \"../SimpleJSON/json.hpp\"")
      writeLines(0, "using json::JSON;")
      writeLines(0, "uint64_t cycle_count = 0;")
    }
    val sg = StatementGraph(circuit, opt.removeFlatConnects)
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
    // if (opt.trackSigs)
    //   declareSigTracking(sg, topName, opt)
    // if (opt.trackParts)
    //   writeLines(1, s"std::array<uint64_t,${sg.getNumParts()}> $actVarName{};")
    // if (opt.trackParts || opt.trackSigs)
    //   emitJsonWriter(opt, condPartWorker.getNumParts())
    // if (opt.partStats)
    //   sg.dumpPartInfoToJson(opt, sigNameToID)
    // if (opt.trackExts)
    //   sg.dumpNodeTypeToJson(sigNameToID)
    // sg.reachableAfter(sigNameToID)
    circuit.modules foreach {
      case m: Module => declareModule(m, topName)
      case m: ExtModule => declareExtModule(m)
    }
    val topModule = findModule(topName, circuit) match {case m: Module => m}
    if (initialOpt.writeHarness) {
      writeLines(0, "")
      writeLines(1, s"void connect_harness(CommWrapper<struct $topName> *comm) {")
      writeLines(2, HarnessGenerator.harnessConnections(topModule))
      writeLines(1, "}")
      writeLines(0, "")
    }
    if (containsAsserts) {
      writeLines(1, "bool assert_triggered = false;")
      writeLines(1, "int assert_exit_code;")
      writeLines(0, "")
    }
    if (opt.useCondParts)
      writeZoningPredecs(sg, condPartWorker, circuit.main, extIOMap, opt)
    writeLines(1, s"void eval(bool update_registers, bool verbose, bool done_reset) {")
    if (opt.trackParts || opt.trackSigs)
      writeLines(2, "cycle_count++;")
    if (opt.useCondParts)
      writeZoningBody(sg, condPartWorker, opt)
    else
      writeBodyInner(2, sg, opt)
    if (containsAsserts)
      writeLines(2, "if (done_reset && update_registers && assert_triggered) exit(assert_exit_code);")
    writeRegResetOverrides(sg)
    writeLines(1, "}")
    // if (opt.trackParts || opt.trackSigs) {
    //   writeLines(1, s"~$topName() {")
    //   writeLines(2, "writeActToJson();")
    //   writeLines(1, "}")
    // }
    writeLines(0, s"} $topName;") //closing top module dec
    writeLines(0, "")
    writeLines(0, s"#endif  // $headerGuardName")
  }
}
*/