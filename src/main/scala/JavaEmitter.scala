package essent

import essent.Emitter.{splatLargeLiteralIntoRawArray}
import essent.Extract.{findDependencesStmt, findExternalPorts, findInstancesOf, findModule, findModuleInstances, findResultName, logger}
import essent.ir.{CondMux, MemWrite, RegUpdate}
import firrtl.PrimOps.{Add, And, Andr, AsAsyncReset, AsClock, AsSInt, AsUInt, Bits, Cat, Cvt, Div, Dshl, Dshr, Eq, Geq, Gt, Head, Leq, Lt, Mul, Neg, Neq, Not, Or, Orr, Pad, Rem, Shl, Shr, Sub, Tail, Xor, Xorr}
import firrtl.{Addw, Dshlw, Subw, WDefInstance, WRef, WSubAccess, WSubField, bitWidth}
import firrtl.ir.{AsyncResetType, Block, Circuit, ClockType, Connect, DefMemory, DefNode, DefRegister, DefWire, DoPrim, Expression, IntWidth, Module, Mux, Port, Print, SIntLiteral, SIntType, Statement, Stop, Type, UIntLiteral, UIntType}

import java.io.Writer
import _root_.logger._

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

  def genPublicJavaType(tpe: Type) = tpe match {
    case UIntType(IntWidth(w)) => if (w == 1) "public boolean" else "public int"
    case SIntType(IntWidth(w)) => if (w == 1) "public boolean" else "public int"
    case AsyncResetType => "public boolean"
    case _ => throw new Exception(s"No Java type implemented for $tpe")
  }

  def genJavaType(tpe: Type) = tpe match {
    case UIntType(IntWidth(w)) => if (w == 1) "boolean" else "int"
    case SIntType(IntWidth(w)) => if (w == 1) "boolean" else "int"
    case AsyncResetType => "boolean"
    case _ => throw new Exception(s"No Java type implemented for $tpe")
  }

  def emitPort(topLevel: Boolean)(p: Port): Seq[String] = p.tpe match {
    case ClockType => if (!topLevel) Seq()
    else Seq(genPublicJavaType(UIntType(IntWidth(1))) + " " + p.name + " = false;")
    // FUTURE: suppress generation of clock field if not making harness (or used)?
    case _ => if (!topLevel) Seq()
    else
      if (p.tpe == UIntType(IntWidth(1)))
        Seq(genPublicJavaType(p.tpe) + " " + p.name + " = false;")
      else
        Seq(genPublicJavaType(p.tpe) + " " + p.name + " = 0;")
  }

  def declareModule(m: Module, topName: String) {
    val registers = findInstancesOf[DefRegister](m.body)
    val memories = findInstancesOf[DefMemory](m.body)
    val registerDecs = registers flatMap {d: DefRegister => {
      val typeStr = genPublicJavaType(d.tpe)
      val regName = d.name
      Seq(s"$typeStr $regName = 0;")
    }}
    val memDecs = memories map {m: DefMemory => {
      s"${genPublicJavaType(m.dataType)} ${m.name}[${m.depth}];"
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

  def emitExpr(e: Expression)(implicit rn: Renamer = null): String = e match {
    case w: WRef => if (rn != null) rn.emit(w.name) else w.name
    case u: UIntLiteral => {
      val maxIn64Bits = (BigInt(1) << 64) - 1
      val width = bitWidth(u.tpe)
      val asDecStr = u.value.toString(10)
      if ((width <= 64) || (u.value <= maxIn64Bits)) {
          s"$asDecStr"
      }
      else s"(${splatLargeLiteralIntoRawArray(u.value, width)})"
    }
    case u: SIntLiteral => {
      val width = bitWidth(u.tpe)
      if ((width <= 64)) {
          s"${u.value.toString(10)}"
      }
      else s"(${splatLargeLiteralIntoRawArray(u.value, width)})"
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
      case Not => s"!${emitExprWrap(p.args.head)}"
      case And => p.args map emitExprWrap mkString(" & ")
      case Or => p.args map emitExprWrap mkString(" | ")
      case Xor => p.args map emitExprWrap mkString(" ^ ")
      case Andr => s"${emitExprWrap(p.args.head)}.andr()"
      case Orr => s"${emitExprWrap(p.args.head)}.orr()"
      case Xorr => s"${emitExprWrap(p.args.head)}.xorr()"
      case Cat => s"${emitExprWrap(p.args(0))}.cat(${emitExpr(p.args(1))})"
      case Bits => s"${emitExprWrap(p.args.head)}.bits<${p.consts(0).toInt},${p.consts(1).toInt}>()"
      case Head => s"${emitExprWrap(p.args.head)}.head<${p.consts.head.toInt}>()"
//      case Tail => s"${emitExprWrap(p.args.head)}.tail<${p.consts.head.toInt}>()"
      case Tail => s"${emitExprWrap(p.args.head)} & 0xffff"
    }
    case _ => throw new Exception(s"Don't yet support $e")
  }

  def emitStmt(s: Statement)(implicit rn: Renamer): Seq[String] = s match {
    case b: Block => b.stmts flatMap emitStmt
    case d: DefNode => {
      val lhs_orig = d.name
      val lhs = rn.emit(lhs_orig)
      val rhs = emitExpr(d.value)
      if (rn.decLocal(lhs_orig)) Seq(s"${genJavaType(d.value.tpe)} $lhs = $rhs;")
      else Seq(s"$lhs = $rhs;")
    }
    case c: Connect => {
      val lhs_orig = emitExpr(c.loc)(null)
      val lhs = rn.emit(lhs_orig)
      val rhs = emitExpr(c.expr)
      if (rn.decLocal(lhs_orig)) Seq(s"${genJavaType(c.loc.tpe)} $lhs = $rhs;")
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

  def emitExprWrap(e: Expression)(implicit rn: Renamer): String = e match {
    case DoPrim(_,_,_,_) | Mux(_,_,_,_) => s"(${emitExpr(e)})"
    case _ => emitExpr(e)
  }
}