package essent

import essent.Extract._

import firrtl._
import firrtl.Annotations._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.passes.bitWidth
import firrtl.PrimOps._
import firrtl.Utils._

import util.Random

object Emitter {
  case class HyperedgeDep(name: String, deps: Seq[String], stmt: Statement)

  case class MemUpdate(memName: String, wrEnName: String, wrMaskName: String,
                       wrAddrName: String, wrDataName: String)

  // Declaration Methods
  def genCppType(tpe: Type) = tpe match {
    case UIntType(IntWidth(w)) => s"UInt<$w>"
    case SIntType(IntWidth(w)) => s"SInt<$w>"
    case _ => throw new Exception(s"No CPP type implemented for $tpe")
  }


  // Replacement methods
  def addPrefixToNameStmt(prefix: String)(s: Statement): Statement = {
    val replaced = s match {
      case n: DefNode => DefNode(n.info, prefix + n.name, n.value)
      case r: DefRegister => r.copy(name = (prefix + r.name))
      case _ => s
    }
    replaced map addPrefixToNameStmt(prefix) map addPrefixToNameExpr(prefix)
  }

  def addPrefixToNameExpr(prefix: String)(e: Expression): Expression = {
    val replaced = e match {
      case w: WRef => WRef(prefix + w.name, w.tpe, w.kind, w.gender)
      case _ => e
    }
    replaced map addPrefixToNameExpr(prefix)
  }

  def findRootKind(e: Expression): Kind = e match {
    case w: WRef => w.kind
    case w: WSubField => findRootKind(w.exp)
  }

  def replaceNamesStmt(renames: Map[String, String])(s: Statement): Statement = {
    val nodeReplaced = s match {
      case n: DefNode => {
        if (renames.contains(n.name)) DefNode(n.info, renames(n.name), n.value)
        else n
      }
      case _ => s
    }
    nodeReplaced map replaceNamesStmt(renames) map replaceNamesExpr(renames)
  }

  def replaceNamesExpr(renames: Map[String, String])(e: Expression): Expression = e match {
    case w: WRef => {
      if (renames.contains(w.name)) WRef(renames(w.name), w.tpe, w.kind, w.gender)
      else w
    }
    case w: WSubField => {
      val fullName = emitExpr(w)
      if (renames.contains(fullName)) WRef(renames(fullName), w.tpe, findRootKind(w), w.gender)
      else w
    }
    case _ => e map replaceNamesExpr(renames)
  }


  // Helper methods for building eval bodies
  def grabMemInfo(s: Statement): Seq[(String, String)] = s match {
    case b: Block => b.stmts flatMap {s: Statement => grabMemInfo(s)}
    case c: Connect => {
      firrtl.Utils.kind(c.loc) match {
        case firrtl.MemKind => Seq((emitExpr(c.loc), emitExpr(c.expr)))
        case _ => Seq()
      }
    }
    case _ => Seq()
  }

  def grabMemAddr(str: String): String = {
    if (str.contains('[')) str.slice(str.indexOf('[')+1, str.lastIndexOf(']'))
    else str
  }

  def memHasRightParams(m: DefMemory) = {
    (m.writeLatency == 1) && (m.readLatency == 0) && (m.readwriters.isEmpty)
  }


  // State initialization methods
  def makeRegisterUpdate(prefix: String)(r: DefRegister): String = {
    val regName = prefix + r.name
    val resetName = emitExpr(r.reset)
    val resetVal = r.init match {
      case l: Literal => emitExpr(r.init)
      case _ => if (resetName != "UInt<1>(0x0)")
        throw new Exception("register reset isn't a literal " + r.init)
    }
    if (resetName == "UInt<1>(0x0)") s"$regName = $regName$$next;"
    else s"$regName = $resetName ? $resetVal : $regName$$next;"
  }

  def initializeVals(topLevel: Boolean)(m: Module, registers: Seq[DefRegister], memories: Seq[DefMemory]) = {
    def initVal(name: String, tpe:Type) = s"$name.rand_init();"
    val regInits = registers map {
      r: DefRegister => initVal(r.name + "$next", r.tpe)
    }
    val memInits = memories map { m: DefMemory => {
      s"for (size_t a=0; a < ${m.depth}; a++) ${m.name}[a].rand_init();"
    }}
    val portInits = m.ports flatMap { p => p.tpe match {
      case ClockType => Seq()
      case _ => if ((p.name != "reset") && !topLevel) Seq()
                else Seq(initVal(p.name, p.tpe))
    }}
    regInits ++ memInits ++ portInits
  }


  // Emission methods
  def emitPort(topLevel: Boolean)(p: Port): Seq[String] = p.tpe match {
    case ClockType => Seq()
    case _ => if ((p.name != "reset") && !topLevel) Seq()
              else Seq(genCppType(p.tpe) + " " + p.name + ";")
  }

  def emitRegUpdate(r: DefRegister): String = {
    val regName = r.name
    val resetName = emitExpr(r.reset)
    if (resetName == "UInt<1>(0x0)") s"$regName = $regName$$next;"
    else {
      val resetVal = r.init match {
        case l: Literal => emitExpr(r.init)
        case _ => throw new Exception("register reset isn't a literal " + r.init)
      }
      s"$regName = $resetName ? $resetVal : $regName$$next;"
    }
  }

  def chunkLitString(litStr: String, chunkWidth:Int = 16): Seq[String] = {
    if (litStr.size < chunkWidth) Seq(litStr.takeRight(chunkWidth))
    else chunkLitString(litStr.dropRight(chunkWidth)) ++ Seq(litStr.takeRight(chunkWidth))
  }

  def emitExpr(e: Expression): String = e match {
    case w: WRef => w.name
    case u: UIntLiteral => {
      val maxIn64Bits = (BigInt(1) << 64) - 1
      val width = bitWidth(u.tpe)
      val asHexStr = u.value.toString(16)
      if ((width <= 64) || (u.value <= maxIn64Bits)) s"UInt<$width>(0x$asHexStr)"
      else {
        val arrStr = (chunkLitString(asHexStr) map { "0x" + _}).mkString(",")
        val numWords = (width + 63) / 64
        s"UInt<$width>(std::array<uint64_t,$numWords>({$arrStr}))"
      }
    }
    case u: SIntLiteral => {
      val width = bitWidth(u.tpe)
      if (width <= 64) s"SInt<$width>(${u.value.toString(10)})"
      else {
        val numWords = (width + 63) / 64
        if (u.value >= 0) {
          val asHexStr = u.value.toString(16)
          val arrStr = (chunkLitString(asHexStr) map { "0x" + _}).mkString(",")
          s"SInt<$width>(std::array<uint64_t,$numWords>({$arrStr}))"
        } else {
          val asNegHexStr = (-u.value).toString(16)
          val arrStr = (chunkLitString(asNegHexStr) map { "0x" + _}).mkString(",")
          s"""(-SInt<$width>(std::array<uint64_t,$numWords>({$arrStr}))).tail<1>()"""
        }
      }
    }
    case m: Mux => {
      val condName = emitExpr(m.cond)
      val tvalName = emitExpr(m.tval)
      val fvalName = emitExpr(m.fval)
      s"$condName ? $tvalName : $fvalName"
    }
    case w: WSubField => s"${emitExpr(w.exp)}.${w.name}"
    case p: DoPrim => p.op match {
      case Add => p.args map emitExpr mkString(" + ")
      case Addw => s"${emitExpr(p.args(0))}.addw(${emitExpr(p.args(1))})"
      case Sub => p.args map emitExpr mkString(" - ")
      case Subw => s"${emitExpr(p.args(0))}.subw(${emitExpr(p.args(1))})"
      case Mul => {
        val argNames = p.args map emitExpr
        val mulStr = argNames mkString(" * ")
        if (bitWidth(p.tpe) != (bitWidth(p.args(0).tpe) + bitWidth(p.args(1).tpe))) {
          val delta = (bitWidth(p.args(0).tpe) + bitWidth(p.args(1).tpe)) - bitWidth(p.tpe)
          s"($mulStr).tail<$delta>()"
        } else mulStr
      }
      case Div => p.args map emitExpr mkString(" / ")
      case Rem => p.args map emitExpr mkString(" % ")
      case Lt  => p.args map emitExpr mkString(" < ")
      case Leq => p.args map emitExpr mkString(" <= ")
      case Gt  => p.args map emitExpr mkString(" > ")
      case Geq => p.args map emitExpr mkString(" >= ")
      case Eq => p.args map emitExpr mkString(" == ")
      case Neq => p.args map emitExpr mkString(" != ")
      case Pad => s"${emitExpr(p.args.head)}.pad<${bitWidth(p.tpe)}>()"
      case AsUInt => s"${emitExpr(p.args.head)}.asUInt()"
      case AsSInt => s"${emitExpr(p.args.head)}.asSInt()"
      case AsClock => throw new Exception("AsClock unimplemented!")
      case Shl => s"${emitExpr(p.args.head)}.shl<${p.consts.head.toInt}>()"
      case Shlw => s"${emitExpr(p.args.head)}.shl<${p.consts.head.toInt}>().bits<${bitWidth(p.tpe)}-1,0>()"
      case Shr => s"${emitExpr(p.args.head)}.shr<${p.consts.head.toInt}>()"
      case Dshl => p.args map emitExpr mkString(" << ")
      case Dshlw => s"(${emitExpr(p.args(0))} << ${emitExpr(p.args(1))}).bits<${bitWidth(p.tpe)}-1,0>()"
      case Dshr => p.args map emitExpr mkString(" >> ")
      case Cvt => s"${emitExpr(p.args.head)}.cvt()"
      case Neg => s"-${emitExpr(p.args.head)}"
      case Not => s"~${emitExpr(p.args.head)}"
      case And => p.args map emitExpr mkString(" & ")
      case Or => p.args map emitExpr mkString(" | ")
      case Xor => p.args map emitExpr mkString(" ^ ")
      case Andr => throw new Exception("Andr unimplemented!")
      case Orr => throw new Exception("Orr unimplemented!")
      case Xorr => throw new Exception("Xorr unimplemented!")
      case Cat => s"${emitExpr(p.args(0))}.cat(${emitExpr(p.args(1))})"
      case Bits => s"${emitExpr(p.args.head)}.bits<${p.consts(0).toInt},${p.consts(1).toInt}>()"
      case Head => s"${emitExpr(p.args.head)}.head<${p.consts.head.toInt}>()"
      case Tail => s"${emitExpr(p.args.head)}.tail<${p.consts.head.toInt}>()"
    }
    case _ => throw new Exception(s"Don't yet support $e")
  }

  def emitStmt(doNotDec: Set[String])(s: Statement): Seq[String] = s match {
    case b: Block => b.stmts flatMap {s: Statement => emitStmt(doNotDec)(s)}
    case d: DefNode => {
      val lhs = if (doNotDec.contains(d.name)) d.name
                else genCppType(d.value.tpe) + " " + d.name
      val rhs = emitExpr(d.value)
      Seq(s"$lhs = $rhs;")
    }
    case c: Connect => {
      val lhs = emitExpr(c.loc)
      val rhs = emitExpr(c.expr)
      firrtl.Utils.kind(c.loc) match {
        case firrtl.MemKind => Seq()
        case firrtl.RegKind => Seq(s"$lhs$$next = $rhs;")
        case firrtl.WireKind => {
          if (doNotDec.contains(lhs))
            Seq(s"$lhs = $rhs;")
          else Seq(s"${genCppType(c.loc.tpe)} $lhs = $rhs;")
        }
        case firrtl.PortKind => {
          if (lhs.contains("$")) {
            if (doNotDec.contains(lhs))
              Seq(s"$lhs = $rhs;")
            else Seq(s"${genCppType(c.loc.tpe)} $lhs = $rhs;")
          } else Seq(s"$lhs = $rhs;")
        }
        case firrtl.InstanceKind => {
          if (lhs.contains(".")) Seq(s"$lhs = $rhs;")
          else {
            if (doNotDec.contains(lhs))
              Seq(s"$lhs = $rhs;")
            else Seq(s"${genCppType(c.loc.tpe)} $lhs = $rhs;")
          }
        }
        case _ => Seq(s"$lhs = $rhs;")
      }
    }
    case p: Print => {
      val formatters = "(%h)|(%d)|(%ld)".r.findAllIn(p.string.serialize).toList
      val argWidths = p.args map {e: Expression => bitWidth(e.tpe)}
      if (!(argWidths forall { _ <= 64 })) throw new Exception(s"Can't print wide signals")
      val replacements = formatters zip argWidths map { case(format, width) =>
        if (format == "%h") {
          val printWidth = math.ceil((width/4).toDouble).toInt
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
                        (p.args map {arg => s"${emitExpr(arg)}.as_single_word()"})
      Seq(s"if (${emitExpr(p.en)}) printf(${printfArgs mkString(", ")});")
    }
    case st: Stop => Seq(s"if (${emitExpr(st.en)}) exit(${st.ret});")
    case r: DefRegister => Seq()
    case w: DefWire => Seq()
    case m: DefMemory => Seq()
    case i: WDefInstance => Seq()
    case _ => throw new Exception(s"Don't yet support $s")
  }

  def emitBody(m: Module, circuit: Circuit, prefix: String) = {
    val body = addPrefixToNameStmt(prefix)(m.body)
    val memories = findMemory(body)
    memories foreach {m =>
      if(!memHasRightParams(m)) throw new Exception(s"improper mem! $m")}
    val nodeNames = findNodes(body) map { _.name }
    val wireNames = findWires(body) map { prefix + _.name }
    // FUTURE: remove unneeded or identity renames
    val externalPortNames = findPortNames(m) map { prefix + _ }
    val internalPortNames = findModuleInstances(m.body) flatMap {
      case (moduleType, moduleName) =>
        findPortNames(findModule(moduleType, circuit)) map {prefix + s"$moduleName." + _}
    }
    val allTempSigs = nodeNames ++ wireNames ++ externalPortNames ++ internalPortNames
    val renames = (allTempSigs map { s: String =>
      (s, if (s.contains(".")) s.replace('.','$') else s)
    }).toMap
    val memConnects = grabMemInfo(body).toMap
    val memWriteCommands = memories flatMap {m: DefMemory => {
      m.writers map { writePortName:String => {
        val wrEnName = memConnects(s"$prefix${m.name}.$writePortName.en")
        val wrAddrName = memConnects(s"$prefix${m.name}.$writePortName.addr")
        val wrDataName = memConnects(s"$prefix${m.name}.$writePortName.data")
        val wrMaskName = memConnects(s"$prefix${m.name}.$writePortName.mask")
        val wrEnNameRep = renames.getOrElse(wrEnName, wrEnName)
        val wrAddrNameRep = renames.getOrElse(wrAddrName, wrAddrName)
        val wrDataNameRep = renames.getOrElse(wrDataName, wrDataName)
        val wrMaskNameRep = renames.getOrElse(wrMaskName, wrMaskName)
        MemUpdate(prefix + m.name, wrEnNameRep, wrMaskNameRep, wrAddrNameRep, wrDataNameRep)
      }}
    }}
    val readOutputs = memories flatMap {m: DefMemory => {
      m.readers map { readPortName:String =>
        val rdAddrName = memConnects(s"$prefix${m.name}.$readPortName.addr")
        val rdDataName = s"$prefix${m.name}.$readPortName.data"
        val rdAddrRep = renames.getOrElse(rdAddrName, rdAddrName)
        val rdDataRep = renames.getOrElse(rdDataName, rdDataName)
        (rdDataRep, s"$prefix${m.name}[${rdAddrRep}.as_single_word()]")
      }
    }}
    val readMappings = readOutputs.toMap
    val namesReplaced = replaceNamesStmt(readMappings ++ renames)(body)
    (findRegisters(body), namesReplaced, memWriteCommands)
  }

  def emitMemUpdate(mu: MemUpdate) = {
    s"if (${mu.wrEnName} && ${mu.wrMaskName}) ${mu.memName}[${mu.wrAddrName}.as_single_word()] = ${mu.wrDataName};"
  }
}
