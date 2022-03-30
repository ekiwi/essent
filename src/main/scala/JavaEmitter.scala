package essent

import essent.ir._
import firrtl._
import firrtl.ir._
import firrtl.PrimOps._

object JavaEmitter {

  def isBigInt(tpe: Type): Boolean = genJavaType(tpe) == "BigInteger"

  def isBoolean(tpe: Type): Boolean = genJavaType(tpe) == "boolean"

  def genJavaType(tpe: Type): String = tpe match {
    case UIntType(IntWidth(w)) => if (w == 1) "boolean" else if (w <= 64) "long" else "BigInteger"
    case SIntType(IntWidth(w)) => if (w == 1) "boolean" else if (w <= 64) "long" else "BigInteger"
    case AsyncResetType => "boolean"
    case _ => throw new Exception(s"No Java type implemented for $tpe")
  }

  def asBigInt(e: Expression)(implicit rn: Renamer) : String = {
    if (isBigInt(e.tpe)) {
      emitBigIntExpr(e)
    } else if (isBoolean(e.tpe)) {
      s"BigInteger.valueOf(${emitExpr(e)} ? 1 : 0)"
    }
    else {
      s"BigInteger.valueOf(${emitExpr(e)})"
    }
  }

  def fromBigInt(tpe: Type, value: String)(implicit rn: Renamer) : String = {
    if (isBigInt(tpe)) {
      value
    } else if (isBoolean(tpe)) {
      s"!$value.equals(BigInteger.ZERO)"
    }
    else {
      s"$value.longValue()"
    }
  }

  def emitPort(topLevel: Boolean)(p: Port): Seq[String] = p.tpe match {
    case ClockType => if (!topLevel) Seq()
    else Seq("public " + genJavaType(UIntType(IntWidth(1))) + " " + p.name + " = false;")
    case _ => if (!topLevel) Seq()
    else {
      val publicType = "public" + genJavaType(p.tpe)
      if (p.tpe == UIntType(IntWidth(1)))
        Seq("public " + genJavaType(p.tpe) + " " + p.name + " = false;")
      else if (publicType.contains("BigInteger"))
        Seq("public " + genJavaType(p.tpe) + " " + p.name + " = BigInteger.valueOf(0);")
      else {
        Seq("public " + genJavaType(p.tpe) + " " + p.name + " = 0L;")
      }
    }
  }

  def emitStmt(s: Statement)(implicit rn: Renamer): Seq[String] = s match {
    case b: Block => b.stmts flatMap emitStmt
    case d: DefNode =>
      val lhs_orig = d.name
      val lhs = rn.emit(lhs_orig)
      val rhs = emitExpr(d.value)
      if (rn.decLocal(lhs_orig)) Seq(s"${genJavaType(d.value.tpe)} $lhs = $rhs;") else Seq(s"$lhs = $rhs;")
    case c: Connect =>
      val lhs_orig = emitExpr(c.loc)(null)
      val lhs = rn.emit(lhs_orig)
      val rhs = emitExpr(c.expr)
      if (rn.decLocal(lhs_orig)) Seq(s"${genJavaType(c.loc.tpe)} $lhs = $rhs;") else Seq(s"$lhs = $rhs;")
    case p: Print =>
      val formatters = "(%h)|(%x)|(%d)|(%ld)".r.findAllIn(p.string.serialize).toList
      val argWidths = p.args map {e: Expression => bitWidth(e.tpe)}
      if (!(argWidths forall { _ <= 64 })) throw new Exception(s"Can't print wide signals")
      val replacements = formatters zip argWidths map { case(format, width) =>
        if (format == "%h" || format == "%x") {
          val printWidth = math.ceil(width.toDouble/4).toInt
          (format, s"%0${printWidth}h")
        } else {
          val printWidth = math.ceil(math.log10((1L<<width.toInt).toDouble)).toInt
          (format, s"%${printWidth}d")
        }
      }
      val formatString = replacements.foldLeft(p.string.serialize){
        case (str, (searchFor, replaceWith)) => str.replaceFirst(searchFor, replaceWith)
      }
      val printfArgs = Seq(s""""$formatString"""") ++
        (p.args map {arg => s"${emitExprWrap(arg)}"})
      Seq(s"if (done_reset && update_registers && verbose && ${emitExprWrap(p.en)}) System.out.println(${printfArgs mkString ", "});")
    case st: Stop =>
      Seq(s"if (${emitExpr(st.en)}) {assert_triggered = true; assert_exit_code = ${st.ret};}")
    case mw: MemWrite =>
      Seq(s"if (update_registers && ${emitExprWrap(mw.wrEn)} && ${emitExprWrap(mw.wrMask)}) ${mw.memName}[${emitExprWrap(mw.wrAddr)}] = ${emitExpr(mw.wrData)};")
    case ru: RegUpdate => Seq(s"if (update_registers) ${emitExpr(ru.regRef)} = ${emitExpr(ru.expr)};")
    case r: DefRegister => Seq()
    case w: DefWire => Seq()
    case m: DefMemory => Seq()
    case i: WDefInstance => Seq()
    case _ => throw new Exception(s"Don't yet support $s")
  }

  def emitExpr(e: Expression)(implicit rn: Renamer = null): String = e match {
    case w: WRef => if (rn != null) rn.emit(w.name) else w.name
    case u: UIntLiteral =>
      val maxIn64Bits = (BigInt(1) << 64) - 1
      val width = bitWidth(u.tpe)
      if (width == 1) s"${u.value == 1}"
      else if ((width <= 64) || (u.value <= maxIn64Bits)) s"${u.value.toString(10)}L"
      else emitBigIntExpr(e)
    case u: SIntLiteral =>
      val width = bitWidth(u.tpe)
      if (width <= 64) s"${u.value.toString(10)}L"
      else emitBigIntExpr(e)
    case m: Mux =>
      val condName = emitExprWrap(m.cond)
      val tvalName = emitExprWrap(m.tval)
      val fvalName = emitExprWrap(m.fval)
      s"$condName ? $tvalName : $fvalName"
    case w: WSubField =>
      val result = s"${emitExpr(w.expr)(null)}.${w.name}"
      if (rn != null) rn.emit(result)
      else result
    case w: WSubAccess => s"${emitExpr(w.expr)}[${emitExprWrap(w.index)}]"
    case p: DoPrim =>
      for (arg<-p.args) {
        if (isBigInt(arg.tpe)) return emitBigIntExpr(e)
      }
      p.op match {
        case Add => p.args map emitExprWrap mkString " + "
        case Addw => s"(${emitExprWrap(p.args.head)} + ${emitExprWrap(p.args(1))}) & ${1 << (bitWidth(p.args.head.tpe).intValue.max(bitWidth(p.args(1).tpe).intValue) - 1)}"
        case Sub => p.args map emitExprWrap mkString " - "
        case Subw => s"(${emitExprWrap(p.args.head)} - ${emitExprWrap(p.args(1))}) & ${1 << (bitWidth(p.args.head.tpe).intValue.max(bitWidth(p.args(1).tpe).intValue) - 1)}"
        case Mul => p.args map emitExprWrap mkString " * "
        case Div => p.args map emitExprWrap mkString " / "
        case Rem => p.args map emitExprWrap mkString " % "
        case Lt  => p.args map emitExprWrap mkString " < "
        case Leq => p.args map emitExprWrap mkString " <= "
        case Gt  => p.args map emitExprWrap mkString " > "
        case Geq => p.args map emitExprWrap mkString " >= "
        case Eq => p.args map emitExprWrap mkString " == "
        case Neq => p.args map emitExprWrap mkString " != "
        case Pad => s"${emitExprWrap(p.args.head)}"
        case AsUInt => emitExprWrap(p.args.head)
        case AsSInt => emitExprWrap(p.args.head)
        case AsClock => throw new Exception("AsClock unimplemented!")
        case AsAsyncReset => emitExpr(p.args.head)
        case Shl => s"${emitExprWrap(p.args.head)} << ${p.consts.head.toInt}"
        case Shr => s"${emitExprWrap(p.args.head)} >> ${p.consts.head.toInt}"
        case Dshl => p.args map emitExprWrap mkString " << "
        case Dshlw => p.args map emitExprWrap mkString " >> "
        case Dshr => p.args map emitExprWrap mkString " >> "
        case Cvt => s"${emitExprWrap(p.args.head)}"
        case Neg => s"-${emitExprWrap(p.args.head)}"
        case Not => s"!${emitExprWrap(p.args.head)}"
        case And => p.args map emitExprWrap mkString " & "
        case Or => p.args map emitExprWrap mkString " | "
        case Xor => p.args map emitExprWrap mkString " ^ "
        case Andr => s"${emitExprWrap(p.args.head)} & ${(1 << bitWidth(p.args.head.tpe).intValue) - 1} == ${emitExprWrap(p.args.head)} ? 1L : 0L"
        case Orr => s"${emitExprWrap(p.args.head)} == 0L ? 0L : 1L"
        case Xorr => s"xorr(${emitExprWrap(p.args.head)})"
        case Cat => s"(${emitExprWrap(p.args.head)} << ${bitWidth(p.args(1).tpe)}) + ${emitExpr(p.args(1))}"
        case Bits => s"${emitExprWrap(p.args.head)} & ${(1 << (p.consts(1).toInt - p.consts.head.toInt + 1) - 1) << p.consts.head.toInt}"
        case Head => s"${emitExprWrap(p.args.head)} & ${((1 << (bitWidth(p.args.head.tpe).intValue - p.consts.head.toInt)) - 1) << p.consts.head.toInt}"
        case Tail => s"${emitExprWrap(p.args.head)} & ${(1 << (bitWidth(p.args.head.tpe).intValue - p.consts.head.toInt)) - 1}"
      }
    case _ => throw new Exception(s"Don't yet support $e")
  }

  def emitBigIntExpr(e: Expression)(implicit rn: Renamer): String = e match {
    case w: WRef => if (rn != null) rn.emit(w.name) else w.name
    case u: UIntLiteral =>
      s"""new BigInteger("${u.value}")"""
    case u: SIntLiteral =>
      s"""new BigInteger("${u.value}")"""
    case m: Mux =>
      val condName = emitBigIntExprWrap(m.cond)
      val tvalName = emitBigIntExprWrap(m.tval)
      val fvalName = emitBigIntExprWrap(m.fval)
      s"$condName ? $tvalName : $fvalName"
    case w: WSubField =>
      val result = s"${emitBigIntExpr(w.expr)(null)}.${w.name}"
      if (rn != null) rn.emit(result)
      else result
    case p: DoPrim =>
      p.op match {
        case Add => s"${emitBigIntExpr(p.args.head)}.add(${emitBigIntExpr(p.args(1))})"
        case Addw => "not implemented yet1"
        case Sub => s"${emitBigIntExpr(p.args.head)}.subtract(${emitBigIntExpr(p.args(1))})"
        case Subw => "not implemented yet2"
        case Mul => s"${emitBigIntExpr(p.args.head)}.multiply(${emitBigIntExpr(p.args(1))})"
        case Div => s"${emitBigIntExpr(p.args.head)}.divide(${emitBigIntExpr(p.args(1))})"
        case Rem => s"${emitBigIntExpr(p.args.head)}.remainder(${emitBigIntExpr(p.args(1))})"
        case Lt => s"${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == -1"
        case Leq => s"${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == -1 || ${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == 0"
        case Gt => s"${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == 1"
        case Geq => s"${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == 1 || ${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == 0"
        case Eq => s"${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == 0"
        case Neq => s"!${emitBigIntExpr(p.args.head)}.equals(${emitBigIntExpr(p.args(1))})"
        case Pad => "not implemented yet3"
        case AsUInt => "not implemented yet4"
        case AsSInt => "not implemented yet5"
        case AsClock => throw new Exception("AsClock unimplemented!")
        case AsAsyncReset => "not implemented yet6"
        case Shl => "not implemented yet7"
        case Shr => "not implemented yet8"
        case Dshl => s"${emitBigIntExpr(p.args.head)}.shiftLeft(${emitBigIntExpr(p.args(1))})"
        case Dshlw => "not implemented yet9"
        case Dshr => s"${emitBigIntExpr(p.args.head)}.shiftRight(${emitBigIntExpr(p.args(1))})"
        case Cvt => "not implemented yet10"
        case Neg => s"${emitBigIntExpr(p.args.head)}.negate()"
        case Not => s"${emitBigIntExpr(p.args.head)}.not()"
        case And => s"${emitBigIntExpr(p.args.head)}.and(${emitBigIntExpr(p.args(1))})"
        case Or => s"${emitBigIntExpr(p.args.head)}.or(${emitBigIntExpr(p.args(1))})"
        case Xor => s"${emitBigIntExpr(p.args.head)}.xor(${emitBigIntExpr(p.args(1))})"
        case Andr => s"${emitBigIntExprWrap(p.args.head)}.and(BigInteger.valueOf(${(1 << bitWidth(p.args.head.tpe).intValue) - 1})).equals(${emitBigIntExprWrap(p.args.head)}) ? BigInteger.ONE : BigInteger.ZERO"
        case Orr => s"${emitBigIntExprWrap(p.args.head)}.equals(BigInteger.ZERO) ? BigInteger.ZERO : BigInteger.ONE"
        case Xorr => s"xorr(${emitBigIntExprWrap(p.args.head)})"
        case Cat => s"(${emitBigIntExprWrap(p.args.head)}.shiftLeft(${bitWidth(p.args(1).tpe)}).add(${emitBigIntExpr(p.args(1))})"
        case Bits => s"${emitBigIntExprWrap(p.args.head)}.and(BigInteger.valueOf((1 << ((${p.consts.head.toInt} + 1) - ${p.consts(1).toInt})) - 1 << ${p.consts(1).toInt}))"
        case Head => "not implemented yet"
        case Tail => s"${emitBigIntExprWrap(p.args.head)}.and(BigInteger.ONE.shiftLeft(${bitWidth(p.args.head.tpe) - p.consts.head.toInt}) - BigInteger.ONE)"
        case _ => "not implemented"
      }
  }

  def emitExprWrap(e: Expression)(implicit rn: Renamer): String = e match {
    case DoPrim(_,_,_,_) | Mux(_,_,_,_) => s"(${emitExpr(e)})"
    case _ => emitExpr(e)
  }

  def emitBigIntExprWrap(e: Expression)(implicit rn: Renamer): String = e match {
    case DoPrim(_,_,_,_) | Mux(_,_,_,_) => s"(${emitBigIntExpr(e)})"
    case _ => emitBigIntExpr(e)
  }
}