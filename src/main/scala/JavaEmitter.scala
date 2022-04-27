package essent

import essent.ir._
import firrtl._
import firrtl.ir._
import firrtl.PrimOps._

object JavaEmitter {
  val primOp2Expr : Set[PrimOp] = Set(Add, Sub, Mul, Div, Rem, Lt, Leq, Gt, Geq,  Eq, Neq, Dshl, Dshr, And, Or, Xor, Cat)

  def genJavaType(tpe: Type): String = tpe match {
    case UIntType(IntWidth(w)) => if (w == 1) "boolean" else if (w <= 63) "long" else "BigInteger"
    case SIntType(IntWidth(w)) => if (w == 1) "boolean" else if (w <= 63) "long" else "BigInteger"
    case AsyncResetType => "boolean"
    case _ => throw new Exception(s"No Java type implemented for $tpe")
  }

  def isBigInt(tpe: Type): Boolean = genJavaType(tpe) == "BigInteger"

  def isLong(tpe : Type) : Boolean = genJavaType(tpe) == "long"

  def isBoolean(tpe: Type): Boolean = genJavaType(tpe) == "boolean"

  def asBigInt(e: Expression)(implicit rn: Renamer) : String = {
    if (isBigInt(e.tpe)) {
      emitBigIntExpr(e)
    } else if (isBoolean(e.tpe)) {
      if (isSInt(e)) s"${emitExpr(e)} ? BigInteger.ONE.negate() : BigInteger.ZERO"
      else s"${emitExpr(e)} ? BigInteger.ONE : BigInteger.ZERO"
    }
    else {
      s"BigInteger.valueOf(${emitExpr(e)})"
    }
  }

  def fromBigInt(e: Expression, value: String)(implicit rn: Renamer) : String = {
    if (isBigInt(e.tpe)) {
      value
    } else if (isBoolean(e.tpe)) {
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
      if (isBoolean(p.tpe))
        Seq("public " + genJavaType(p.tpe) + " " + p.name + " = false;")
      else if (isBigInt(p.tpe))
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
        (p.args map {arg => if (isBoolean(arg.tpe)) s"${emitExprWrap(arg)} ? 1L : 0L" else emitExprWrap(arg)})
      Seq(s"if (done_reset && update_registers && verbose && ${emitExprWrap(p.en)}) System.out.print(String.format(${printfArgs mkString ", "}));")
    case st: Stop =>
      Seq(s"if (${emitExpr(st.en)}) {assert_triggered = true; assert_exit_code = ${st.ret};}")
    case mw: MemWrite =>
      Seq(s"if (update_registers && ${emitExprWrap(mw.wrEn)} && ${emitExprWrap(mw.wrMask)}) ${mw.memName}[(int)${emitExprWrap(mw.wrAddr)}] = ${emitExpr(mw.wrData)};")
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
      val width = bitWidth(u.tpe)
      if (width == 1) s"${u.value == 1}"
      else if (width <= 63) s"${u.value.toString(10)}L"
      else emitBigIntExpr(e)
    case u: SIntLiteral =>
      val width = bitWidth(u.tpe)
      if (width <= 63) s"${u.value.toString(10)}L"
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
    case w: WSubAccess => s"${emitExpr(w.expr)}[(int)${emitExprWrap(w.index)}]"
    case p: DoPrim =>
      if (isBigInt(p.tpe) || p.args.exists(arg => isBigInt(arg.tpe))) return emitBigIntExpr(p)
      val signCvt = if (p.tpe.isInstanceOf[UIntType]) "asUInt" else "asSInt"
      val arg1 = if (isBoolean(p.args.head.tpe)) {
        if (p.args.head.tpe.isInstanceOf[UIntType]) s"(${emitExprWrap(p.args.head)} ? 1L : 0L)"
        else s"(${emitExprWrap(p.args.head)} ? -1L : 0L)"
        } else emitExprWrap(p.args.head)
      val arg2 = if (primOp2Expr contains p.op)
        if (isBoolean(p.args(1).tpe)) {
          if (p.args(1).tpe.isInstanceOf[UIntType]) s"(${emitExprWrap(p.args(1))} ? 1L : 0L)"
          else s"(${emitExprWrap(p.args(1))} ? -1L : 0L)"
        } else emitExprWrap(p.args(1))
      p.op match {
        case Add => s"$arg1 + $arg2"
        case Sub => s"$signCvt($arg1 - $arg2, ${bitWidth(p.tpe)})"
        case Mul => s"$arg1 * $arg2"
        case Div => narrowLong(p.tpe, s"$arg1 / $arg2")
        case Rem => narrowLong(p.tpe, s"$arg1 % $arg2")
        case Lt  => s"$arg1 < $arg2"
        case Leq => s"$arg1 <= $arg2"
        case Gt  => s"$arg1 > $arg2"
        case Geq => s"$arg1 >= $arg2"
        case Eq => s"$arg1 == $arg2"
        case Neq => s"$arg1 != $arg2"
        case Pad => s"$arg1"
        case AsUInt => narrowLong(p.tpe, s"asUInt($arg1, ${bitWidth(p.args.head.tpe)})")
        case AsSInt => narrowLong(p.tpe, s"asSInt($arg1, ${bitWidth(p.args.head.tpe)})")
        case Shl => s"$arg1 << ${p.consts.head.toInt}L"
        case Shr => narrowLong(p.tpe, s"$arg1 >> ${p.consts.head.toInt}L")
        case Dshl => s"$arg1 << $arg2"
        case Dshr =>
          if (p.tpe.isInstanceOf[UIntType])
            narrowLong(p.tpe, s"${bitWidth(p.args.head.tpe)} > (int)$arg2 ? $arg1 >> $arg2 : 0L")
          else
            narrowLong(p.tpe, s"${bitWidth(p.args.head.tpe)} > (int)$arg2 ? $arg1 >> $arg2 : -1L")
        case Cvt => narrowLong(p.tpe, arg1)
        case Neg => narrowLong(p.tpe, s"-$arg1")
        case Not => narrowLong(p.tpe, s"(~$arg1 & ${(1L << bitWidth(p.tpe).longValue) - 1L}L)")
        case And => narrowLong(p.tpe, s"($arg1 & $arg2) & ${(1L << bitWidth(p.tpe).longValue) - 1L}L")
        case Or => narrowLong(p.tpe, s"($arg1 | $arg2) & ${(1L << bitWidth(p.tpe).longValue) - 1L}L")
        case Xor => narrowLong(p.tpe, s"($arg1 ^ $arg2) & ${(1L << bitWidth(p.tpe).longValue) - 1L}L")
        case Andr => s"($arg1 & ${(1L << bitWidth(p.args.head.tpe).longValue) - 1L}L) == ${(1L << bitWidth(p.args.head.tpe).longValue) - 1L}L ? true : false"
        case Orr => s"$arg1 == 0L ? false : true"
        case Xorr => s"xorr($arg1, ${bitWidth(p.args.head.tpe)})"
        case Cat =>
            val e1 = s"($arg1 & ${(1L << bitWidth(p.args.head.tpe).longValue) - 1L}L)"
            val e2 = s"($arg2 & ${(1L << bitWidth(p.args(1).tpe).longValue) - 1L}L)"
            s"($e1 << ${bitWidth(p.args(1).tpe)}) | $e2"
        case Bits => narrowLong(p.tpe, s"($arg1 & ${((1L << bitWidth(p.tpe).intValue) - 1L) << p.consts(1).toLong}L) >> ${p.consts(1).toLong}")
        case Head => narrowLong(p.tpe, s"($arg1 & ${((1L << p.consts.head.toLong) - 1L) << (bitWidth(p.args.head.tpe).intValue - p.consts.head.toLong)}L) >> ${bitWidth(p.args.head.tpe).intValue - p.consts.head.toLong}")
        case Tail => narrowLong(p.tpe, s"$arg1 & ${(1L << (bitWidth(p.args.head.tpe).longValue - p.consts.head.toLong)) - 1L}L")
        case other => s"not implemented: ${other.serialize}"
      }
    case _ => throw new Exception(s"Don't yet support $e")
  }

  def narrowBigInt(resultTpe : Type, expr : String) : String = {
    if (isBigInt(resultTpe)) expr
    else if (isBoolean(resultTpe)) s"!($expr).equals(BigInteger.ZERO)"
    else s"($expr).longValue()"
  }

  def narrowLong(resultTpe : Type, expr : String) : String = {
    if (isLong(resultTpe)) expr
    else s"($expr) != 0L"
  }

  def isSInt(e: Expression): Boolean = {
    SIntType(IntWidth(bitWidth(e.tpe))) == e.tpe
  }

  def emitBigIntExpr(e: Expression)(implicit rn: Renamer): String = e match {
    case w: WRef =>
      val name = if (rn != null) rn.emit(w.name) else w.name
      if (bitWidth(w.tpe) >= 64) name
      else if (bitWidth(w.tpe) >= 2) s"BigInteger.valueOf($name)"
      else if (isSInt(w)) s"$name ? BigInteger.ONE.negate() : BigInteger.ZERO"
      else s"$name ? BigInteger.ONE : BigInteger.ZERO"
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
      val arg1 = emitBigIntExprWrap(p.args.head)
      val arg2 = if (primOp2Expr contains p.op) emitBigIntExprWrap(p.args(1))
      p.op match {
        case Add => s"($arg1).add($arg2)"
        case Sub => s"($arg1).subtract($arg2)"
        case Mul => s"($arg1).multiply($arg2)"
        case Div => narrowBigInt(p.tpe, s"($arg1).divide($arg2)")
        case Rem => narrowBigInt(p.tpe, s"($arg1).remainder($arg2)")
        case Lt => s"($arg1).compareTo($arg2) == -1"
        case Leq => s"($arg1).compareTo($arg2) == -1 || ($arg1).compareTo($arg2) == 0"
        case Gt => s"($arg1).compareTo($arg2) == 1"
        case Geq => s"($arg1).compareTo($arg2) == 1 || ($arg1).compareTo($arg2) == 0"
        case Eq => s"($arg1).compareTo($arg2) == 0"
        case Neq => s"!($arg1).equals($arg2)"
        case Pad => s"$arg1"
        case AsUInt => s"asUInt($arg1, ${bitWidth(p.tpe)})"
        case AsSInt => s"asSInt($arg1, ${bitWidth(p.tpe)})"
        case Shl => s"($arg1).shiftLeft(${p.consts.head.toInt})"
        case Shr =>
          narrowBigInt(p.tpe,s"($arg1).shiftRight(${p.consts.head.toInt})")
        case Dshl =>
          val shamt = if (isBoolean(p.args(1).tpe)) s"${emitExprWrap(p.args(1))} ? 1 : 0" else s"(int)${emitExprWrap(p.args(1))}"
          s"($arg1).shiftLeft($shamt)"
        case Dshr =>
          val shamt = if (isBoolean(p.args(1).tpe)) s"${emitExprWrap(p.args(1))} ? 1 : 0" else s"(int)${emitExprWrap(p.args(1))}"
          if (p.tpe.isInstanceOf[UIntType])
            narrowBigInt(p.tpe,s"${bitWidth(p.args.head.tpe)} > $shamt ? ($arg1).shiftRight($shamt) : BigInteger.ZERO")
          else
            narrowBigInt(p.tpe,s"${bitWidth(p.args.head.tpe)} > $shamt ? ($arg1).shiftRight($shamt) : BigInteger.ONE.negate()")
        case Cvt => narrowBigInt(p.tpe, arg1)
        case Neg => narrowBigInt(p.tpe, s"$arg1.negate()")
        case Not => narrowBigInt(p.tpe, s"""($arg1).not().and(new BigInteger("${(BigInt(1) << bitWidth(p.tpe).intValue) - BigInt(1)}"))""")
        case And => narrowBigInt(p.tpe, s"""($arg1).and($arg2).and(new BigInteger("${(BigInt(1) << bitWidth(p.tpe).intValue) - BigInt(1)}"))""")
        case Or => narrowBigInt(p.tpe, s"""($arg1).or($arg2).and(new BigInteger("${(BigInt(1) << bitWidth(p.tpe).intValue) - BigInt(1)}"))""")
        case Xor => narrowBigInt(p.tpe, s"""($arg1).xor($arg2).and(new BigInteger("${(BigInt(1) << bitWidth(p.tpe).intValue) - BigInt(1)}"))""")
        case Andr => s"""($arg1).and(new BigInteger("${(BigInt(1) << bitWidth(p.args.head.tpe).intValue) - BigInt(1)}")).equals(new BigInteger("${(BigInt(1) << bitWidth(p.args.head.tpe).intValue) - BigInt(1)}")) ? true : false"""
        case Orr => s"($arg1).equals(BigInteger.ZERO) ? false : true"
        case Xorr => s"xorr($arg1, ${bitWidth(p.args.head.tpe)})"
        case Cat =>
          val e1 = s"""$arg1.and(new BigInteger("${(BigInt(1) << bitWidth(p.args.head.tpe).intValue) - BigInt(1)}"))"""
          val e2 = s"""$arg2.and(new BigInteger("${(BigInt(1) << bitWidth(p.args(1).tpe).intValue) - BigInt(1)}"))"""
          s"$e1.shiftLeft(${bitWidth(p.args(1).tpe)}).or($e2)"
        case Bits => narrowBigInt(p.tpe, s"""($arg1).and(new BigInteger("${((BigInt(1) << bitWidth(p.tpe).intValue) - BigInt(1)) << p.consts(1).intValue}")).shiftRight(${p.consts(1).toInt})""")
        case Head => narrowBigInt(p.tpe, s"""($arg1).and(new BigInteger("${((BigInt(1) << p.consts.head.intValue) - 1) << (bitWidth(p.args.head.tpe).intValue - p.consts.head.intValue)}")).shiftRight(${bitWidth(p.args.head.tpe).intValue - p.consts.head.intValue})""")
        case Tail => narrowBigInt(p.tpe, s"($arg1).and(BigInteger.ONE.shiftLeft(${bitWidth(p.args.head.tpe) - p.consts.head.toInt}).subtract(BigInteger.ONE))")
        case other => s"not implemented: ${other.serialize}"
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