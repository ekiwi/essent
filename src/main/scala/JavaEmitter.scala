package essent

import essent.ir._
import firrtl._
import firrtl.ir._
import firrtl.PrimOps._

object JavaEmitter {

  def isBigInt(tpe: Type): Boolean = genJavaType(tpe) == "BigInteger"

  def genPublicJavaType(tpe: Type): String = tpe match {
    case UIntType(IntWidth(w)) => if (w == 1) "public boolean" else if (w <= 64) "public long" else "public BigInteger"
    case SIntType(IntWidth(w)) => if (w == 1) "public boolean" else if (w <= 64) "public long" else "public BigInteger"
    case AsyncResetType => "public boolean"
    case _ => throw new Exception(s"No Java type implemented for $tpe")
  }

  def genJavaType(tpe: Type): String = tpe match {
    case UIntType(IntWidth(w)) => if (w == 1) "boolean" else if (w <= 64) "long" else "BigInteger"
    case SIntType(IntWidth(w)) => if (w == 1) "boolean" else if (w <= 64) "long" else "BigInteger"
    case AsyncResetType => "boolean"
    case _ => throw new Exception(s"No Java type implemented for $tpe")
  }

  def emitPort(topLevel: Boolean)(p: Port): Seq[String] = p.tpe match {
    case ClockType => if (!topLevel) Seq()
    else Seq(genPublicJavaType(UIntType(IntWidth(1))) + " " + p.name + " = false;")
    case _ => if (!topLevel) Seq()
    else {
      val publicType = genPublicJavaType(p.tpe)
      if (p.tpe == UIntType(IntWidth(1)))
        Seq(genPublicJavaType(p.tpe) + " " + p.name + " = false;")
      else if (publicType.contains("BigInteger"))
        Seq(genPublicJavaType(p.tpe) + " " + p.name + " = BigInteger.valueOf(0);")
      else
        Seq(genPublicJavaType(p.tpe) + " " + p.name + " = 0L;")
    }
  }
  def emitExpr(e: Expression)(implicit rn: Renamer = null): String = e match {
    case w: WRef => if (rn != null) rn.emit(w.name) else w.name
    // check this -> if literal is bigint
    case u: UIntLiteral =>
      val maxIn64Bits = (BigInt(1) << 64) - 1
      val width = bitWidth(u.tpe)
      val asDecStr = u.value.toString(10)
      if ((width <= 64) || (u.value <= maxIn64Bits)) s"$asDecStr" else s"(${splatLargeLiteralIntoRawArray(u.value, width)})"
    case u: SIntLiteral =>
      val width = bitWidth(u.tpe)
      if ((width <= 64)) s"${u.value.toString(10)}" else s"(${splatLargeLiteralIntoRawArray(u.value, width)})"
    case m: Mux =>
      val condName = emitExprWrap(m.cond)
      val tvalName = emitExprWrap(m.tval)
      val fvalName = emitExprWrap(m.fval)
      s"$condName ? $tvalName : $fvalName"
    case w: WSubField =>
      val result = s"${emitExpr(w.expr)(null)}.${w.name}"
      if (rn != null) rn.emit(result)
      else result
    // case w: WSubAccess => s"${emitExpr(w.expr)}[${emitExprWrap(w.index)}.as_single_word()]"
    case p: DoPrim =>


      // val hasBigInt = p.args.any(a => isBigInt(a.tpe))
      var hasBigInt = false
      for(arg<-p.args){
        if (isBigInt(arg.tpe)){
          hasBigInt = true
        }
      }
      // check if any big int, make sure all are big ints
      // 
      // isBigInt(bitWp.args.head.tpe)

      // p.args.head => first variable
      // p.args(1) => second variable
      // 
      // if (isBigInt(p.args(1).tpe)) emitExprWrap(p.args(1)) else s"BigInteger.valueOf${emitExprWrap(p.args(1))}"
      //    
      // var first_param = false   
      // try{
      //   first_param = isBigInt(p.args(1).tpe)
      // }
      if(hasBigInt){
        // replace all emitExpr with emitBigIntExpr
        p.op match{
          case Add => s"${emitBigIntExpr(p.args.head)}.add(${emitBigIntExpr(p.args(1))})"
          case Addw => "not implemented yet"
          case Sub => s"${emitBigIntExpr(p.args.head)}.subtract(${emitBigIntExpr(p.args(1))})"
          case Subw => "not implemented yet"
          case Mul => s"${emitBigIntExpr(p.args.head)}.multiply(${emitBigIntExpr(p.args(1))})"
          case Div => s"${emitBigIntExpr(p.args.head)}.divide(${emitBigIntExpr(p.args(1))})"
          case Rem => s"${emitBigIntExpr(p.args.head)}.remainder(${emitBigIntExpr(p.args(1))})"
          case Lt  => s"${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == -1"
          case Leq  => s"${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == -1 || ${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == 0"
          case Gt  => s"${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == 1"
          case Geq => s"${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == 1 || ${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == 0"
          case Eq => s"${emitBigIntExpr(p.args.head)}.compareTo(${emitBigIntExpr(p.args(1))}) == 0"
          // case Eq if !first_param => s"${emitExprWrap(p.args.head)}.equals(BigInteger.valueOf(${emitExprWrap(p.args(1))}))"
          case Neq => s"!${emitBigIntExpr(p.args.head)}.equals(${emitBigIntExpr(p.args(1))})"
          case Pad => "not implemented yet"
          case AsUInt => "not implemented yet"
          case AsSInt => "not implemented yet"
          case AsClock => throw new Exception("AsClock unimplemented!")
          case AsAsyncReset => "not implemented yet"
          case Shl => "not implemented yet"
          case Shr => "not implemented yet"
          case Dshl => s"${emitBigIntExpr(p.args.head)}.shiftLeft(${emitBigIntExpr(p.args(1))})"
          case Dshlw => "not implemented yet"
          case Dshr => s"${emitBigIntExpr(p.args.head)}.shiftRight(${emitBigIntExpr(p.args(1))})"
          case Cvt => "not implemented yet"
          case Neg => s"${emitBigIntExpr(p.args.head)}.negate()"
          case Not => s"${emitBigIntExpr(p.args.head)}.not()"
          case And => s"${emitBigIntExpr(p.args.head)}.and(${emitBigIntExpr(p.args(1))})"
          case Or => s"${emitBigIntExpr(p.args.head)}.or(${emitBigIntExpr(p.args(1))})"
          case Xor => s"${emitBigIntExpr(p.args.head)}.xor(${emitBigIntExpr(p.args(1))})"
          case Andr => "not implemented yet"
          case Orr => "not implemented yet"
          case Xorr => "not implemented yet"
          case Cat => "not implemented yet"
          case Bits => "not implemented yet"
          case Head => "not implemented yet"
          case Tail => "not implemented yet"
          case _ => "not implemented"
        }
      }
      else{
        p.op match {
          case Add => p.args map emitExprWrap mkString(" + ") // BigInt -> old.add(BigInteger.valueOf(new))
          case Addw => s"${emitExprWrap(p.args.head)}.addw(${emitExprWrap(p.args(1))})"
          case Sub => p.args map emitExprWrap mkString(" - ") // BigInt -> old.subtract(BigInteger.valueOf(new))
          case Subw => s"${emitExprWrap(p.args.head)}.subw(${emitExprWrap(p.args(1))})"
          case Mul => p.args map emitExprWrap mkString(" * ") // BigInt -> old.multiply(BigInteger.valueOf(new))
          case Div => p.args map emitExprWrap mkString(" / ") // BigInt -> old.divide(BigInteger.valueOf(new))
          case Rem => p.args map emitExprWrap mkString(" % ") // BigInt -> old.remainder/mod(BigInteger.valueOf(new))
          case Lt  => p.args map emitExprWrap mkString(" < ")
          case Leq => p.args map emitExprWrap mkString(" <= ")
          case Gt  => p.args map emitExprWrap mkString(" > ")
          case Geq => p.args map emitExprWrap mkString(" >= ")
          case Eq => p.args map emitExprWrap mkString(" == ") // BigInt -> old.equals(new))
          case Neq => p.args map emitExprWrap mkString(" != ")
          case Pad => s"${emitExprWrap(p.args.head)}.pad<${bitWidth(p.tpe)}>()"
          case AsUInt => s"${emitExprWrap(p.args.head)}.asUInt()"
          case AsSInt => s"${emitExprWrap(p.args.head)}.asSInt()"
          case AsClock => throw new Exception("AsClock unimplemented!")
          case AsAsyncReset => emitExpr(p.args.head)
          case Shl => s"${emitExprWrap(p.args.head)}.shl<${p.consts.head.toInt}>()"
          case Shr => s"${emitExprWrap(p.args.head)}.shr<${p.consts.head.toInt}>()"
          case Dshl => p.args map emitExprWrap mkString(" << ") // old.shiftLeft(int)
          case Dshlw => s"${emitExprWrap(p.args.head)}.dshlw(${emitExpr(p.args(1))})"
          case Dshr => p.args map emitExprWrap mkString(" >> ") // old.shiftRight(int)
          case Cvt => s"${emitExprWrap(p.args.head)}.cvt()"
          case Neg => s"-${emitExprWrap(p.args.head)}" // old.negate()
          case Not => s"!${emitExprWrap(p.args.head)}" // old.not()
          case And => p.args map emitExprWrap mkString(" & ") // old.and(new)
          case Or => p.args map emitExprWrap mkString(" | ") // old.or(new)
          case Xor => p.args map emitExprWrap mkString(" ^ ") // old.xor(new)
          case Andr => s"${emitExprWrap(p.args.head)}.andr()"
          case Orr => s"${emitExprWrap(p.args.head)}.orr()"
          case Xorr => s"${emitExprWrap(p.args.head)}.xorr()"
          case Cat => s"${emitExprWrap(p.args.head)}.cat(${emitExpr(p.args(1))})"
          case Bits => s"${emitExprWrap(p.args.head)}.bits<${p.consts.head.toInt},${p.consts(1).toInt}>()"
          case Head => s"${emitExprWrap(p.args.head)}.head<${p.consts.head.toInt}>()"
          case Tail => s"${emitExprWrap(p.args.head)} & 0xffff"
        }
      }
    case _ => throw new Exception(s"Don't yet support $e")
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
    case st: Stop =>
      Seq(s"if (UNLIKELY(${emitExpr(st.en)})) {assert_triggered = true; assert_exit_code = ${st.ret};}")
    case mw: MemWrite =>
      Seq(s"if (update_registers && ${emitExprWrap(mw.wrEn)} && ${emitExprWrap(mw.wrMask)}) ${mw.memName}[${emitExprWrap(mw.wrAddr)}.as_single_word()] = ${emitExpr(mw.wrData)};")
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

  def emitBigIntExpr(e: Expression)(implicit rn: Renamer): String = {
    if(isBigInt(e.tpe)){
      emitExprWrap(e)
    }
    else{
      s"BigInteger.valueOf(${emitExprWrap(e)})"
    }
  }

  def splatLargeLiteralIntoRawArray(value: BigInt, width: BigInt): String = {
    val rawHexStr = value.toString(16)
    val isNeg = value < 0
    val asHexStr = if (isNeg) rawHexStr.tail else rawHexStr
    val arrStr = chunkLitString(asHexStr) map { "0x" + _} mkString(",")
    val leadingNegStr = if (isNeg) "(uint64_t) -" else ""
    val numWords = (width + 63) / 64
    s"std::array<uint64_t,$numWords>({$leadingNegStr$arrStr})"
  }

  def chunkLitString(litStr: String, chunkWidth:Int = 16): Seq[String] = {
    if (litStr.length < chunkWidth) Seq(litStr) else chunkLitString(litStr.dropRight(chunkWidth)) ++ Seq(litStr.takeRight(chunkWidth))
  }
}