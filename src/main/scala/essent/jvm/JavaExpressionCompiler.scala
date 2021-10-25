package essent.jvm

import firrtl.ir

/** Based on Chick's implementations in https://github.com/chipsalliance/treadle */
object JavaExpressionCompiler {
  def javaTpe(t: ir.Type): String = javaTpe(firrtl.bitWidth(t).toInt)

  def javaTpe(bits: Int): String = {
    require(bits > 0, "Zero size types need to be removed earlier!")
    if(bits == 0) "boolean"
    else if(bits  < 64) "long"
    else "BigInteger"
  }

  def random(tpe: ir.Type): String = javaTpe(tpe) match {
    case "boolean" => "rand.b()"
    case "long" => s"rand.l(${firrtl.bitWidth(tpe)})"
    case "BigInteger" => s"rand.bi(${firrtl.bitWidth(tpe)})"
  }

  def longLiteral(value: BigInt): String = {
    require(value.isValidLong)
    s"${value}L"
  }

  def compile(e: ir.Expression): String = comp(e, withParenthesis = false)

  private def comp(e: ir.Expression, withParenthesis: Boolean = true): String = e match {
    case ir.UIntLiteral(value, ir.IntWidth(bits)) => compLiteral(value, bits)
    case ir.SIntLiteral(value, ir.IntWidth(bits)) => compLiteral(value, bits)
    case p: ir.DoPrim => if (withParenthesis) "(" + compPrim(p) + ")" else compPrim(p)
    // TODO: renamer
    case ir.Reference(name, _, _, _) => name
    case ir.SubField(expr, name, _, _) => comp(expr, withParenthesis = false) + "." + name
    case ir.Mux(cond, tval, fval, _) =>
      val s = s"${comp(cond)}? ${comp(tval)} : ${comp(fval)}"
      if (withParenthesis) "(" + s + ")" else s
    case other => throw new NotImplementedError(s"Not supported yet: ${other.serialize}")
  }


  private def convert(e: String, inBits: Int, outBits: Int): String = {
    require(inBits <= outBits)
    val (inTpe, outTpe) = (javaTpe(inBits), javaTpe(outBits))
    if(inTpe == outTpe) { e } else {
      (inTpe, outTpe) match {
        case ("boolean", "long") => s"if($e) 1L else 0L"
        case ("boolean", "BigInteger") => s"if($e) BigInteger.valueOf(1) else BigInteger.valueOf(0)"
        case ("long", "BigInteger") => s"BigInteger.valueOf($e)"
      }
    }
  }

  private def compLiteral(value: BigInt, bits: BigInt): String = ???
  private def compPrim(p: ir.DoPrim): String = ???

}
