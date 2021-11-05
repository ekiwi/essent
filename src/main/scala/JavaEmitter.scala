package essent

import essent.Emitter.{emitPort, genCppType, initializeVals}
import essent.Extract.{findInstancesOf, findModule, findModuleInstances}
import firrtl.ir.{AsyncResetType, Circuit, DefMemory, DefRegister, ExtModule, IntWidth, Module, SIntType, Type, UIntType}

import java.io.Writer

class JavaEmitter(initialOpt: OptFlags, writer: Writer) {
  val tabs = "  "

  def writeLines(indentLevel: Int, lines: String): Unit = writeLines(indentLevel, Seq(lines))

  def writeLines(indentLevel: Int, lines: Seq[String]): Unit = {
    lines.foreach { s => writer.write(tabs*indentLevel + s + "\n") }
  }

  def genJavaType(tpe: Type) = tpe match {
    case UIntType(IntWidth(w)) => "public int"
    case SIntType(IntWidth(w)) => "public int"
    case AsyncResetType => "public boolean"
    case _ => throw new Exception(s"No Java type implemented for $tpe")
  }


  def declareModule(m: Module, topName: String) {
    val registers = findInstancesOf[DefRegister](m.body)
    val memories = findInstancesOf[DefMemory](m.body)
    val registerDecs = registers flatMap {d: DefRegister => {
      val typeStr = genCppType(d.tpe)
      val regName = d.name
      Seq(s"$typeStr $regName;")
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
    writeLines(0, "")
    writeLines(1, s"$modName() {")
    writeLines(2, initializeVals(modName == topName)(m, registers, memories))
    writeLines(1, "}")

  }

  def execute(circuit: Circuit): Unit = {
    val topName = circuit.main

    circuit.modules foreach {
      case m: Module => declareModule(m, topName)
    }
    val topModule = findModule(topName, circuit) match {case m: Module => m}
  }
}