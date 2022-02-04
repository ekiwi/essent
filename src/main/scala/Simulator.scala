package essent

import java.nio.file.Files
import java.nio.file.Paths
import net.openhft.compiler.CompilerUtils

class Simulator(firFilePath : String = null) {
  Driver.main(Array("-O0", "-java", firFilePath))

  val javaFilePath: String = firFilePath.replaceAll(".fir", ".java")
  val javaFileContent: String = Files.readString(Paths.get(javaFilePath))
  val className: String = firFilePath.split("/").last.split("\\.")(0)

  val aClass: Class[_] = CompilerUtils.CACHED_COMPILER.loadFromJava(className, javaFileContent)
  val runner: Runnable = aClass.newInstance.asInstanceOf[Runnable]
  runner.run()

  def poke(node : String, value : Int): Unit = {

  }

  def peek(node : String): Int = {
    0
  }

  def step(): Unit = {

  }
}
