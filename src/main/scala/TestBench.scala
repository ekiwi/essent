import essent.{Driver, Simulator}
import net.openhft.compiler.CompilerUtils
import java.nio.file.{Files, Paths}

object TestBench {
  def main(args: Array[String]) {
    val firFilePath = args(0)

    // Driver.main(Array("-O0", "-java", firFilePath))

    val javaFilePath: String = firFilePath.replaceAll(".fir", ".java")
    val javaFileContent: String = Files.readString(Paths.get(javaFilePath))
    val className: String = firFilePath.split("/").last.split("\\.")(0)

    val aClass: Class[_] = CompilerUtils.CACHED_COMPILER.loadFromJava(className, javaFileContent)
    val sim: Simulator = aClass.newInstance.asInstanceOf[Simulator]

    sim.run() // just for debugging
    sim.makeMap()
    sim.poke("io_a", 4)
    sim.poke("io_b", 8)
    sim.step()
    println(sim.peek("io_z"))
    sim.step()
    println(sim.peek("io_z"))
  }
}
