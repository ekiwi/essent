package essent

import java.io.{BufferedReader, FileInputStream, InputStreamReader}
import java.net.URLClassLoader
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import javax.tools.ToolProvider
import java.util.stream.Collectors

object JavaRuntimeCompiler {
  def compile(sourcePath : String): Simulator = {
    val source = readCode(sourcePath)
    val name = Paths.get(sourcePath).getFileName.toString.split('.')(0)
    val javaFile = saveSource(source, name)
    val classFile: Path = compileSource(javaFile, name)
    loadClass(classFile, name)
  }

  def loadClass(javaClass: Path, name : String): Simulator = {
    val classUrl = javaClass.getParent.toFile.toURI.toURL
    val classLoader = URLClassLoader.newInstance(Array[java.net.URL](classUrl))
    val cls = Class.forName(name, true, classLoader)
    val instance = cls.getDeclaredConstructor().newInstance()
    instance.asInstanceOf[Simulator]
  }

  def compileSource(javaFile: Path, name: String): Path = {
    val compiler = ToolProvider.getSystemJavaCompiler
    compiler.run(null, null, null, javaFile.toFile.getAbsolutePath)
    javaFile.getParent.resolve(name + ".class")
  }

  def saveSource(source: String, name: String): Path = {
    val tmpProperty = System.getProperty("java.io.tmpdir")
    val sourcePath = Paths.get(tmpProperty, name + ".java")
    Files.write(sourcePath, source.getBytes(StandardCharsets.UTF_8))
  }

  def readCode(sourcePath: String): String = {
    val stream = new FileInputStream(sourcePath)
    val separator = System.getProperty("line.separator")
    val reader = new BufferedReader(new InputStreamReader(stream))
    reader.lines.collect(Collectors.joining(separator))
  }
}
