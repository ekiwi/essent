package essent

import java.net.URLClassLoader
import javax.tools.ToolProvider

object JavaRuntimeCompiler {
  import CompilerHelpers._

  def compile(sourcePath: os.Path): Simulator = {
    val source = os.read(sourcePath)
    val name = sourcePath.baseName
    val javaFile = saveSource(source, name)
    val classFile = compileSource(javaFile, name)
    loadClass(classFile, name).asInstanceOf[Simulator]
  }

  def compile(name: String, code: String): Simulator = {
    require(!name.contains('\n'), "The name should not contain newlines. Did you sonfuse name and code?")
    val javaFile = saveSource(code, name)
    val classFile = compileSource(javaFile, name)
    loadClass(classFile, name).asInstanceOf[Simulator]
  }
}

private object CompilerHelpers {
  def loadClass(javaClass: os.Path, name: String): Any = {
    val classUrl = javaClass.toNIO.getParent.toFile.toURI.toURL
    val currentClassLoader = Thread.currentThread.getContextClassLoader
    val classLoader = URLClassLoader.newInstance(Array[java.net.URL](classUrl), currentClassLoader)
    val cls = classLoader.loadClass(name)
    val instance = cls.getDeclaredConstructor().newInstance()
    instance
  }

  def compileSource(javaFile: os.Path, name: String): os.Path = {
    val compiler = ToolProvider.getSystemJavaCompiler
    // explicitly specify the class path
    compiler.run(null, null, null,
      "-classpath", getClassPath.mkString(":"), javaFile.toString())
    javaFile / os.up / (name + ".class")
  }

  def saveSource(source: String, name: String): os.Path = {
    val filename = os.temp.dir() / (name + ".java")
    os.write.over(filename, source)
    filename
  }

  /** Returns the current class path, taking into account sbt's layered class loader strategy */
  def getClassPath: Seq[String] = {
    val loader = Thread.currentThread().getContextClassLoader
    loader match {
      // this matches sbt's layered class loader from which we can extract the local class path
      case url : URLClassLoader => url.getURLs.map(_.getPath).toIndexedSeq
      // in a forked JVM, the default Java class loader will be active and we can just use the system path
      case _ => System.getProperty("java.class.path").split(':').toIndexedSeq
    }
  }
}