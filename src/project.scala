/** 
     - Author: Caio Rodrigues <caiorss [DOT] rodrigues [AT] gmail [DOT] com>

    This is free and unencumbered software released into the public domain.

    Anyone is free to copy, modify, publish, use, compile, sell, or
    distribute this software, either in source code form or as a compiled
    binary, for any purpose, commercial or non-commercial, and by any
    means.

    In jurisdictions that recognize copyright laws, the author or authors
    of this software dedicate any and all copyright interest in the
    software to the public domain. We make this dedication for the benefit
    of the public at large and to the detriment of our heirs and
    successors. We intend this dedication to be an overt act of
    relinquishment in perpetuity of all present and future rights to this
    software under copyright law.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
    OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
    ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
    OTHER DEALINGS IN THE SOFTWARE.  

 =============================================================== */

package jarget.project

import jarget.utils.{Utils, JarUtils}
import jarget.utils.JarBuilder
import jarget.mvn._
import jarget.reader._


// Provided by package: com.typesafe/config/1.3.3
import com.typesafe.config.{ConfigFactory, Config => TConfig}

class ConfigWrapper(config: TConfig){
  import scala.collection.JavaConverters._

  def get() = config

  def getOrDefault[A](path: String, default: A)(getter: TConfig => A) =
    try getter(this.config)
    catch {
      case ex: com.typesafe.config.ConfigException => default
    }

  def getString(path: String, default: String) =
    this.getOrDefault(path, default){ _.getString(path)}

  def getStringList(path: String): List[String] =
    this.getOrDefault(path, List(): List[String]){
      _.getStringList(path).asScala.toList
    }
}

class ProjectBuilder(
  conf:  TConfig,
 ){
  import java.io.File

  private var verbose = false
  private val repoUrl   = "http://repo1.maven.org/maven2"
  private val cachePath = PackCache.getCacheHome(".jarget")

  private val confw = new ConfigWrapper(conf)
  private val scalaVersion: String =
    confw.getString("scalaVersion", "2.12")
  // Directory containing sources that will be compiled
  private val src: String = confw.getString("src", "./src")
  // Application name without any extension:
  private val appname: String =
    confw.getString("app", "app")

  // Output directory
  private val outputDir: String =
    confw.getString("output", "./out")

  private val outputFile =
    new File(outputDir, appname + "-dev.jar")
  
  private val libDir =
    new File("./lib")
  
  // Dependencies that will be downloaded from remote repository.
  private val packageNames: List[String] =
    confw.getStringList("packages")
  
  // Resource directories
  private val resources: List[String] =
    confw.getStringList("resources")

  /** Find files matching a predicate function in a directory and all its sub directories */
  private def findFiles(path: String, pred: java.io.File => Boolean) : List[java.io.File] = {
    import java.nio.file.{Files, Paths, Path}
    import java.util.stream.{Stream => JStream}
    val root = Paths.get(path)
    Files.walk(root)
      .toArray()
      .toList.asInstanceOf[List[Path]]
      .map(_.toFile)
      .filter{f => pred(f) }
  }

  def setVerbose(verbose: Boolean) = {
    this.verbose = verbose
    this
  }
  
  /** Get source files from src directory recursively */
  def getSources() =
    findFiles(src, _.getName().endsWith(".scala"))

  def getPackages() = packageNames map { pstr =>
    val p = PackData.read(pstr, "/")
    if (p.isEmpty) 
      throw new RuntimeException(s"Error: Invalid package format <$pstr>")
    p.get
  }

  def getLibJars(): List[String] = {
    if(!libDir.isDirectory())
      List()
    else new java.io.File("./lib")
      .listFiles().toList.filter(_.getName().endsWith(".jar")).map(_.getPath)
  }

  /** Get list with abosulte path of all jar files used by the application. */
  def getAllJars(): List[String] = {
    val packList  = this.getPackages()    
    // val classpath = Packget.getPackCPathFromCache(packList, cachePath, repoUrl)    
    val packFiles =
      Packget.getPackJarsFromCache(
        packList,
        cachePath,
        repoUrl
      )
    packFiles ++ getLibJars()
  }

  /** Get overall classpath from packages in lib directory and from mvn repository. */
  def getClasspath() = {
    val packList  = this.getPackages()
    val classpath = Packget.getPackCPathFromCache(packList, cachePath, repoUrl)
    // Get default separator (:) for Unix or (;) for Windows 
    val sep = System.getProperty("path.separator")
    "./" + sep + classpath + sep + this.getLibJars().mkString(sep)
  }

  /** Check whether output file exists. */
  def outputExists() =
    outputFile.isFile()

  /** Show project details - It is useful to debug the build. */
  def show() = {
    println(s"Source directory = $src")
    println(s"Development build output file = $outputFile")
    println(s"Output directory              = $outputDir")
    println(s"Scala Version    = $scalaVersion")
    println()
    println("Source files: ")
    println(" ------------------------------------ ")
    this.getSources() foreach {source => println(s" - $source")}
    println()
    println("Dependencies: ")
    println(" ------------------------------------ ")
    this.getPackages() foreach {pack => println(s" - ${pack.format()}")}
  }

  def buildDev() = {
    val sources   = this.getSources().map(p =>  p.getPath())
    // Create output directory if does not exist:
    new java.io.File(outputDir).mkdirs()        
    Utils.execl(
      if (this.outputExists()) "fsc" else "scalac",
      args = List("-d", outputFile.getPath(), "-cp",  this.getClasspath()) ++ sources,
      env = List(),
      verbose = this.verbose
    )
  }

  def buildRelease(wrapper: String) = {
    // Compile if output file does not exist.
    if(!this.outputExists())
      this.buildDev()    
    val exe = JarBuilder.parseWrapper(wrapper)
    val outputBuild = outputDir + "/" + appname + "-release" + exe.getExt()
    JarBuilder.makeUberJar(
      cls       = getClass(),
      output    = outputBuild,
      main      = outputFile.getPath(),
      scalaLib  = true,
      resources = resources,
      jarFiles  = this.getAllJars(),
      wrapper   = exe
    )
    println("Built file: " + outputBuild)
  }

  /** Run application passing arguments to it. */
  def run(args: List[String]) = {
    // Compile if output file does not exist.
    if(!this.outputExists())
      this.buildDev()
    Utils.execl(
      "scala",
      args = List("-cp", this.getClasspath(),  outputFile.getPath()) ++ args,
      env = List(),
      verbose = this.verbose
    )
  }

}


object ProjectBuilder{
  def ofFile(file: String) = {
    val conf = ConfigFactory.parseFile(new java.io.File(file)).resolve()
    new ProjectBuilder(conf)
  }
}
