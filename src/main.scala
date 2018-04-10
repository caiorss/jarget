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

package jarget.main

import jarget.utils.{Utils, JarUtils}
import jarget.utils.JarBuilder
import jarget.mvn._
import jarget.reader._

case class AppSettings(
  version: String,
  repoUrl: String,
  website: String
)


object MainUtils {

  def parsePack(pstr: String) = {
    val p = PackData.read(pstr, "/")
    if (p.isEmpty) {
      println("Error: Invalid package format")
      System.exit(1)
    }
    p.get 
  }

  def parseScalaPack(pstr: String, scalaVersion: String) = {
    val fields = pstr.split("/").map(_.trim)
    val p = fields match {
      case Array(group, artifact, version)
          => Some(PackData(group, artifact + "_" + scalaVersion, version))
      case _
          => None
    }

    if (p.isEmpty) {
      println("Error: Invalid package format")
      System.exit(1)
    }
    p.get
  }


  /// Get package from maven XML in the clipboard.
  def getPackMaven() = {
    val p = Utils.getClipboardText()
      .map(scala.xml.XML.loadString)
      .map(PackData.readFromXML)

    if (p.isEmpty) {
      println("Error: Invalid maven XML package format")
      System.exit(1)
    }
    p.get 
  }

  def showPackageInfo(pack: PackData) = 
    for (pom <- pack.getPomXML()){
      val dat = Pom.getPomData(pom)
      println( "Package:         " + dat.name)
      println( "Packaging:       " + dat.packaging)
      println(s"Coordinates[1]:  group = ${dat.group} artifact = ${dat.artifact} version = ${dat.version}")
      println(s"Coordinates[2]:  ${dat.group}/${dat.artifact}/${dat.version}")
      println( "Url:             " + dat.url)
      println( "Description:     " + dat.description)

      println("\nDependencies:\n")
      Pom.getPomDependencies(pom) foreach { p =>
        println("  - " + p.format() + "\n")
      }
    }
  

  def showPom(pack: PackData) = {
    pack.getPomXML() foreach println 
  }

  def openUrl(pack: PackData) = {
    val url = s"https://mvnrepository.com/artifact/${pack.group}/${pack.artifact}/${pack.version}"
    println("Opening package: " + url)
    Utils.openUrl(url)
  }

  /** Read application configuration file */
  def getAppSettings(prop: java.util.Properties) = {
    val sopt = for {
      version <- Option(prop.getProperty("jarget.version"))
      repoUrl <- Option(prop.getProperty("jarget.repository.url"))
      website <- Option(prop.getProperty("jarget.website"))
    } yield AppSettings(version, repoUrl, website)
    sopt match {
      case Some(s)
          => s
      case None
          => throw new java.lang.IllegalArgumentException("Error: cannot read property file")
    }
  }

   def showPlatformInfo() = {
     val lineSep = System.getProperty("line.separator")
       .replace("\r\n", "'\\r\\n' - CRLF")
       .replace("\n", "'\\n' - LF")
       .replace("\r", "'\\r' - CR")
     val nproc = Runtime.getRuntime().availableProcessors()
     println(s"""
Operating System              = ${System.getProperty("os.name")}
Operating System Version      = ${System.getProperty("os.version")}
Operating System Architecture = ${System.getProperty("os.arch")}
Number of processors          = $nproc cores
Processor endianess           = ${System.getProperty("sun.cpu.endian")}

OS Path Separators and File Encoding

- path.separator  = '${System.getProperty("path.separator")}'
- file.separator  = '${System.getProperty("file.separator")}'
- line.separator  =  $lineSep
- file.enconding  =  ${System.getProperty("file.encoding")}

Java Runtime

- java.vm.specification.version = ${System.getProperty("java.specification.version")}
- java.runtime.version          = ${System.getProperty("java.runtime.version")}

- java.vm.name                  = ${System.getProperty("java.vm.name")}
- java.home                     = ${System.getProperty("java.home")}
   """)
   } // --- EOF showPlatformInfo --- //


} // ------ End of object MainUtils ---------- //


object Main{

  import MainUtils._

  def tryMVNGet(action: => Unit) = 
    try {
      action
      System.exit(0)
    } catch {
      case ex: java.io.FileNotFoundException
          => {
            println("Error: package not found.")
            System.exit(1)
          }

      case ex: java.net.UnknownHostException
          => {
            println("Error: DNS Failure")
            System.exit(1)
          }

      // Throw unknown exception again  
      case ex: Throwable => throw ex 
    }

  def parseArgs(args: Array[String]) : Unit = {

    jarget.logger.Log.setLevel()

    val config =
      Utils.readResourceProperties("/assets/app.properties")
        .map(MainUtils.getAppSettings _ )
        .run(getClass())

    val cachePath = PackCache.getCacheHome(".jarget")

    args.toList match {
      case List("-v") | List("-version")
          => println(config.version)
      case List("-site")
        => Utils.openUrl(config.website)

    }

  }// -- End of function main() --- //

  import jarget.optParser.{OptResult, OptParser, OptSet}


  val config =
      Utils.readResourceProperties("/assets/app.properties")
        .map(MainUtils.getAppSettings _ )
        .run(getClass())

  val repoUrl = Option(System.getenv("jarget.url")) getOrElse config.repoUrl

  val cachePath = PackCache.getCacheHome(".jarget")

  def getLibPath(path: String) = Option(System.getenv("jarget.path")) getOrElse path


  def makeCommandWithCPATH(
    name:      String,
    desc:      String = "",
    usage:     String = "",
    longDesc:  String = ""
  )(action: OptResult => Unit) =
    new OptSet(
        name,
        desc,
        usage,
        longDesc
      ).addOpt(
        name      = "package",
        shortName = "p",
        argName   = "<PACK>",
        desc      = "Package maven's coordinate"
      ).addOpt(
        name      = "package-str",
        argName   = "<PACK1>,<PACK2>...",
        shortName = "ps",
        desc      = "Package's separated by command <pack1>,<pack2>...<packN> "
    ).setAction(action)



  val mvnShow = new OptSet(
    name  = "mvn-show",
    usage = "<PACKAGE>",
    desc  = "Show package's information."
  ).setAction{ res => 
    val pstr = res.getOperands()(0)
    showPackageInfo(parsePack(pstr)) run repoUrl 
  }

  val mvnSearch = new OptSet(
    name  = "mvn-search",
    usage = "<QUERY>",
    desc  = "Search for a package at the site https://mvnrepository.com"
  ).setAction{ res => 
    val query = res.getOperands()(0)
    Utils.openUrl("https://mvnrepository.com/search?q=" + query)
  }


  val mvnPom = new OptSet(
    name  = "mvn-pom",
    usage = "<PACKAGE>",
    desc  = "Show package's pom.xml file."
  ).setAction{ res => 
    val pstr = res.getOperands()(0)
    showPom(parsePack(pstr)) run repoUrl
  }
  

  val mvnPull = new OptSet(
    name  = "mvn-pull",
    usage = "<PACKAGE1> [<PACKAGE2> ...]",
    desc  = "Download package to cache directory.",
    longDesc = """
 Note: The package ins the format <group>/<artifact>/<version>

 Example: 
  $ jarget mvn-pull org.scalaz/scalaz-core_2.11/7.3.0-M15 org.jfree/jfreechart/1.0.17                 
               """
  ).setAction{ res =>
    tryMVNGet{
      val packs = res.getOperands() map parsePack
      Packget.getPackJarsFromCache(packs, cachePath, repoUrl)
    }
  }

  val mvnDoc = new OptSet(
    name  = "mvn-doc",
    usage = "<PACKAGE>",
    desc  = "Open package documentation in the web browser."
  ).setAction{ res => 
    val pstr = res.getOperands()(0)
    val pack = parsePack(pstr)
    val url  = s"https://mvnrepository.com/artifact/${pack.group}/${pack.artifact}/${pack.version}"
    Utils.openUrl(url)
  } 

  //  Copy packages from cache directory to ./lib and download it
  //  if has not been downloaded yet.
  val mvnCopy = new OptSet(
    name  = "mvn-copy",
    usage = "<PACKAGE1> [<PACKAGE2> ...]",
    desc  = "Copy jar packages from cache directory to ./lib downloading them if not available."
  ).setAction{ res => 
    val packs = res.getOperands() map parsePack
     tryMVNGet {
       Packget.copyPackageFromCache(packs, cachePath, repoUrl, getLibPath("./lib"))
     }
  }


  val uberOptSet = new OptSet(
    name  = "uber",
    usage = "[OPTIONS] <MAIN-JAR> [<JARFILE1.jar> <JARFILE2.jar> ...]",
    desc  = "Build uber jar file for deployment by bundling dependencies and resource files."
  ).addOpt(
    name      = "output",
    shortName = "o",
    argName   = "<file>",
    desc      = "Output file, default out.jar"
  ).addOpt(
    name      = "scala",
    shortName = "s",
    desc      = "Bundle Scala runtime library scala-runtime.jar"
  ).addOpt(
    name      = "package",
    shortName = "p",
    argName   = "<pack>",
    desc      = "MVN Coordinates of a java package -  <group>/<artifact>/<version>."      
  ).addOpt(
    name      = "file",
    shortName = "f",
    argName   = "<file>",
    desc      = "Jar files to be added to the package."
  ).addOpt(
    name      = "resource",
    shortName = "r",
    argName   = "<folder>",
    desc      = "Resource directory"
  ).addOpt(
    name      = "jardir",
    shortName = "jd",
    argName   = "<folder>",
    desc      = "Directory containing jar files to be bundled into the uber jar."
  ).addOpt(
    name      = "exe",
    shortName = "e",
    argName   = "<EXE>",
    desc      = ""
  ).setAction{ (res: OptResult) =>
    
    val scalaFlag     = res.getFlag("scala")
    val packages      = res.getListStr("package")
    val files         = res.getListStr("file")
    val output        = res.getStr("output", "out.jar")
    val resourcesDirs = res.getListStr("resource")
    val exe           = JarBuilder.parseWrapper(res.getStr("exe", "empty"))
    val mainJarFile   = res.getOperandOrExit(0, "Error: missing main jar file.")
    val jarFiles      = res.getOperands.tail

    val packFiles =
      Packget.getPackJarsFromCache(
        packages map parsePack,
        cachePath,
        config.repoUrl
      )

    JarBuilder.makeUberJar(
      cls       = getClass(),
      output    = output,  
      main      = mainJarFile,
      scalaLib  = scalaFlag,
      resources = resourcesDirs,
      jarFiles  = jarFiles ++ packFiles,
      wrapper   = exe 
    )
  }

  val execCommand = makeCommandWithCPATH(
    name  = "exec",
    desc  = "Execute a shell command and pass -cp <CLASSPATH> of packages downloaded to it.",
    usage = "[OPTIONS] -- <PROGRAM> [<PROGRAM ARGS> ...]"
  ){ res =>
    val packList = res.getListStr("package") map parsePack toList

    if (res.getListStr("--").isEmpty){
      println("Error: missing command after -- ")
      System.exit(1)
    }
    val command     = res.getListStr("--").head
    val commandArgs = res.getListStr("--").tail
    tryMVNGet {
      val cpath = Packget.getPackCPathFromCache(packList, cachePath, config.repoUrl)
      JarUtils.runWithClassPath2(command, commandArgs, cpath)
     }
  }

  val scriptCommand = makeCommandWithCPATH(
    name = "script",
    desc = "Run a scala script with a given set of packages from cache.",
    usage = "[OPTIONS] -- <SCRIPT.scala> [<SCRIPT ARGS> ...]"
  ){ res =>
    val packList1 = res.getListStr("package").map(parsePack).toList
    val packList2 = res.getStr("package-str", "").split(",") match {
      case Array("") => List()
      case xs        => xs.map(parsePack).toList
    }

    if(res.getListStr("--").isEmpty){
      println("Error: missing command after -- ")
      System.exit(1)
    }
    val script     = res.getListStr("--").head
    val scriptArgs = res.getListStr("--").tail
    tryMVNGet {      
      val cpath = Packget.getPackCPathFromCache(packList1 ++ packList2, cachePath, config.repoUrl)
      //println(s"Script = ${script} args = ${args}")
      JarUtils.runWithClassPath2("scala", "-save"::script::scriptArgs, cpath)
    }
  }

  val scalaCommand = makeCommandWithCPATH(
    name  = "scala",
    desc  = "Run a scala script with a given set of packages from cache.",
    usage = "[OPTIONS] -- [<SCALA ARGS> ...]"
  ){ res =>
    // println("Running scala command")
    val packList1 = res.getListStr("package").map(parsePack).toList
    // println("packaList1 = " + packList1)
    val packList2 = res.getStr("package-str", "").split(",") match {
      case Array("") => List()
      case xs        => xs.map(parsePack).toList
    }
    // println("packaList2 = " + packList2)
    val scalaArgs = res.getListStr("--")
    // println("args = " + scalaArgs)
    tryMVNGet {
      val cpath = Packget.getPackCPathFromCache(packList1 ++ packList2, cachePath, config.repoUrl)
      // println("cpath = " + cpath)
      //println(s"Script = ${script} args = ${args}")
      JarUtils.runWithClassPath2("scala", scalaArgs, cpath)
    }
 }


  //----- Cache commands ------------------- //

  val cachePathOpt = new OptSet(
    name = "cache",
    usage = "<ACTION>",
    desc = "Show packages in cache directory."
  ).addOpt(
    name = "path",
    desc = "Show cache's directory path."
  ).addOpt(
    name = "pack",
    desc = "Show packages in cache directory"
  ).addOpt(
    name = "jars",
    desc = "Show all jar files in cache directory"
  ).setAction{ (res: OptResult) =>

    if( res.getFlag("path")){
      println(cachePath)
      System.exit(0)
    }

    // Show all packages available in the cache repository
    if(res.getFlag("pack")){
       PackCache.getPackagesInCache(cachePath)
         .foreach { case (group, artifact) => println(s"${group}/${artifact}") }
      System.exit(0)
    }

    if(res.getFlag("jars")){
      PackCache.showJarFiles(cachePath)
      System.exit(0)
    }

  }


  //------ Jar Commadns ------------------- //

  val jarManOpt = new OptSet(
    name = "jar-man",
    usage = "<FILE.jar>",
    desc = "Show manifest of a jar file."
  ).setAction{ (res: OptResult) =>
    val file = res.getOperands()(0)
    JarUtils.showManifest(file)
  }

  val jarMainClass = new OptSet(
    name  = "jar-main-class",
    usage = "<FILE.jar>",
    desc  = "Show main class of a jar file."
  ).setAction{ (res: OptResult) =>
    val file = res.getOperands()(0)
    JarUtils.getMainClass(file) foreach println
  }

  val jarShowFiles = new OptSet(
    name = "jar-ls",
    usage = "<FILE.jar>",
    desc  = "Show contents of a jar file."
  ).setAction{ (res: OptResult) =>
    val file = res.getOperandOrExit(0, "Error: missing jar file.")
    JarUtils.showFiles(file) 
  }

  val jarResources = new OptSet(
    name = "jar-rs",
    usage = "<FILE.jar>",
    desc = "Show resources of a jar file ignoring *.class files."
  ).setAction{ res =>
    val file = res.getOperands()(0)
    JarUtils.getAssetFiles(file) foreach println    
  }

  val jarCat = new OptSet(
    name = "jar-cat",
    usage = "<FILE.jar> <FILE>",
    desc = "Show content of a file in a jar package."
  ).setAction{ res =>
    val jarFile = res.getOperandOrExit(0, "Error: missing jar file.")
    val file    = res.getOperandOrExit(1, "Error: missing file name.")
    JarUtils.printFile(jarFile, file)
  }

  val jarExtract  = new OptSet(
    name  = "jar-ex",
    usage =  "<FILE.jar> <file>",
    desc  = "Extract <file> from jar file <FILE.jar> to current directory."
  ).setAction{ res =>
    val jarFile = res.getOperands()(0)
    val file    = res.getOperands()(1)
    JarUtils.extractFile(jarFile, file, ".")
  }

  val jarExtractAll = new OptSet(
    name  = "jar-ex-all",
    usage = "<FILE.jar>",
    desc  = "Extract contents of <FILE.jar> to ·/<FILE> directory."
  ).setAction{ res =>
    val jarFile = res.getOperands()(0)
    val path = new java.io.File(jarFile)
      .getName()
      .stripSuffix(".jar")
    Utils.mkdir(path)
    JarUtils.extract(jarFile, path, true)
  }


  val jarToEXE = new OptSet(
    name  = "jar-to-exe",
    usage = "[OPTIONS] <FILE.jar>",
    desc = "Embed Uber jar into Unix executable or Windows Executable (experimental).",
    longDesc = """
    Note - <EXE> can be:
      + uexe - for Unix executable - Shell script with embedded uber-jar payload.
      + wcli - for Windows CLI command line executable. *.exe file.
      + wgui - for Windows GUI with user interface. -> *.exe file.
   """
  ).addOpt(
    name      = "exe",
    shortName = "e",
    argName   = "<EXE>",
    desc      = "Executable type."
  ).addOpt(
    name      = "output",
    shortName = "o",
    argName   = "<FILE>",
    desc      = "Output file, default <FILE> without extension + .sh or .exe."
  ).setAction{ res =>
    val wrapper   = JarBuilder.parseWrapper(res.getStr("exe", "uexe"))
    val inputJar  = res.getOperands()(0)
    val defaultName = wrapper match {
      case JarBuilder.JWrapperUEXE
          => inputJar.stripSuffix(".jar")
      case JarBuilder.JWrapperWCLI | JarBuilder.JWrapperWGUI
          => inputJar.stripSuffix(".jar") + ".exe"
      case JarBuilder.JWrapperEmpty
          => throw new java.lang.IllegalArgumentException("Invalid jar wrapper option.")
    }
    val output = res.getStr("output", defaultName)
    JarBuilder.makeExecutable(getClass(), inputJar, output, wrapper)
    println(s"Built file ./${output}")
  }


  // --- Crypto Hash Commands -------------------------/

  val digestStrOpt = new OptSet(
    name  = "digest-s",
    usage = " <ALGORITHM> <STRING>",
    desc  = "Compute crypto hash of string. - Algorithm: [md5 | sha1 | sha256 ]"
  ).setAction{ res =>
    import jarget.crypto.Digest
    val algorithms = Map("md5" -> "MD5", "sha1" -> "SHA1", "sha256" -> "SHA-256")
    val op0 = res.getOperandOrExit(0, "Error: missing algorithm: md5, sha1, or sha256.")
    val op1 = res.getOperandOrExit(1, "Error: missing string.")
    val alg = algorithms.get(op0) match {
      case Some(a) => {
         println(Digest.stringDigestSum(a, op1))
      }
      case None => {
        println("Error: algorithm not found.")
        System.exit(1)
      }
    }
  }

  val digestFileOpt = new OptSet(
    name  = "digest-f",
    usage = " <ALGORITHM> <FILE>",
    desc  = "Compute crypto hash of a file. - Algorithm: [md5 | sha1 | sha256 ]"
  ).setAction{ res =>
    import jarget.crypto.Digest
    val algorithms = Map("md5" -> "MD5", "sha1" -> "SHA1", "sha256" -> "SHA-256")
    val op0 = res.getOperandOrExit(0, "Error: missing algorithm: md5, sha1, or sha256.")
    val op1 = res.getOperandOrExit(1, "Error: missing file.")
    val alg = algorithms.get(op0) match {
      case Some(a) =>
        println(Digest.fileDigestSum(a, op1))              
      case None => {
        println("Error: algorithm not found.")
        System.exit(1)
      }
    }
  }



  val utilsOpt = new OptSet(
    name     = "utils",
    usage    = "<ACTION>",
    desc     = "General utilities helpers for platform information and debugging.",
    longDesc = """
  Actions:
   + env        - Show environment variables
   + env <var>  - Show a given environment variable.
   + prop       - Show java properties.
   + prop <var> - Show a given a java property.
   + path       - Show path variable
   + info       - Show platform information.

  Example: $ jarget utils info

   """
  ).setAction{ res =>
    val args = res.getOperands()
    args match {
      // Show environment variable
      case List("env")
          => Utils.showEnvironmentVars()
      // Show a specific environment variable
      case List("env", evar)
          => for { v <- Option(System.getenv(evar)) } println(v)
      // Show Java properties
      case List("prop")
          => Utils.showJavaProperties()
      // Show an specific property
      case List("prop", name)
          => Option(System.getProperty(name)) foreach println
    // Show PATH enviroment variable
    case List("path")
        => for {
          pvar   <- Option(System.getenv("PATH"))
          sep    <- Option(System.getProperty("path.separator"))
          paths  = pvar.split(sep)
        } paths foreach println

   // Show Platform Info
      case List("info")
          => showPlatformInfo()

      // Show path to executable in $PATH variable
      case List("expath", program)
        => Utils.getProgramPath(program) foreach println

      case _ => {
        println("Error: invalid command.")
        System.exit(1)
      }
    }
  }

  val desc = """
 Jarget 3.0 - command line toolbox for Java Platform.
"""

  val parser = new OptParser(desc = desc)
    .add(uberOptSet)
    .add(execCommand)
    .add(scriptCommand)
    .add(scalaCommand)
    .add(mvnShow)
    .add(mvnSearch)
    .add(mvnDoc)  
    .add(mvnPom)
    .add(mvnPull)
    .add(mvnCopy)
    .add(cachePathOpt)
    .add(jarToEXE)
    .add(jarManOpt)
    .add(jarMainClass)
    .add(jarShowFiles)
    .add(jarResources)
    .add(jarCat)
    .add(jarExtract)
    .add(utilsOpt)
    .add(digestStrOpt)
    .add(digestFileOpt)
  
  def main(args: Array[String]) : Unit  = {
    parser.parse(args.toList)
  }

  
} // ------- End of object Main -------- //
