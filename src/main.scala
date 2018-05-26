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

  def onDebug(action: => Unit) =
    if(System.getProperty("jarget.debug") != null) action

  def tryMVNGet(action: => Unit) = 
    try {
      action
      System.exit(0)
    } catch {
      case ex: java.io.FileNotFoundException
          => {
            println("Error: package not found.")
            onDebug{ print(Console.RED); ex.printStackTrace(); print(Console.RESET) }
            System.exit(1)
          }

      case ex: java.net.UnknownHostException
          => {
            println("Error: DNS Failure")
            onDebug{ print(Console.RED); ex.printStackTrace(); print(Console.RESET) }
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
  }// -- End of function parseArgs() --- //


  import jarget.optParser.{OptResult, OptParser, OptCommand, Separator}


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
    new OptCommand(
      name = name,
      desc = desc,
      usage = usage,
      longDesc = longDesc,
      helpFlag = true
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



  val mvnShow = new OptCommand(
    name  = "mvn-show",
    usage = "<PACKAGE>",
    desc  = "Show package's information.",
    helpFlag = true 
  ).setAction{ res => 
    val pstr = res.getOperandOrError(0, "Error: missing package. Use -h or -help to show help.")
    showPackageInfo(parsePack(pstr)) run repoUrl 
  }

  val mvnSearch = new OptCommand(
    name  = "mvn-search",
    usage = "<QUERY>",
    desc  = "Search for a package at the site https://mvnrepository.com",
    helpFlag = true
  ).setAction{ res =>
    val query = res.getOperandOrError(0, "Error: missing query. Use -h to show help.")
    Utils.openUrl("https://mvnrepository.com/search?q=" + query)
  }


  val mvnPom = new OptCommand(
    name  = "mvn-pom",
    usage = "<PACKAGE>",
    desc  = "Show package's pom.xml file.",
    helpFlag = true
  ).setAction{ res => 
    val pstr = res.getOperandOrError(0, "Error: missing package. Use -h or -help to show help.")
    showPom(parsePack(pstr)) run repoUrl
  }
  
  val mvnPull = new OptCommand(
    name  = "mvn-pull",
    usage = "<PACKAGE1> [<PACKAGE2> ...]",
    helpFlag = true,
    desc  = "Download package to cache directory.",    
    longDesc = """Note: Packages are in the the format <group>/<artifact>/<version>""",
    example = """
   Example:
    $ jarget mvn-pull org.scalaz/scalaz-core_2.11/7.3.0-M15 org.jfree/jfreechart/1.0.17
    """
  ).setAction{ res =>
    tryMVNGet{
      val packs = res.getOperands() map parsePack
      Packget.getPackJarsFromCache(packs, cachePath, repoUrl)
    }
  }

  val mvnRunJar = new OptCommand(
    name  = "mvn-run-jar",
    desc  = "Run main method of executable jar package in repository.",
    usage = "<PACKAGE> --  [<ARGS>...]",
    helpFlag = true,
    example = """
 Example 1 :  This command download the file proguard-base-6.0.2.jar
 to the cache repository and runs the command java -jar <path-to-jar>/proguard-base-6.0.2.jar
 Once the file was downloaded, it will be run from cache repository.
  >> $ jarget mvn-run-jar net.sf.proguard/proguard-base/6.0.2

 Example 2 :
   >> $ jarget mvn-run-jar org.codehaus.groovy/groovy/2.5.0-rc-1 -- file1.groovy

 Example 3:  Show Clojure help, to run the repl remove (--help) switch.
  >> $ jarget mvn-run-jar org.clojure/clojure/1.8.0 -- --help
   """
  ).setAction{ res =>
    tryMVNGet{
      val pack = parsePack(res.getOperandOrError(0, "Error: missing package."))
      val args = res.getListStr("--")
      val jarPath = Packget.getPackJarsFromCache(List(pack), cachePath, config.repoUrl).head
      // println("cpath = " + jarPath + "\n")
      Utils.execl("java", List("-jar", jarPath) ++ args)
    }
  }


  val mvnRunCls = new OptCommand(
    name  = "mvn-run-cls",
    desc  = "Run a main class of a java package (class with main static method).",
    longDesc = "Note: this command is useful to run packages with multiple entry points.",
    usage = "<MAIN-CLASS> [OPTIONS] [<JAVA-PROPERTIES> ...] --  [<ARGS>...]",
    example = """
 Example 1: Run scala compiler and invokes -help by calling the class scala.tools.nsc.Main.
 If the scala compiler packages are not in the cache, they will be downloaded. Further 
 commands needing those packages will no longer downloaded them.

  >>> $ jarget mvn-run-cls scala.tools.nsc.Main \
       -p=org.scala-lang.virtualized/scala-compiler/2.11.2 -Dscala.usejavacp=true -- -help 

 Example 2: It will run the Scala REPL.

  >>> $ jarget mvn-run-cls -p=org.scala-lang.virtualized/scala-compiler/2.11.2  \
       scala.tools.nsc.MainGenericRunner -Dscala.usejavacp=true 

 Example 3: Run Groovy REPL. 

  >>> $ jarget mvn-run-cls org.codehaus.groovy.tools.shell.Main \
    -ps=org.codehaus.groovy/groovy-all/2.4.15,jline/jline/2.11,commons-cli/commons-cli/1.2 

 Example 4: Run Bean Shell Console and Interpreter

   >>> $ jarget mvn-run-cls bsh.Console -p=org.beanshell/bsh/2.0b5
   >>> $ jarget mvn-run-cls bsh.Interpreter -p=org.beanshell/bsh/2.0b5
 """,
    helpFlag = true
  ).addOpt(
    name      = "package",
    shortName = "p",
    argName   = "<pack>",
    desc      = "MVN Coordinates of a java package -  <group>/<artifact>/<version>."      
  ).addOpt(
    name      = "package-str",
    argName   = "<PACK1>,<PACK2>...",
    shortName = "ps",
    desc      = "Package's separated by command <pack1>,<pack2>...<packN> "
  ).addOpt( 
    name      = "classpath",
    argName   = "<CLASSPATH>",
    shortName = "cp",
    desc      = "Additional classpath (default '.')"
  ).setAction{ res =>
    tryMVNGet{
      val sep = System.getProperty("path.separator")
      val cpath = res.getStr("classpath", ".")
      val cls  = res.getOperandOrError(0, "Error: missing main class.")
      val packages1 = res.getListStr("package") map parsePack
      val packages2 = res.getStr("package-str", "").split(",") match {
        case Array("") => List()
        case xs        => xs.map(parsePack).toList
      }
      val args = res.getListStr("--")
      val properties = res.getProperties()
        .toList
        .map{ case (k, v) => "-D" + k + "=" + v }
      val classPath = Packget.getPackCPathFromCache(
        packages1 ++ packages2, cachePath, config.repoUrl
      )
      Utils.execl("java", properties ++ List("-cp", classPath + sep + cpath, cls) ++ args)
    }
  }

  val mvnDoc = new OptCommand(
    name  = "mvn-doc",
    usage = "<PACKAGE>",
    desc  = "Open package documentation in the web browser.",
    helpFlag = true
  ).setAction{ res => 
    val pstr = res.getOperandOrError(0, "Error: missing query. Use -h to show help.")
    val pack = parsePack(pstr)
    val url  = s"https://mvnrepository.com/artifact/${pack.group}/${pack.artifact}/${pack.version}"
    Utils.openUrl(url)
  } 

  //  Copy packages from cache directory to ./lib and download it
  //  if has not been downloaded yet.
  val mvnCopy = new OptCommand(
    name  = "mvn-copy",
    usage = "<PACKAGE1> [<PACKAGE2> ...]",
    desc  = "Copy jar packages from cache directory to ./lib downloading them if not available.",
    helpFlag = true
  ).setAction{ res => 
    val packs = res.getOperands() map parsePack
     tryMVNGet {
       Packget.copyPackageFromCache(packs, cachePath, repoUrl, getLibPath("./lib"))
     }
  }


  val uberOptCommand = new OptCommand(
    name  = "uber",
    usage = "[OPTIONS] <MAIN-JAR> [<JARFILE1.jar> <JARFILE2.jar> ...]",
    desc  = "Build uber jar file for deployment by bundling dependencies and resource files.",
    longDesc = """
    Note - <EXE> can be:
      + empty - (default) for jar file without any executable wrapper. 
      + uexe  - for Unix executable - Shell script with embedded uber-jar payload.
      + wcli  - for Windows CLI command line executable. *.exe file.
      + wgui  - for Windows GUI with user interface. -> *.exe file.
   """,
    helpFlag = true
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
    desc      = "Executable wrapper - default (empty)."
  ).setAction{ (res: OptResult) =>
    
    val scalaFlag     = res.getFlag("scala")
    val packages      = res.getListStr("package")
    val files         = res.getListStr("file")
    val resourcesDirs = res.getListStr("resource")
    val mainJarFile   = res.getOperandOrError(0, "Error: missing main jar file.")
    val jarFiles      = res.getOperands.tail

    val exe         = JarBuilder.parseWrapper(res.getStr("exe", "empty"))

    // Output file
    val output =  res.getStr("output", mainJarFile.stripSuffix(".jar") + "-out" + exe.getExt())

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

    println("Built file: "  + output )
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
    desc  = "Run Scala REPL (scala) passing the class of packages from the repository.",
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



  val runCommand = new OptCommand(
    name  = "run",
    desc  = "Run a main class from a set of jar file passing the classpath of packages in repository.",
    usage = "[OPTIONS] <MAIN-CLASS> <JAR0> [<JAR1> ....] [<JAVA-PROPERTIES> ...] -- [<ARGS>...]",
    example = """ 
 + <MAIN-CLASS> : Is the a class with a main static method that will be executed.
 + <JAR0>       : Is a jar package such as ImageViewer.jar 
 + <ARGS>       : Are the arguments passed to the main class.

 Example and use case: Run the class Main from the jar
 demoImageViewer.jar passing the classpath of the package
 com.jtattoo/JTattoo/1.6.11 from (http://www.jtattoo.net/) and setting
 the property swing.defaultlaf that changes to Java Swing default look
 and feel theme.
 
$  jarget run Main demoImageViewer.jar -p=com.jtattoo/JTattoo/1.6.11 \
   -Dswing.defaultlaf=com.jtattoo.plaf.hifi.HiFiLookAndFeel
    """,
    helpFlag = true
  ).addOpt(
    name      = "package",
    shortName = "p",
    argName   = "<pack>",
    desc      = "MVN Coordinates of a java package -  <group>/<artifact>/<version>."      
  ).setAction{ res =>
    tryMVNGet{
      val cls   = res.getOperandOrError(0, "Error: missing main class.")
      val jar0  = res.getOperandOrError(1, "Error: missing jar package 0.")
      val otherJars = res.getOperands().drop(2)
      val packages  = res.getListStr("package") map parsePack
      val args = res.getListStr("--")
      val properties = res.getProperties()
        .toList
        .map{ case (k, v) => "-D" + k + "=" + v }
      val jarsClassPath =  (List(jar0) ++ otherJars).mkString(":")
      val repoClassPath  = Packget.getPackCPathFromCache(packages, cachePath, config.repoUrl)
      Utils.execl("java", properties ++ List("-cp", repoClassPath + ":" + jarsClassPath, cls) ++ args)
    }
  }


  //----- Cache commands ------------------- //

  val cacheCommand = new OptCommand(
    name = "cache",
    usage = "<ACTION>",
    desc = "Show packages in cache directory.",
    helpFlag = true 
  ).addOpt(
    name = "path",
    desc = "Show cache's directory path."
  ).addOpt(
    name = "pack",
    desc = "Show packages in cache directory"
  ).addOpt(
    name = "jars",
    desc = "Show all jar files in cache directory"
  ).addOpt(
    name = "clean",
    desc = "Clean cache directory freeing space."
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

    if(res.getFlag("clean")){
      Utils.deleteDirectory(cachePath, true)
      System.exit(0)
    }

  }


  //------ Jar Commadns ------------------- //

  val jarManOpt = new OptCommand(
    name = "jar-man",
    usage = "<FILE.jar>",
    desc = "Show manifest of a jar file."
  ).setAction{ (res: OptResult) =>
    val file = res.getOperandExistingFile(
      0,
      "Error expected jar file. Use (-h) to show command help."
    )
    JarUtils.showManifest(file.getPath)
  }

  val jarMainClass = new OptCommand(
    name  = "jar-main-class",
    usage = "<FILE.jar>",
    desc  = "Show main class of a jar file."
  ).setAction{ (res: OptResult) =>
    val file = res.getOperandExistingFile(
      0,
      "Error expected jar file. Use (-h) to show command help."
    )
    JarUtils.getMainClass(file.getPath) foreach println
  }

  val jarShowFiles = new OptCommand(
    name = "jar-ls",
    usage = "<FILE.jar>",
    desc  = "Show contents of a jar file."
  ).setAction{ (res: OptResult) =>
    val file = res.getOperandExistingFile(
      0,
      "Error expected jar file. Use (-h) to show command help."
    )
    JarUtils.showFiles(file.getPath)
  }

  val jarResources = new OptCommand(
    name = "jar-rs",
    usage = "<FILE.jar>",
    desc = "Show resources of a jar file ignoring *.class files."
  ).setAction{ res =>
    val file = res.getOperandExistingFile(
      0,
      "Error expected jar file. Use (-h) to show command help."
    )
    JarUtils.getAssetFiles(file.getPath) foreach println
  }

  val jarCat = new OptCommand(
    name = "jar-cat",
    usage = "<FILE.jar> <FILE>",
    desc = "Show content of a file in a jar package."
  ).setAction{ res =>
    val jarFile = res.getOperandOrError(0, "Error: missing jar file.")
    val file    = res.getOperandOrError(1, "Error: missing file name.")
    JarUtils.printFile(jarFile, file)
  }

  val jarExtract  = new OptCommand(
    name  = "jar-ex",
    usage =  "<FILE.jar> <file>",
    desc  = "Extract <file> from jar file <FILE.jar> to current directory."
  ).setAction{ res =>
    val jarFile = res.getOperandOrError(0, "Error: missing jar file. Use -h to show help.")
    val file    = res.getOperandOrError(1, "Error: missing file name. Use -h to show help.")
    JarUtils.extractFile(jarFile, file, ".")
  }

  val jarExtractAll = new OptCommand(
    name  = "jar-ex-all",
    usage = "<FILE.jar>",
    desc  = "Extract contents of <FILE.jar> to ·/<FILE> directory."
  ).setAction{ res =>
    val jarFile = res.getOperandOrError(0, "Error: missing jar file. Use -h or -help to show help.")
    val path = new java.io.File(jarFile)
      .getName()
      .stripSuffix(".jar")
    Utils.mkdir(path)
    JarUtils.extract(jarFile, path, true)
  }


  val jarToEXE = new OptCommand(
    name  = "jar-to-exe",
    usage = "[OPTIONS] <FILE.jar>",
    desc = "Embed Uber jar into Unix executable or Windows Executable (experimental).",
    example = """
 Note - <EXE> can be:
   + uexe - for Unix executable - Shell script with embedded uber-jar payload.
   + wcli - for Windows CLI command line executable. *.exe file.
   + wgui - for Windows GUI with user interface. -> *.exe file.
   """,
    helpFlag = true
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
    val inputJar  = res.getOperandOrError(0,
      "Error: missing input jar file. Use option -h to show help."
    )
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

  val digestStrOpt = new OptCommand(
    name  = "digest-s",
    usage = " <ALGORITHM> <STRING>",
    desc  = "Compute crypto hash of string. - Algorithm: [md5 | sha1 | sha256 ]"
  ).setAction{ res =>
    import jarget.crypto.Digest
    val algorithms = Map("md5" -> "MD5", "sha1" -> "SHA1", "sha256" -> "SHA-256")
    val op0 = res.getOperandOrError(0, "Error: missing algorithm: md5, sha1, or sha256.")
    val op1 = res.getOperandOrError(1, "Error: missing string.")
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

  val digestFileOpt = new OptCommand(
    name  = "digest-f",
    usage = " <ALGORITHM> <FILE>",
    desc  = "Compute crypto hash of a file. - Algorithm: [md5 | sha1 | sha256 ]",
    example = """ 
  $ jarget digest-f md5 jarget.jar 
  7d4515999a55857eeaf36e5fcbab39cd

  $ jarget digest-f sha256 config.pro 
  5f262848ee35add54a84c2c7e4413d45308d29210d4fe284c7d5730a252aed96
  """
  ).setAction{ res =>
    import jarget.crypto.Digest
    val algorithms = Map("md5" -> "MD5", "sha1" -> "SHA1", "sha256" -> "SHA-256")
    val op0 = res.getOperandOrError(0, "Error: missing algorithm: md5, sha1, or sha256.")
    val op1 = res.getOperandExistingFile(1, "Error: missing file parameter.")
    val alg = algorithms.get(op0) match {
      case Some(a) =>
        println(Digest.fileDigestSum(a, op1.getPath))              
      case None => {
        println("Error: algorithm not found.")
        System.exit(1)
      }
    }
  }

  val utilsOpt = new OptCommand(
    name     = "utils",
    usage    = "<ACTION>",
    desc     = "General utilities helpers for platform information and debugging.",
    helpFlag = true,
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
 Jarget 3.2 - command line toolbox for Scala and the Java Platform.
"""

  val parser = new OptParser(desc = desc)
    .add(new Separator("Main Commands"))
    .add(uberOptCommand)
    .add(execCommand)
    .add(scriptCommand)
    .add(scalaCommand)
    .add(runCommand)
    .add(new Separator("Mvn Commands"))
    .add(mvnShow)
    .add(mvnSearch)
    .add(mvnDoc)
    .add(mvnRunJar)
    .add(mvnRunCls)
    .add(mvnPom)
    .add(mvnPull)
    .add(mvnCopy)
    .add(cacheCommand)
    .add(new Separator("Jar Commands"))
    .add(jarToEXE)
    .add(jarManOpt)
    .add(jarMainClass)
    .add(jarShowFiles)
    .add(jarResources)
    .add(jarCat)
    .add(jarExtract)
    .add(new Separator("Misc Commands"))
    .add(utilsOpt)
    .add(digestStrOpt)
    .add(digestFileOpt)
  
  def main(args: Array[String]) : Unit  = 
    parser.parse(args.toList)
  
  
} // ------- End of object Main -------- //
