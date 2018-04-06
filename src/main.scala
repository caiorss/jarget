package jarget.main

import jarget.utils.{Utils, JarUtils}
import jarget.utils.JarBuilder
import jarget.utils.OptParse
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

  /** Handles utils commands. - ./jarget utils <commands>  */
  def parseUtilsArgs(arglist: List[String]) = arglist match {

    // Show environment variable
    case List("-env") => Utils.showEnvironmentVars()

    case List("-env", evar)
        => for { v <- Option(System.getenv(evar)) } println(v)

     // Show Java properties
    case List("-prop")
        => Utils.showJavaProperties()

    case List("-prop", name)
        => Option(System.getProperty(name)) foreach println

    // Show PATH enviroment variable
    case List("-path")
        => for {
          pvar   <- Option(System.getenv("PATH"))
          sep    <- Option(System.getProperty("path.separator"))
          paths  = pvar.split(sep)
        } paths foreach println

    // Show Platform Info
    case List("-info")
        => showPlatformInfo()

    // Show path to executable in $PATH variable
    case List("-expath", program)
        => Utils.getProgramPath(program) foreach println

    case _
        => {
          println("Error: Invalid Utils Commands.")
          println("Valid commands: utils [-env | -prop | -path | -expath]")
          System.exit(1)
    }
  }


  /** Handles command Uber. ./jarget uber <options> */
  def parseUberArgs(arglist: List[String],  config: AppSettings, cachePath: String, cls: Class[_]) = {
    val parser = new OptParse()

    var wrapper: JarBuilder.JWrapper = JarBuilder.JWrapperEmpty
    var scala                       = false
    var output                      = "output.jar"
    var main:        Option[String] = None
    var paths:       List[String]   = List()
    var files:       List[String]   = List()
    var filesEntry:  List[String]   = List()
    var jarFiles:    List[String]   = List()
    var resources:   List[String]   = List()
    var packList:    List[PackData] = List()


    parser.addOption(
      "-scala",
      "Pack Scala library with the application.",
      arg => scala = arg.getFlag()
    )

    parser.addOption(
      "-exe",
      "Build self-executable jar file",
      arg => wrapper = JarBuilder.parseWrapper(arg.getOne())
    )

    parser.addOption(
      "-m",
      "Main file",
      true,
      arg =>  main = Some(arg.getOne())
    )


    parser.addOption(
      "-o",
      "Output file",
      arg => output = arg.getOne()
    )

    parser.addOption(
      "-jd",
      "Paths containing libraries (jar files)",
       arg => paths = arg.getOneOrMany()
    )

    parser.addOption(
      "-p",
      "Java package with maven coordinates <group>/<artifact>/<version>",
      false,
      arg => { packList = arg.getOneOrMany().map(parsePack)}
    )

    parser.addOption(
      "-j",
      "Additional jar files.",
      false,
      arg => jarFiles = arg.getOneOrMany()
    )

    parser.addOption(
      "-f",
      "Files to be added to the jar file",
      arg => files = arg.getOneOrMany()
    )

    parser.addOption(
      "-fe",
      "Files to appended to the jar file with entry separated by semicolon",
      arg => filesEntry = arg.getOneOrMany()
    )

    parser.addOption(
      "-r",
      "Resource directory.",
      arg => resources = arg.getOneOrMany()
    )

    try{
      parser.parseArgs(arglist)
      val packFiles =  Packget.getPackJarsFromCache(packList, cachePath, config.repoUrl)
      jarFiles = jarFiles ++ packFiles
    }
    catch {
      case ex: java.lang.IllegalArgumentException
          => {
            println(ex.getMessage)
            System.exit(1)
          }
    }

    main match {
      case Some(m)
          => {
            JarBuilder.makeUberJar(
              cls,
              output,
              m,
              paths,
              jarFiles,
              files,
              filesEntry,
              resources,
              scala,
              wrapper
            )
            println("Built file:  " + output + " ok")
            println("Run it with: $ java -jar " + output)
            System.exit(0)
          }
      case None
          => println("Error: missing main jar file.") ; System.exit(1)
    }
  } // End of uberParser


  /** Handles command digest to compute crypto hashes. 
      ./jarget digest -md5 -f <file> 
      ./jarget digest -sha256 -s <password>
   */
  def parseDigestArgs(args: List[String]) = {

    import jarget.crypto.Digest

    //println(args)

    args match {

      //----- File Digest ------------ //
      case List("-md5",    "-f", file)
          => println(Digest.fileDigestSum("MD5", file))
      case List("-sha1",   "-f", file)
          => println(Digest.fileDigestSum("SHA1", file))
      case List("-sha256", "-f", file)
          => println(Digest.fileDigestSum("SHA-256", file))

      case List("-md5",    "-f", file, hexDigest)
          => println(Digest.fileDigestSum("MD5", file) == hexDigest)
      case List("-sha1",   "-f", file, hexDigest)
          => println(Digest.fileDigestSum("SHA1", file) == hexDigest)
      case List("-sha256", "-f", file, hexDigest)
          => println(Digest.fileDigestSum("SHA-256", file) == hexDigest)


      // ------ String Digest -------- //
      case List("-md5",    "-s", str)
          => println(Digest.stringDigestSum("MD5", str))
      case List("-sha1",   "-s", str)
          => println(Digest.stringDigestSum("SHA1", str))
      case List("-sha256", "-s", str)
          => println(Digest.stringDigestSum("SHA-256", str))

      case List("-md5",    "-s", str, hexDigest)
          => println(Digest.stringDigestSum("MD5", str) == hexDigest)
      case List("-sha1",   "-s", str, hexDigest)
          => println(Digest.stringDigestSum("SHA1", str) == hexDigest)
      case List("-sha256", "-s", str, hexDigest)
          => println(Digest.stringDigestSum("SHA-256", str) == hexDigest)

      case _
          => {
            println("Error: Invalid digest option")
            System.exit(1)
          }
    }
  }



  /** Displays user help stored in the asset file user-help.txt 
    */
  def showHelp(version: String) = {    
    println(s"jarget ${version.trim()} -  Java platform Toolbox")
    val help = Utils.readResourceFile(getClass(), "/assets/user-help.txt")
    help match {
      case Some(file) => println(file)
      case None       => throw new java.io.IOException("Error: I can't find the resource file user-help.txt")
    }
  }


  def parseArgs(args: Array[String]) : Unit = {

    jarget.logger.Log.setLevel()

    val config =
      Utils.readResourceProperties("/assets/app.properties")
        .map(MainUtils.getAppSettings _ )
        .run(getClass())

    val cachePath = PackCache.getCacheHome(".jarget")

    args.toList match {

      case List() | List("-h") | List("-help")
          => showHelp(config.version)

      case List("-v") | List("-version")
          => println(config.version) 

      case List("-site")
        => Utils.openUrl(config.website)

      // --------  Utils Commands ------------------- //
      case "utils"::rest => parseUtilsArgs(rest)


     //--------- Pom Files Inspection ---------- //
      case "pom"::rest => rest foreach { uri => Pom.showPomDataFromUri(uri, true)}

     //------------  Make Uber Jar ------------- //

      // Turn an uber jar into a unix executable
      // that can be run with ./app instead of java -jar app.jar
      //
      case List("exe", exe, inputJar)
          => {
            val wrapper   = JarBuilder.parseWrapper(exe)
            val outputJar = wrapper match {
              case JarBuilder.JWrapperUEXE
                  => inputJar.stripSuffix(".jar")
              case JarBuilder.JWrapperWCLI | JarBuilder.JWrapperWGUI
                  => inputJar.stripSuffix(".jar") + ".exe"
              case JarBuilder.JWrapperEmpty
                  => throw new java.lang.IllegalArgumentException("Invalid jar wrapper option.")
            }
            JarBuilder.makeExecutable(getClass(), inputJar, outputJar, wrapper)
            println(s"Built ${outputJar}")
            println(s"Run it with ./${outputJar}")
          }

      case List("exe", exe, inputJar, outputJar)
          => {
            val wrapper = JarBuilder.parseWrapper(exe)
            JarBuilder.makeExecutable(getClass(), inputJar, outputJar, wrapper)
            println(s"Built ${outputJar}")
            println(s"Run it with ./${outputJar}")
          }

      case "uber"::rest
          => parseUberArgs(rest, config, cachePath, getClass())


      // ------- Class Path  ----------------- //

      case List("cpath", "-show")
          => println(JarUtils.getClasspath("./lib"))

      case List("cpath", "-show", path)
          => println(JarUtils.getClasspath(path))

     // ------- Crypto Utils -----------------------//

      case "digest"::rest
          => parseDigestArgs(rest)


     //-------- Package cache ----------------------- //

      // Show directory where are the cached packages, jar files and pom files.
      case List("cache", "-path")
          => println(cachePath)


      // Show all packages available in the cache repository 
      case List("cache", "-pack")
          => PackCache.getPackagesInCache(cachePath)
          .foreach { case (group, artifact) => println(s"${group}/${artifact}") }

      // Show all versions of some package available in the cache repository
      case List("cache", "-pack", packStr)
          => packStr.split("/") match {

            case Array(groupID, artifactID)
                => try {
                  PackCache.showPackageInfo(groupID, artifactID, cachePath)
                  PackCache.getPackageVersions(groupID, artifactID, cachePath)
                    .foreach{version =>
                    println (s"${groupID}/${artifactID}/${version}")
                  }
                } catch {
                  case _ : Throwable => println("Error: package not found")
                }
            case _
                => println("Error: Invalid package specification.")
          }


      case "cache"::"-cpath"::packList
          => println(Packget.getPackCPathFromCache(packList map parsePack, cachePath, config.repoUrl))

      // Show all jar files in the cache directory   
      case List("cache", "-jars")
          => PackCache.showJarFiles(cachePath)

      case "cache"::"-jars"::packList
          => Packget.getPackJarsFromCache(
            packList map parsePack,
            cachePath,
            config.repoUrl)
          .foreach(println)


      //-------- Generic Command with Classpath ------//

      case "exec"::pstr::"--"::command::args
          => tryMVNGet {
            val packList = pstr split(",") map parsePack toList
            val cpath = Packget.getPackCPathFromCache(packList, cachePath, config.repoUrl)
            JarUtils.runWithClassPath2(command, args, cpath)
          }

      case "script"::pstr::"--"::script::args
          => tryMVNGet {
            val packList = pstr split(",") map parsePack toList
            val cpath = Packget.getPackCPathFromCache(packList, cachePath, config.repoUrl)
            //println(s"Script = ${script} args = ${args}")
            JarUtils.runWithClassPath2("scala", "-save"::script::args, cpath)
          }

      case _ => println("Error: Invalid command")
    }

  }// -- End of function main() --- //

  import jarget.optParser.{OptResult, OptParser, OptSet}


  val config =
      Utils.readResourceProperties("/assets/app.properties")
        .map(MainUtils.getAppSettings _ )
        .run(getClass())

  val cachePath = PackCache.getCacheHome(".jarget")


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
    desc      = "MVN Coordinates of a java package."      
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
  ).setAction{ (res: OptResult) =>
    println("Results are  = ")
    println(res)
  }


  val parser = new OptParser()
  parser.add(uberOptSet){(res: OptResult) =>
    println("res = "  + res)    
    val scalaFlag = res.getFlag("scala")
    val packages  = res.getListStr("package")
    val files     = res.getListStr("file")
    val output    = res.getStr("output", "out.jar")
    val resourcesDirs = res.getListStr("resource")
    val mainJarFile = res.getOperands().head
    val jarFiles = res.getOperands.tail 

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
      jarFiles  = jarFiles ++ packFiles
    )
  }
  
  def main(args: Array[String]) : Unit  = {
    parser.parse(args.toList)
  }

  
} // ------- End of object Main -------- //
