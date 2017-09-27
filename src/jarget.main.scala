package jarget.main

import jarget.utils.{Utils, JarUtils, JarBuilder}
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
    val p = Packget.readPack(pstr)
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
      .map(Packget.readPackMavenXML)

    if (p.isEmpty) {
      println("Error: Invalid maven XML package format")
      System.exit(1)
    }
    p.get 
  }

  def showPackageInfo(pack: PackData) = 
    for (pom <- Packget.getPomXML(pack)){
      val dat = Pom.getPomData(pom)
      println( "Package:         " + dat.name)
      println( "Packaging:       " + dat.packaging)
      println(s"Coordinates[1]:  group = ${dat.group} artifact = ${dat.artifact} version = ${dat.version}")
      println(s"Coordinates[2]:  ${dat.group}/${dat.artifact}/${dat.version}")
      println( "Url:             " + dat.url)
      println( "Description:     " + dat.description)

      println("\nDependencies:\n")
      Packget.getPomDependencies(pom) foreach { p =>
        println("  - " + Packget.formatPack(p) + "\n")
      }
    }
  


  def showPom(pack: PackData) = {
    println(Packget.getPomXML(pack))
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
      repoUrl <- Option(prop.getProperty("jarget.mvn.url"))
      website <- Option(prop.getProperty("jarget.website"))
    } yield AppSettings(version, repoUrl, website)
    sopt match {
      case Some(s)
          => s
      case None
          => throw new java.lang.IllegalArgumentException("Error: cannot read property file")
    }
  }

} // ------ End of object MainUtils ---------- //


object Main{

  import MainUtils._

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

    // Show path to executable in $PATH variable
    case List("-expath", program)
        => Utils.getProgramPath(program) foreach println

    case List("-doc")
        => Utils.openUrl("https://github.com/caiorss/jarget")

    case _
        => {
          println("Error: Invalid Utils Commands.")
          println("Valid commands: utils [-env | -prop | -path | -expath]")
          System.exit(1)
    }
  }

  /** Handles jar commands. - ./jarget jar <commands> */
  def parseJarArgs(arglist: List[String]) = arglist match {

    case List()
        => println("Jar commands: [ -man | -main | -show | -cat | -assets | -extract | -extract-all ]")

    // Print Jar manifest file or "META-INF/MANIFEST.MF"
    case List("-man", jarFile)
        => JarUtils.showManifest(jarFile)

    // Pint Main class
    case List("-main", jarFile)
        => JarUtils.getMainClass(jarFile) foreach println

    case List("-show", jarFile)
        => JarUtils.showFiles(jarFile)
      
    case List("-package", jarFile)
        => JarUtils.getPackages(jarFile) foreach println

    case List("-package", jarFile, clsName)
        => JarUtils.getPackageClasses(jarFile, clsName) foreach println

    // Show only asset files ignoring class files.
    case List("-resource", jarFile)
        => JarUtils.getAssetFiles(jarFile) foreach println

    case List("-resource", jarFile, file)
        => JarUtils.printFile(jarFile, file)

    case List("-cat", jarFile, file)
        => JarUtils.printFile(jarFile, file)

    case List("-extract", jarFile, file)
        => JarUtils.extractFile(jarFile, file, ".")

    case List("-extract", jarFile, file, path)
        => JarUtils.extractFile(jarFile, file, path)

    case List("-extract-all", jarFile)
        => {
          val path = new java.io.File(jarFile)
            .getName()
            .stripSuffix(".jar")
          Utils.mkdir(path)
          JarUtils.extract(jarFile, path, true)

        }

    case List("-extract-all", jarFile, path)
        => JarUtils.extract(jarFile, path, true)

    case _ => println("Error: Invalid jar argument")

  } //------- EOF function parseJarArgs ------- //


  /** Handles command Uber. ./jarget uber <options> */
  def parseUberArgs(arglist: List[String]) = {
    val parser = new OptParse()

    var sh                          = false
    var scala                       = false
    var output                      = "output.jar"
    var main:        Option[String] = None
    var paths:       List[String]   = List()
    var files:       List[String]   = List()
    var filesEntry:  List[String]   = List()
    var jarFiles:    List[String]   = List()
    var resources:   List[String]   = List()

    parser.addOption(
      "-scala",
      "Pack Scala library with the application.",
      arg => scala = arg.getFlag()
    )

    parser.addOption(
      "-sh",
      "Build self-executable jar file",
      arg => sh = true
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
      "-p",
      "Paths containing libraries",
       arg => paths = arg.getOneOrMany()
    )

    parser.addOption(
      "-j",
      "Additional jar files.",
      false,
      arg => jarFiles = arg.getOneOrMany()
    )

    parser.addOption(
      "-f",
      "Files to appended to the jar file",
      arg => files = arg.getOneOrMany()
    )

    parser.addOption(
      "-fe",
      "Files to appended to the jar file with entry separated by semicolon",
      arg => filesEntry = arg.getOneOrMany()
    )

    parser.addOption(
      "-r",
      "Resource directories.",
      arg => resources = arg.getOneOrMany()
    )

    try
      parser.parseArgs(arglist)
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
              output,
              m,
              paths,
              jarFiles,
              files,
              filesEntry,
              resources,
              scala,
              sh
            )
            println("Built file:  " + output + " ok")
            println("Run it with: $ java -jar " + output)
            System.exit(0)
          }
      case None
          => println("Error: missing main file ") ; System.exit(1)
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
  def showHelp() = {
    val version = MainUtils.getVersion() getOrElse ""
    println(s"jarget ${version.trim()} -  Java platform Toolbox")
    val help = Utils.readResourceFile(getClass(), "/assets/user-help.txt")
    help match {
      case Some(file) => println(file)
      case None       => throw new java.io.IOException("Error: I can't find the resource file user-help.txt")
    }
  }

  val centralMaven = "http://central.maven.org/maven2"

  def main(args: Array[String]) : Unit = {

    jarget.logger.Log.setLevel()

    args.toList match {

      case List() | List("-h") | List("-help")
          => showHelp()

      case List("-v") | List("-version")
          => for { 
            ver <- MainUtils.getVersion()
          } println("Jarget v" + ver)

      case List("mvn", "-pom", pstr)
          => showPom(parsePack(pstr))

      case List("mvn", "-show", pstr)
          => showPackageInfo(parsePack(pstr)) run centralMaven

      case List("mvn", "-show", pstr, "-r", repo)
          => showPackageInfo(parsePack(pstr)) run repo 

      // Download a package and its dependencies
      case List("mvn", "-get", pstr)
          => {
            println("Downloading Packages")
            Packget.downloadPackage(parsePack(pstr), "./lib") run (centralMaven)
          }

      case List("mvn", "-get", pstr, "-r", repo)
          => {
            println("Downloading Packages")
            Packget.downloadPackage(parsePack(pstr), "./lib") run (repo)
          }
        

      // Download a Scala package  
      case List("mvn", "-get", "scala", version, pstr)
          => {
            val pack = parseScalaPack(pstr, version)
            Packget.downloadPackage(pack, "./lib") run (centralMaven)            
          }

      case List("mvn", "-path", path, "-get", pstr)
          => Packget.downloadPackage(parsePack(pstr), path)

      case List("mvn", "-search", query)
          => CentralMaven.searchPackageBrowser(query)

      case List("mvn", "-search2", query)
          => CentralMaven.searchPackage(query)

      case List("mvn", "-search2", query, n)
          => CentralMaven.searchPackage(query, n.toInt)

      case List("mvn", "-browse", pstr)
          => openUrl(parsePack(pstr))

      case List("mvn", "-go")
          => Utils.openUrl("https://mvnrepository.com")

      case List("mvn", "-go", pstr)
          => Utils.openUrl(Packget.getMavenPackgeURL(parsePack(pstr)))

      case List("mvn", "-clip", "-pom")
          => showPom(getPackMaven())

      case List("mvn", "-clip", "-show")
          => showPackageInfo(getPackMaven())

      case List("mvn", "-clip", "-get")
          => Packget.downloadPackage(getPackMaven(), "./lib")

      // --------  Utils Commands ------------------- //

      case "utils"::rest => parseUtilsArgs(rest)

      // ------ Jar package inspection and manipulation -- //
      case "jar"::rest
          =>  JarUtils.withJarException{ parseJarArgs(rest) }

     //--------- Pom Files Inspection ---------- //

      case "pom"::rest => rest foreach { uri => Pom.showPomDataFromUri(uri, true)}

     //------------  Make Uber Jar ------------- //

      // Turn an uber jar into a unix executable
      // that can be run with ./app instead of java -jar app.jar
      //
      case List("uber", "-exjar", inputJar)
          => {
            val outputJar = inputJar.stripSuffix(".jar")
            JarBuilder.makeExecutableJar(inputJar, outputJar)
            println(s"Built ${outputJar}")
            println(s"Run it with ./${outputJar}")
          }

      case List("uber", "-exjar", inputJar, outputJar)
          => {
            JarBuilder.makeExecutableJar(inputJar, outputJar)
            println(s"Built ${outputJar}")
            println(s"Run it with ./${outputJar}")
          }

      case "uber"::rest
          => parseUberArgs(rest)


      // ------- Class Path  ----------------- //

      case List("cpath", "-show")
          => println(JarUtils.getClasspath("./lib"))

      case List("cpath", "-show", path)
          => println(JarUtils.getClasspath(path))

     // ------- Crypto Utils -----------------------//

      case "digest"::rest
          => parseDigestArgs(rest)

      //-------- Generic Command with Classpath ------//

      // run generic command as ./command -cp $CLASSPATH arg1 arg2 arg2 ...
      case List("exec", command)
          => JarUtils.runWithClassPath(command, List(), "./lib")

      case List("exec", command, path)
          => JarUtils.runWithClassPath(command, List(), path)

      case "exec"::command::"--"::args
          => JarUtils.runWithClassPath(command, args, "./lib")

      case "exec"::command::path::"--"::args  
          => JarUtils.runWithClassPath(command, args, path)

      case _ => println("Error: Invalid command")
    }

  }// -- End of function main() --- //

  
} // ------- End of object Main -------- //
