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
  def parseUberArgs(arglist: List[String],  config: AppSettings, cachePath: String) = {
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
    var packList:    List[PackData] = List()


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


  def parseMvnArgs(args: List[String], config: AppSettings, cachePath: String ) = {

    val repoUrl = Option(System.getenv("jarget.url")) getOrElse config.repoUrl

    def getLibPath(path: String) = Option(System.getenv("jarget.path")) getOrElse path 

    // Parse package list separated by (;)
    //
    def parsePackageList(plist: String) =
      plist.split(",").map(parsePack).toList 

    args.toList match {

      case List("-pom", pstr)
          => showPom(parsePack(pstr)) run repoUrl


      case List("-show", pstr)
          => showPackageInfo(parsePack(pstr)) run repoUrl 

     //  Copy packages from cache directory to ./lib and download it
     //  if has not been downloaded yet.
      case List("-copy", pstr)
          => tryMVNGet {
            val packs = parsePackageList(pstr)
            Packget.copyPackageFromCache(packs, cachePath, repoUrl, getLibPath("./lib"))
          }

      // Pull packages from remote repository to package cache.  
      case List("-pull", pstr)
          => tryMVNGet {
            val packs = parsePackageList(pstr)
            Packget.getPackJarsFromCache(packs, cachePath, repoUrl)
          }

      // Clean cache packages
      case List("-clear")
          => tryMVNGet {
            println("Cleaning cache")
            Utils.deleteDirectory(cachePath, true)
          }      
 
      case List("-search", query)
          => {
            val q = java.net.URLEncoder.encode(query)
            Utils.openUrl("https://mvnrepository.com/search?q=" + q)
          }

      case List("-search2", query)
          => PackSearch.searchPackage(query)

      case List("-search2", query, n)
          => PackSearch.searchPackage(query, n.toInt)

      case List("-browse", pstr)
          => openUrl(parsePack(pstr))

       // Open package documentation in browser  
      case List("-open")
          => Utils.openUrl("https://mvnrepository.com")

       // Open package documentation in browser          
      case List("-open", pstr)
          => {
            val pack = parsePack(pstr)
            val url  = s"https://mvnrepository.com/artifact/${pack.group}/${pack.artifact}/${pack.version}"
            Utils.openUrl(url)
          }

      case _ => println("Error: invalid command.")
        
    }
  } // ----- End of parseMvnArgs ---------- //

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


  def main(args: Array[String]) : Unit = {

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

      //--------- Mvn commands ------------------ //
      case "mvn"::rest  => parseMvnArgs(rest, config, cachePath)

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
          => parseUberArgs(rest, config, cachePath)


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

  
} // ------- End of object Main -------- //