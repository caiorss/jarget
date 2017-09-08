package jarget.main

import jarget.utils.{Utils, JarUtils, JarBuilder}
import jarget.utils.OptParse
import jarget.mvn._


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

  def showPacakgeInfo(pack: PackData) = {
    val pom = Packget.getPomXML(pack)
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

  def getVersion() = Utils.readResourceFile(
    getClass(),
    "/version.txt"
  )


} // ------ End of object MainUtils ---------- //


object Main{

  import MainUtils._


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

    // Show only asset files ignoring class files.
    case List("-assets", jarFile)
        => JarUtils.getAssetFiles(jarFile) foreach println

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


  def parseUberArgs(arglist: List[String]) = {
    val parser = new OptParse()

    var sh                        = false
    var scala                     = false
    var output                    = "output.jar"
    var main:      Option[String] = None
    var paths:     List[String]   = List()
    var files:     List[String]   = List()
    var jarFiles:  List[String]   = List()
    var resources: List[String]   = List()

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


  def showHelp() = {
    val version = MainUtils.getVersion() getOrElse ""
    println(s"jarget ${version.trim()} -  Java platform Toolbox")
    println("""
Maven Packages / Jar Packages

 mvn -show [package]                 - Show package's information

 mvn -pom  [package]                 - Show package's pom file

 mvn -get  [package]                 - Download package and dependencies to ./lib

 mvn -get  [package] -dir [path]     - Download package and dependencies to [path] directory.

 mvn -get scala [version] [package]  - Download a Scala package for an specific Scala version.

 mvn -search   [query]               - Search a package in search.maven.org, opening results in 
                                       the browser.

 mvn -search2  [query]               - Search a package showing results in the console. (Experimental)

 mvn -browse [package]               - Open package official website.

 mvn -go                             - Open web site https://mvnrepository.com
 mvn -go [package]                   - Open package mvnrepository url.

 mvn -clip -show                     - Extract package from XML in clipboard and show its information.
 mvn -clip -pom                      - Extract package from XML in clipboard and show its Pom file.
 mvn -clip -get                      - Extract package from XML in clipboard and download it to ./lib

Jar Files Inspection

 jar -man  [jar]                    - Show manifest.
 jar -main [jar]                    - Show main class.
 jar -show [jar]                    - Show all files.

 jar -assets [jar]                  - Show all asset files disregarding *.class files.

 jar -extract [jar] [file]          - Extract [file] from [jar] package to current directory.
 jar -extract [jar] [file] [path]   - Extract [file] from [jar] package to [path] directory.

 jar -extract-all [jar] [path]      - Extract all files from jar package to [path] directory.

 jar -extract-all [jar]             - Extract jar file to directory with same name of jar file 
                                      at current directory. If file is lib/chart.jar it will  
                                      extracted to ./chart 
    
Uber Jar 


 - Build an scala uber jar named output.jar from main.jar which
   contains the main class and lib1 and lib2 are the directories
   containing jar files. It bundles the scala-library.jar runtime with
   the application.

 $ jar uber -scala -o output.jar -m main.jar

 $ jar uber -scala -o output.jar -m main.jar -p ./lib1 ./lib2 
  
 $ jar uber -scala -sh -o output.jar -m main.jar -p ./lib1 -j dependency1.jar -r ./resources1 ./resources2 

 uber [command]

   -o [output file]         - (mandatory) Output jar file bundled with all dependencies.
   -m [main jar file]       - (optional)  Jar file containing main class.
   -p [path1] [path2] ...   - (optional)  Directories with jar file that will be included in the uber jar.
   -j [jar1.jar] [jar2.jar] - (optional)  Single jar files that will be included in the uber jar.
   -r [dir1] [dir2] ...     - (optional)  Directories containing resource files 
   -scala                   - (optional)  Bundles the scala-library.jar file with the uber.jar 
   -sh                      - (optional)  Makes the jar file as a self-executable Unix app.                                 

Classpath

 cpath -show                        - Get classpath from ./lib directory
 cpath -show [path]                 - Get classpath from [path] directory

Exec  

 exec [program] [path] 
 exec [program] -- arg1 arg2 ...      - Executes a program passing classpath (-cp) from ./lib to it.
 exec [program] [path] -- arg1 arg2 
  

System Information

 utils -doc                            - Open jarget's website - https://github.com/caiorss/jarget
 utils -env                            - Show environment variables in tabular format 
 utils -env [var]                      - Show environment variable [var]
 utils -path                           - Show PATH environment variable 
 utils -prop                           - Show java properties in tabular format 
 utils -expath [program]               - Show absolute path of a program in PATH variable


Note: [package] is <group>/<artifact>/<version>.
Examples of valid packages:

  - org.scalaz/scalaz-core_2.11/7.3.0-M15

  - org.jfree/jfreechart/1.0.17

Note: The XML in the clipboard is a maven coordinate: 

    <!-- https://mvnrepository.com/artifact/org.scalaz/scalaz-core_2.11 -->
    <dependency>
        <groupId>org.scalaz</groupId>
        <artifactId>scalaz-core_2.11</artifactId>
        <version>7.3.0-M15</version>
    </dependency>
"""
    )
  }


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
          => showPacakgeInfo(parsePack(pstr))

      // Download a package and its dependencies
      case List("mvn", "-get", pstr)
          => Packget.downloadPackage(parsePack(pstr), "./lib")

      // Download a Scala package  
      case List("mvn", "-get", "scala", version, pstr)
          => Packget.downloadPackage(parseScalaPack(pstr, version), "./lib")

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
          => showPacakgeInfo(getPackMaven())

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
      case "uber"::rest
          => parseUberArgs(rest)


      // ------- Class Path  ----------------- //

      case List("cpath", "-show")
          => println(JarUtils.getClasspath("./lib"))

      case List("cpath", "-show", path)
          => println(JarUtils.getClasspath(path))

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
