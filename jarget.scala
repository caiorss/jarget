/** 
 - Author: Caio Rodrigues <caiorss.rodrigues [AT] gmail [DOT] com>


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

  */

case class PackData(group: String, artifact: String, version: String)

case class PomData(
  name:        String,
  url:         String,
  description: String,
  group:       String,
  artifact:    String,
  version:     String,
  packaging:   String
)

object Utils{

  import scala.concurrent.Future
  import concurrent.ExecutionContext.Implicits.global

  def mkdir(path: String) = {
    new java.io.File(path).mkdir()
  }

  def join(path1: String, path2: String) = {
    new java.io.File(path1, path2).getPath()
  }

  def fileExists(file: String) = {
    new java.io.File(file).isFile()
  }


  def downloadFile(fileUrl: String, file: String) = {
    val url = new java.net.URL(fileUrl)
    val rbc = java.nio.channels.Channels.newChannel(url.openStream())
    val fos = new java.io.FileOutputStream(file)
    fos.getChannel().transferFrom(rbc, 0, java.lang.Long.MAX_VALUE)
  }

  def getClipboardText() = {
    import java.awt.Toolkit
    import java.awt.datatransfer.{Clipboard, DataFlavor, Transferable}

    for {
      clip <- Option(Toolkit.getDefaultToolkit().getSystemClipboard())
      data <- Option(clip.getContents(null))
      if data.isDataFlavorSupported(DataFlavor.getTextPlainUnicodeFlavor())
      text = data.getTransferData(DataFlavor.stringFlavor).asInstanceOf[String]
    } yield text
  }

  def openUrl(uri: String){
    import java.awt.Desktop
    import java.io.IOException
    import java.net.URI
    import java.net.URISyntaxException
    val u = new URI(uri)
    val desktop = Desktop.getDesktop()
    desktop.browse(u)
  }


  def getScalaVersion() = util.Properties.versionNumberString

  /**  Pretty print a collection of tuples as a table.
    Parameters:
    @rows     - Collection of tuples to be printed.
    @title    - Tuple containing the titles of left and right side.
    @line     - Flag that if set to true, prints a new line between each row.
    @margin   - Margin from left side of screen as number of spaces.
    @sep      - Number of spaces between left side and right side
    @maxRside - Maximum number of characters to printed on right side.
    */
  def printTupleAsTable(
    rows:   Seq[(String, String)],
    title:  (String, String) = ("", ""),
    line:   Boolean = false,
    margin: Int = 0,
    sep:    Int = 4,
    maxRside: Int = 100
  ) = {

    def printRow(wmax1: Int, clamp: Boolean = true) = (row: (String, String)) => {
      val (lside, rside) = row

      // print left margin
      for (a <- 0 to margin) print(' ')

      print(lside)

      // Print spaces
      for (a <- 0 to wmax1 - lside.length + sep) print(' ')

      if (rside.length <= maxRside) {
        println(rside)
      } else if (clamp){
        val dots = "..."
        println(rside.take(maxRside - dots.length) + dots)
      } else {
        println(rside.take(maxRside))
      }

      // Print line between rows
      if (line) println()
    }

    def printDashes(wmax1: Int) = {
      printRow(wmax1, false)("-" * wmax1, "-" * maxRside)
    }

    val (title1, title2) = title

    // Maximum length of left side column
    val wmax1 = title1.length max rows.map(row => row._1.length).max

    printRow(wmax1)(title)
    printDashes(wmax1)
    rows foreach printRow(wmax1)
    printDashes(wmax1)
  }


  def showEnvironmentVars() {
    import scala.collection.JavaConverters._
    printTupleAsTable(
      rows     = System.getenv.asScala.toSeq,
      title    = ("Environment Variable", "Value"),
      margin   = 2,
      maxRside = 50
    )
  }

  def showJavaProperties() {
    import scala.collection.JavaConverters._
    printTupleAsTable(
      rows     = System.getProperties.asScala.toSeq,
      title    = ("Java Property", "Value"),
      margin   = 2,
      maxRside = 50
    )
  }

} /* ---- End of object Utils ------- */


object JarUtils{

  //import scala.collection.JavaConversions._

  def withJarException[A](comp: => Unit) = {
    try comp
    catch {
      case ex: java.util.zip.ZipException
          => println("Error, not a zip file")
      case ex: java.io.FileNotFoundException
          => println("Error: file not found.")
    }
  }

  def showManifest(file: String) = {
    val jar = new java.util.jar.JarFile(file)
    val man = jar.getManifest()
    man.write(System.out)
  }

  def getFilesAsList(jarFile: String) = {
    import scala.collection.JavaConverters._
    val jar = new java.util.jar.JarFile(jarFile)
    val files = jar.entries().asScala.toList
    jar.close()
    files
  }

  /** Get only asset files, disregarding all *.class files and directory entries. */
  def getAssetFiles(jarFile: String) = {
    val files = getFilesAsList(jarFile)
    files filter (f => !f.isDirectory() && !f.getName().endsWith(".class"))
  }

  def printFile(jarFile: String, file: String) = {
    val jar = new java.util.jar.JarFile(jarFile)
    val is = for {
      entry <- Option(jar.getEntry(file))
      is    =  jar.getInputStream(entry)
    } yield is

    is foreach { ist =>
      while(ist.available() > 0 ){
        System.out.write(ist.read())
      }
      ist.close()
    }
    jar.close()
  }

  /** Extract jar file to a given directory */
  def extractFile(jarFile: String, file: String, dest: String) = {
    val jar   = new java.util.jar.JarFile(jarFile)
    val entry = Option(jar.getEntry(file))

    entry match {
      case None    => println("Error: File not found.")
      case Some(e) => {
        val is = jar.getInputStream(e)
        val fileObj = new java.io.File(file)
        val fout = new java.io.File(dest, fileObj.getName())
        val os = new java.io.FileOutputStream(fout)
        while(is.available() > 0){
          os.write(is.read())
        }
        is.close()
        os.close()
      }
    }

    jar.close()
  }


  def getClasspath(path: String): String = {
    val files = new java.io.File(path)
      .listFiles()
      .toStream
      .filter(_.getName().endsWith(".jar"))

    files.foldLeft("."){ (acc: String, file: java.io.File) =>
       file.getPath() + ":" + acc
    }
  }

} // -------- End of object JarUtils ------------ //


object Packget { 

  def getCentralMavenArtifactURL(ext: String) = (pack: PackData) => {
    val gpath    = pack.group.replaceAll("\\.", "/")
    val artifact = pack.artifact
    val version  = pack.version
    s"http://central.maven.org/maven2/${gpath}/${artifact}/${version}/${artifact}-${version}.${ext}"
  }

  def getMavenPackgeURL(pack: PackData) = {
    s"https://mvnrepository.com/artifact/${pack.group}/${pack.artifact}/${pack.version}"
  }

  val getJarUrl  = getCentralMavenArtifactURL("jar")
  val getPomUrl  = getCentralMavenArtifactURL("pom")

  def getFileNameFull(pack: PackData, ext: String) = {
    s"${pack.artifact}-${pack.version}.${ext}"
  }

  def getPomXML(pack: PackData): scala.xml.Node = {
     scala.xml.XML.load(getPomUrl(pack))
  }

  def readPack(packstr: String) = {
    val fields = packstr.split("/").map(_.trim)
    fields match {
      case Array(group, artifact, version)
          => Some(PackData(group, artifact, version))
      case _
          => None
    }
  }

  def formatPack(pack: PackData) = {
    s"${pack.group}/${pack.artifact}/${pack.version}"
  }

  /**  Extract package data from Maven XML like this: 

    <!-- https://mvnrepository.com/artifact/org.json4s/json4s-native_2.11 -->
    <dependency>
        <groupId>org.json4s</groupId>
        <artifactId>json4s-native_2.11</artifactId>
        <version>3.5.3</version>
    </dependency>

    */
  def readPackMavenXML(xml: scala.xml.Node) = {
    PackData(
      group    = (xml \\ "dependency" \\ "groupId").text,
      artifact = (xml \\ "dependency" \\ "artifactId").text,
      version  = (xml \\ "dependency" \\ "version").text
    )
  }


  def getPomData(pom: scala.xml.Node) = {
    val nodes = pom.child
    val getText = (tag: String) => {
      nodes.find(_.label == tag).map(_.text).getOrElse("")
    }
    PomData(
      name        = getText("name"), 
      url         = getText("url"),
      description = getText("description"),
      group       = getText("groupId"),
      artifact    = getText("artifactId"),
      version     = getText("version"),
      packaging   = getText("packaging")
    )  
  }

  def getPomPackData(pom: scala.xml.Node) = {
    val nodes   = pom.child
    val getText = (tag: String) => nodes.find(_.label == tag).map(_.text)
    for {
      group     <- getText("groupId")
      artifact  <- getText("artifactId")
      version   <- getText("version")
    } yield PackData(group, artifact, version)
  }



  /**  Get a XML node like this:

          <dependency xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://maven.apache.org/POM/4.0.0">
                      <groupId>javax.servlet</groupId>
                      <artifactId>servlet-api</artifactId>
                      <version>2.5</version>
                      <scope>provided</scope>
                  </dependency>
  */
  def getPomDependencies(pom: scala.xml.Node) = {
    def getPomBasicDependencyData(node: scala.xml.Node) = {
      val ch = node.child
      for {
        group     <- ch.find(_.label == "groupId").map(_.text)
        artifact  <- ch.find(_.label == "artifactId").map(_.text)
        version   <- ch.find(_.label == "version").map(_.text)
        scope = ch.find(_.label == "scope")
        if scope.isEmpty    
      } yield PackData(group, artifact, version)
    }

    val deplist = for {
      depRoot <- pom.child.find(_.label == "dependencies")
      depsXml = depRoot.child.filter(_.label == "dependency")
      deps    = depsXml map getPomBasicDependencyData filter (!_.isEmpty) map(_.get)
    } yield deps
    deplist.getOrElse(List())
  }

  def getAllDependencies(pack: PackData) = {
    var packlist = Set[PackData]()
    packlist += pack   
    def aux(pack: PackData) {
      val xml  = getPomXML(pack)
      val deps = getPomDependencies(xml)
      deps foreach {d => packlist += d}
      deps foreach packlist
    }
    aux(pack)
    packlist
  }


  def downloadPackage(pack: PackData, path: String) = {
    import scala.concurrent.Future
    import concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration

    def downloadJarFile(p: PackData) = {
      val file = Utils.join(path, getFileNameFull(p, "jar"))
      val url  = getJarUrl(p)
      println(s"Downloading file ${file}.")            
      Utils.downloadFile(url, file)
      println(s"File ${file} downloaded. Ok.")
    }

    def downloadPomFile(p: PackData) = {
      val file = Utils.join(path, getFileNameFull(p, "pom"))
      val url  = getPomUrl(p)
      println(s"Downloading file ${file}.")            
      Utils.downloadFile(url, file)
      println(s"File ${file} downloaded. Ok.")      
    }

    Utils.mkdir(path)

    val packlist = getAllDependencies(pack) filter { p =>
      !Utils.fileExists(Utils.join(path, getFileNameFull(p, "jar")))
    }

    //println(packlist)

    val result = Future.traverse(packlist){ p =>
      println("Downloading package " + p)

      //println(s"Downloading file ${file} / ${pack.group} - ${pack.artifact} - ${pack.version} ... ")
      val fut = Future {
        downloadPomFile(p)
        downloadJarFile(p)
      }

      //fut onSuccess { case _ => println(s"File ${file} download Ok.") }
      //fut onFailure { case _ => println(s"File ${file} download Failed.")}
      fut 
    }

    result.onSuccess { case _ => println("Download Successful") }
    result.onFailure { case _ => println("Download Failed") }

    scala.concurrent.Await.result(result, Duration.Inf)
  }
  


} // ------ End of object Packget ------ // 


object Main{

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

  def showPomData(pack: PackData) = {
    val pom = Packget.getPomXML(pack)
    val dat = Packget.getPomData(pom)
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

  def showHelp() = println("""jarget - Tool to download jar packages.

Maven Packages / Jar Packages

 mvn -show [package]                 - Show package's information

 mvn -pom  [package]                 - Show package's pom file

 mvn -get  [package]                 - Download package and dependencies to ./lib

 mvn -get  [package] -dir [path]     - Download package and dependencies to [path] directory.

 mvn -get scala [version] [package]  - Download a Scala package for an specific Scala version.

 mvn -browse [package]               - Open package official website.

 mvn -go                             - Open web site https://mvnrepository.com
 mvn -go [package]                   - Open package mvnrepository url.

 mvn -clip -show                     - Extract package from XML in clipboard and show its information.
 mvn -clip -pom                      - Extract package from XML in clipboard and show its Pom file.
 mvn -clip -get                      - Extract package from XML in clipboard and download it to ./lib

Jar Files Inspection

 jar -manifest [jar]                - Show manifest.

 jar -assets                        - Show all asset files disregarding *.class files.

 jar -extract [jar] [file]          - Extract [file] from [jar] package to current directory.
 jar -extract [jar] [file] [path]   - Extract [file] from [jar] package to [path] directory.

 jar -cpath                         - Get classpath from ./lib directory
 jar -cpath [path]                  - Get classpath from [path] directory

System Information

 sys -env                            - Show environment variables
 sys -path                           - Show PATH environment variable
 sys -prop                           - Show java properties

Misc

 doc                                 - Open jarget's website - https://github.com/caiorss/jarget


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


  def main(args: Array[String]) : Unit = args.toList match {

    case List() | List("-h") | List("-help")
        => showHelp()

    case List("doc")
        => Utils.openUrl("https://github.com/caiorss/jarget")

    case List("mvn", "-pom", pstr)
        => showPom(parsePack(pstr))

    case List("mvn", "-show", pstr)
        => showPomData(parsePack(pstr))

    // Download a package and its dependencies
    case List("mvn", "-get", pstr)
        => Packget.downloadPackage(parsePack(pstr), "./lib")

    // Download a Scala package  
    case List("mvn", "-get", "scala", version, pstr)
        => Packget.downloadPackage(parseScalaPack(pstr, version), "./lib")

    case List("mvn", "-path", path, "-get", pstr)
        => Packget.downloadPackage(parsePack(pstr), path)

    case List("mvn", "-browse", pstr)
        =>   openUrl(parsePack(pstr))

    case List("mvn", "-go")
        => Utils.openUrl("https://mvnrepository.com")

    case List("mvn", "-go", pstr)
        => Utils.openUrl(Packget.getMavenPackgeURL(parsePack(pstr)))

    case List("mvn", "-clip", "-pom")
        => showPom(getPackMaven())

    case List("mvn", "-clip", "-show")
        => showPomData(getPackMaven())

    case List("mvn", "-clip", "-get")
        => Packget.downloadPackage(getPackMaven(), "./lib")

    // --------  System Commands ------------------- //

    // Show environment variable
    case List("sys", "-env")
        => Utils.showEnvironmentVars()

    // Show Java properties
    case List("sys", "-prop")
        => Utils.showJavaProperties()

    // Show PATH enviroment variable
    case List("sys", "-path")
        => for {
          pvar   <- Option(System.getenv("PATH"))
          sep    <- Option(System.getProperty("path.separator"))
          paths  = pvar.split(sep)
        } paths foreach println                  


    // -----------  Jar files  ------------------- //

    // Print Jar manifest file or "META-INF/MANIFEST.MF"
    case List("jar", "-manifest", jarFile)
        =>  JarUtils.withJarException{
          JarUtils.printFile(jarFile, "META-INF/MANIFEST.MF")
        }

    // Show only asset files ignoring class files.
    case List("jar", "-assets", jarFile)
        => JarUtils.withJarException{
          JarUtils.getAssetFiles(jarFile) foreach println
        }

    case List("jar", "-cat", jarFile, file)
        => JarUtils.withJarException{
          JarUtils.printFile(jarFile, file)
        }

    case List("jar", "-extract", jarFile, file)
        => JarUtils.withJarException{
          JarUtils.extractFile(jarFile, file, ".")
        }

    case List("jar", "-extract", jarFile, file, path)
        => JarUtils.withJarException{
          JarUtils.extractFile(jarFile, file, path)
        }

    case List("jar", "-cpath")
        => println(JarUtils.getClasspath("./lib"))

    case List("jar", "-cpath", path)
        => println(JarUtils.getClasspath(path))

    case _ => println("Error: Invalid command")
    }
  
} // ------- End of object Main -------- //

