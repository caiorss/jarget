
// mvn -DoutputDirectory=./lib -DincludeArtifactIds=commons-logging,commons-io dependency:copy-dependencies


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


  def downloadFile(fileUrl: String, file: String) = Future {
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

} /* ---- End of object Utils ------- */


object JarTools{
  import scala.collection.JavaConversions._ 

  def showManifest(file: String) = {
    val jar = new java.util.jar.JarFile(file)
    val man = jar.getManifest()
    man.write(System.out)
  }  

}


object Packget { 

  def centralMavenURL(ext: String) = (pack: PackData) => { 
    val gpath    = pack.group.replaceAll("\\.", "/")
    val artifact = pack.artifact
    val version  = pack.version
    s"http://central.maven.org/maven2/${gpath}/${artifact}/${version}/${artifact}-${version}.${ext}"
  }

  val getJarUrl = centralMavenURL("jar")

  val getPomUrl  = centralMavenURL("pom")

  def getJarNameFull(pack: PackData) = {
    s"${pack.artifact}-${pack.version}.jar"
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

    Utils.mkdir(path)

    val packlist = getAllDependencies(pack) filter { p =>
      !Utils.fileExists(Utils.join(path, getJarNameFull(p)))
    }

    //println(packlist)

    val result = Future.traverse(packlist){ p =>
      println("Download package " + p)

      val file = Utils.join(path, getJarNameFull(p))
      val url  = getJarUrl(p)
      //println(s"Downloading file ${file} / ${pack.group} - ${pack.artifact} - ${pack.version} ... ")
      val fut = Utils.downloadFile(url, file)
      fut onSuccess { case _ => println(s"File ${file} download Ok.") }
      fut onFailure { case _ => println(s"File ${file} download Failed.")}
      fut 
    }

    result.onSuccess { case _ => println("Download Successful") }
    result.onFailure { case _ => println("Download Failed") }

    scala.concurrent.Await.result(result, Duration.Inf)
  }
  


} // ------ End of object Packget ------ // 


object Main{
  val packJFree = PackData("org.jfree", "jfreechart", "1.0.17")
  val packGUava = PackData("com.google.guava", "guava", "23.0")

  def parsePack(pstr: String) = {
    val p = Packget.readPack(pstr)
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



  def main(args: Array[String]) : Unit = args.toList match {

    case List("-pom", pstr)    => showPom(parsePack(pstr))
    case List("-show", pstr)   => showPomData(parsePack(pstr))
    case List("-get", pstr)    => Packget.downloadPackage(parsePack(pstr), "./lib")
    case List("-browse", pstr) =>   openUrl(parsePack(pstr))


    case List("-clip", "-pom")  => showPom(getPackMaven())
    case List("-clip", "-show") => showPomData(getPackMaven())
    case List("-clip", "-get")  => Packget.downloadPackage(getPackMaven(), "./lib")

    case List("-mvn")           => Utils.openUrl("https://mvnrepository.com")

    case _ => println("Error: Invalid option")
    }
  
} // ------- End of object Main -------- //

