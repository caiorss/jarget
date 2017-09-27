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

  */
package jarget.mvn  

import jarget.utils._
import jarget.logger.Log.logger

/** Java Package Data  */
case class PackData(
  group:    String,
  artifact: String,
  version:  String
)

/** Data from the POM file - Project Object Model */
case class PomData(
  name:        String,
  url:         String,
  description: String,
  group:       String,
  artifact:    String,
  version:     String,
  packaging:   String
)

/** Extraction of data from POM files */
object Pom{

    /** Extract Pom data from xml file */
  def getPomData(pom: scala.xml.Node): PomData = {
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


  /** Extract package data from pom xml file. */
  def getPomPackData(pom: scala.xml.Node): Option[PackData] = {
    val nodes   = pom.child
    val getText = (tag: String) => nodes.find(_.label == tag).map(_.text)
    for {
      group     <- getText("groupId")
      artifact  <- getText("artifactId")
      version   <- getText("version")
    } yield PackData(group, artifact, version)
  }

  /** Display attributes of a POM file in a summarized way */
  def showPomData(dat: PomData) = {
    println( "Package:         " + dat.name)
    println( "Packaging:       " + dat.packaging)
    println(s"Coordinates[1]:  group = ${dat.group} artifact = ${dat.artifact} version = ${dat.version}")
    println(s"Coordinates[2]:  ${dat.group}/${dat.artifact}/${dat.version}")
    println( "Url:             " + dat.url)
    println( "Description:     " + dat.description)
    println("\n")
  }

  def showPomDataFromXml(pom: scala.xml.Node) = {
    val dat = getPomData(pom)
    showPomData(dat)
  }

  /** Display attributes of a POM Xml in the same way as showPomData */
  def showPomDataFromUri(uri: String, showUri: Boolean = false) = {
    val doc = scala.xml.XML.load(uri)
    if (showUri) println("Uri = " + uri)
    showPomData(Pom.getPomData(doc))
    println("\n")
  }  

} // -------- End of object Pom -------- //


object CentralMaven{

  def makeRepositoryUrl(repoUrl: String) = (ext: String, pack: PackData) => {
    val gpath    = pack.group.replaceAll("\\.", "/")
    val artifact = pack.artifact
    val version  = pack.version
    s"${repoUrl}/${gpath}/${artifact}/${version}/${artifact}-${version}.${ext}"
  }

  val getArtifactUrl = makeRepositoryUrl("https://repo1.maven.org/maven2")

  def searchPackage(query: String, rows: Int = 20) = {
    import scala.concurrent.Future
    import concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration

    def getResultPackage(node: scala.xml.Node): Option[PackData] = {
      val nlist = node \\ "str"
      def nodeAttrEq(attr: String, value: String)(node: scala.xml.Node) = {
        node.attributes.get(attr) exists (_.text == value)
      }
      def findNode(name: String) = {
        nlist find nodeAttrEq("name", name) map (_.text)
      }
      for {
        dat <- findNode("id")
        Array(groupId, artifactId) = dat.split(":")
        ver <- findNode("latestVersion")
      } yield PackData(groupId, artifactId, ver)
    }

    def getCentralMavenPom(p: PackData) = {
      val uri  = getArtifactUrl("pom", p)
      val pom  =  scala.xml.XML.load(uri)
      Pom.getPomData(pom)
    }

    val url = s"http://search.maven.org/solrsearch/select?q=${query}&rows=${rows}&wt=xml"
    val xml = scala.xml.XML.load(url)
    val packs = (xml \\ "doc") map { d => getResultPackage(d).get }

    val result = Future.traverse(packs) { p =>

      logger.info("Getting package " + p)

      val fut = Future {
        try Some(getCentralMavenPom(p))
        catch {
          case ex: java.io.FileNotFoundException
              => {
                logger.warning("Failed to get package =" + p)
                None
              }
        }
      }
      fut onFailure { case _ =>  logger.warning("Failed to get package " + p) }
      fut onSuccess { case _ =>  logger.info("Got package's pom " + p) }
      fut
    }
    result.onSuccess { case pomList =>
      logger.fine("Show pom data from all packages")
      pomList foreach { pom =>
        pom match {
          case Some(p) => {
            logger.fine("Show POM data of " + p)
            Pom.showPomData(p)
          }
          case None    => logger.warning("Failed to get pom")
        }
      }
    }
    result.onFailure { case _ => println("Error: failed to get data") }

    logger.info("Waiting result to download")
    scala.concurrent.Await.result(result, Duration.Inf)
  }

  /** Search a package in Maven Central [[http://search.maven.org]] */
  def searchPackageBrowser(query: String) = {
    val encoded = java.net.URLEncoder.encode(s"search|ga|1|${query}", "UTF-8")
    jarget.utils.Utils.openUrl("http://search.maven.org#" + encoded)
  }

} // ------ End of CentralMaven ------------- //


object Packget {
  
  import Pom._
  import jarget.reader._

  // type Repo[A] = Reader[String, String]

  def getArtifactURL(ext: String) = (pack: PackData) => for {
    repo     <- Reader.ask[String]
    gpath    = pack.group.replaceAll("\\.", "/")
    artifact = pack.artifact
    version  = pack.version
    uri      = s"${repo}/${gpath}/${artifact}/${version}/${artifact}-${version}.${ext}"
  } yield uri

  def getMavenPackgeURL(pack: PackData) = {
    s"https://mvnrepository.com/artifact/${pack.group}/${pack.artifact}/${pack.version}"
  }

  val getJarUrl = getArtifactURL("jar")
  val getPomUrl = getArtifactURL("pom")

  def getFileNameFull(pack: PackData, ext: String) = {
    s"${pack.artifact}-${pack.version}.${ext}"
  }

  def getPomXML(pack: PackData) = 
    getPomUrl(pack) map scala.xml.XML.load
  

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
    var packlist = Set[PackData](pack)
    //packlist += pack
    for {    
      xml  <- getPomXML(pack)
      deps = getPomDependencies(xml)
      _    <- Reader.liftIO{ deps foreach (d => packlist += d)}
    } yield packlist
  }


  def downloadArtifact(pack: PackData, ext: String, path: String) =
    for (url <- getArtifactURL(ext)(pack)){
      val file  = Utils.join(path, getFileNameFull(pack, ext))
      println(s"Downloading file ${file}.")
      Utils.downloadFile(url, file)
      println(s"File ${file} downloaded. Ok.")
    }


  def downloadPackage(pack: PackData, path: String): Reader[String, Unit] = {
    import scala.concurrent.Future
    import concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration

    Utils.mkdir(path)

    println("Testing package")

    for(repoURL  <- Reader.ask[String] ){

      val packlist = getAllDependencies(pack) run (repoURL) filter { p =>
        !Utils.fileExists(Utils.join(path, getFileNameFull(p, "jar")))
      }

      println("Downloading ---------------------")
      packlist foreach println 
      println("----------------------------------")

      val result = Future.traverse(packlist){ p =>
        println("Downloading package " + p)
        //println(s"Downloading file ${file} / ${pack.group} - ${pack.artifact} - ${pack.version} ... ")
        val fut = Future {
          downloadArtifact(p, "pom", path).run(repoURL)
          downloadArtifact(p, "jar", path).run(repoURL)
        }
        fut
      }
      result.onSuccess { case _ => println("Download Successful") }
      result.onFailure { case _ => println("Download Failed") }
      scala.concurrent.Await.result(result, Duration.Inf)      

    }

  } // End of downloadPackage 
   

} // ------ End of object Packget ------ // 

