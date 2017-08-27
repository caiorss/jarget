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
package jarget.mvn  

import jarget.utils._

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

