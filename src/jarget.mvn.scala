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


case class RepoConf(
  cache:   String,
  repoUrl: String
)


/** Encodes java package specification.

   Example: The package org.jfree:jfreechart:1.0.17 is encoded as 
   PackData("org.jfree", "jfreechart", "1.0.17")


   Example: Package's operations. 

   {{{
    $ scala -cp jarget.jar
    
    import jarget.mvn.PackData
    import jarget.mvn.Packget 
 
    scala> val p = PackData("org.jfree", "jfreechart", "1.0.17")
    p: jarget.mvn.PackData = PackData(org.jfree,jfreechart,1.0.17)

    scala> p.group
    res0: String = org.jfree

    scala> p.artifact
    res1: String = jfreechart

    scala> p.version
    res2: String = 1.0.17


    scala> p.format 
    res3: String = org.jfree/jfreechart/1.0.17


    scala> p.getArtifactFile("jar")
    res4: String = jfreechart-1.0.17.jar

    scala> p.getArtifactFile("pom")
    res5: String = jfreechart-1.0.17.pom

    scala> p.getArtifactFile("md5")
    res6: String = jfreechart-1.0.17.md5


   }}} 

   @param group    - Package's groupID. 
   @param artifact - Package's artifactID
   @param version  - Package's version.
  */
case class PackData(
  group:    String,
  artifact: String,
  version:  String

) {
  import jarget.reader.Reader

  /** Format package to string 

   Example: 
   {{{  
    $ scala -cp jarget.jar
    
    import jarget.mvn.PackData
    import jarget.mvn.Packget   
    
    scala> val p = PackData("org.xerial", "sqlite-jdbc", "3.20.0")
    p: jarget.mvn.PackData = PackData(org.xerial,sqlite-jdbc,3.20.0)

    scala> p.format 
    res3: String = org.xerial/sqlite-jdbc/3.20.0
   }}}
   */ 
  def format() = {
    s"${this.group}/${this.artifact}/${this.version}"
  }


  /** Build artifact file names from package data.

    Example: 
    {{{ 
    $ scala -cp jarget.jar
    
    import jarget.mvn.PackData
    import jarget.mvn.Packget   
    
    scala> val p = PackData("org.xerial", "sqlite-jdbc", "3.20.0")
    p: jarget.mvn.PackData = PackData(org.xerial,sqlite-jdbc,3.20.0)
    
   scala> p.getArtifactFile("jar")
   res0: String = sqlite-jdbc-3.20.0.jar

   scala> p.getArtifactFile("pom")
   res1: String = sqlite-jdbc-3.20.0.pom

   scala> p.getArtifactFile("md5")
   res2: String = sqlite-jdbc-3.20.0.md5
    }}}
   */ 
  def getArtifactFile(ext: String) = {
    s"${this.artifact}-${this.version}.${ext}"
  }

  /** Get path to artifact *.jar or *.pom file of a given package.*/
  def getArtifactURI(ext: String) = for {
    repo     <- Reader.ask[String]
    gpath    = this.group.replaceAll("\\.", "/")
    artifact = this.artifact
    version  = this.version
    uri      = s"${repo}/${gpath}/${artifact}/${version}/${artifact}-${version}.${ext}"
  } yield uri

  /** Get URL or Path to package's Jar file */
  def getJarURI = this.getArtifactURI("jar")

  /** Get URL or Path to package's POM file */
  def getPomURI = this.getArtifactURI("pom")

  /** Get package's pom XML */
  def getPomXML() = 
    this.getPomURI map scala.xml.XML.load
  

} //---------- End of object PackData ------------- // 


object PackData{

  /** Read package from string */ 
  def read(packstr: String, separator: String) = {
    val fields = packstr.split(separator).map(_.trim)
    fields match {
      case Array(group, artifact, version)
          => Some(PackData(group, artifact, version))
      case _
          => None
    }
  }

  /**  Read package from a maven XML specification like the one below.

    {{{
    <!-- https://mvnrepository.com/artifact/org.json4s/json4s-native_2.11 -->
    <dependency>
        <groupId>org.json4s</groupId>
        <artifactId>json4s-native_2.11</artifactId>
        <version>3.5.3</version>
    </dependency>
    }}}
    */
  def readFromXML(xml: scala.xml.Node) = {
    PackData(
      group    = (xml \\ "dependency" \\ "groupId").text,
      artifact = (xml \\ "dependency" \\ "artifactId").text,
      version  = (xml \\ "dependency" \\ "version").text
    )
  }

} //------- End of object PackData -------------- // 


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
  } // ----- End of   getPomDependencies --- //


  /** Display attributes of a POM file in a summarized way */
  def showPomData(dat: PomData) = {
    println( "Package:         " + dat.name)
    println( "Packaging:       " + dat.packaging)
    println(s"Coordinates[1]:  group = ${dat.group} artifact = ${dat.artifact} version = ${dat.version}")
    println(s"Coordinates[2]:  ${dat.group}/${dat.artifact}/${dat.version}")
    println( "Url:             " + dat.url)
    println( "\nDescription:     \n" + dat.description.trim)
    println("-----------------------------------------------")
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


object PackCache {

  /** Get path to packages cache directory in ~/<prefix> directory.  */ 
  def getCacheHome(prefix: String) =
    Utils.joinPathList(System.getProperty("user.home"), prefix, "cache")

  /** Get path to directory or URL directory which contains the jar file. */
  def getPackagePath(cachePath: String, pack: PackData) = {
    val repo     = cachePath
    val gpath    = pack.group.replaceAll("\\.", "/")
    val artifact = pack.artifact
    val version  = pack.version
    // s"${gpath}/${artifact}/${version}/${artifact}-${version}"
    Utils.joinPathList(
      cachePath,
      gpath,
      artifact,
      version
    )
  }

  def getPackageVersions(groupID: String, artifactID: String, cachePath: String) = {
    val repo     = cachePath
    val gpath    = groupID.replaceAll("\\.", "/")
    val path = Utils.joinPathList(cachePath, gpath, artifactID)
    (new java.io.File(path)).listFiles map (_.getName) toList
  }


  /** Get all packages available in the cache. */
  def getPackagesInCache(cachePath: String) = {
    import java.nio.file.{Files, Paths, Path}
    val root = Paths.get(cachePath)
    def getPack(p: Path) = {
      val k = root relativize p getParent() getParent()
      (k.getParent.toString.replace("/", "."), k.getFileName.toString)
    }
    Files.walk(root)
      .filter(_.toString().endsWith(".jar"))
      .toArray
      .map(p => getPack(p.asInstanceOf[java.nio.file.Path]))
      .toSet 
  }

  def getArtifactPath(cachePath: String, ext: String, pack: PackData) = {
    val path = getPackagePath(cachePath, pack)
    val file =  s"${pack.artifact}-${pack.version}.${ext}"
    Utils.join(path, file)
  }

  def showPackageInfo(groupID: String, artifactID: String, cachePath: String) = {
    val repo     = cachePath
    val gpath    = groupID.replaceAll("\\.", "/")
    val path     = Utils.joinPathList(cachePath, gpath, artifactID)
    val version  = (new java.io.File(path))
      .listFiles
      .filter(_.isDirectory)
      .head
      .getName()
    val pomFile = Utils.joinPathList(
      path,
      version,
      s"${artifactID}-${version}.pom"
    )
    Pom.showPomDataFromUri(pomFile, showUri = true)
  }

  /** Check if package exists in cache */
  def exists(cachePath: String, pack: PackData) = 
    Utils.fileExists(getArtifactPath(cachePath, "jar", pack))

  /** Show all jar files in the cache repository */
  def showJarFiles(cachePath: String) = {
    import java.nio.file.{Files, Paths}
    Files.walk(Paths.get(cachePath)).filter(_.toString().endsWith(".jar")).forEach(println)
  }

} //---------- End of PackCache object ---------- // 


object Packget {
  
  import Pom._
  import jarget.reader._

  def getAllDependencies(pack: PackData):  Reader[String, Set[PackData]] = {
    var packlist = Set[PackData](pack)
    //packlist += pack
    for {    
      xml  <- pack.getPomXML
      deps = getPomDependencies(xml)
      _    <- Reader.liftIO{ deps foreach (d => packlist += d)}
    } yield packlist
  }


  def getAllDependenciesFromCache(cache: String, pack: PackData) = {
    var packlist = Set[PackData](pack)
    val pomFile = PackCache.getArtifactPath(cache, "pom", pack)
    val xml = scala.xml.XML.loadFile(pomFile)
    val deps = getPomDependencies(xml)
    deps foreach (d => packlist += d)
    packlist
  } 

  /** Download artifact with extension jar or pom to a given path. 

      @param pack  - Package that will be download 
      @param ext   - File extension - 'jar', 'pom', 'md5', 'sha256' and etc.
      @param path  - Path where the artifact will be stored. 

      Example: It will download the package's jar artifact and store it in 
               ./download/jfreechart-1.0.17.jar 
      {{{ 
         val p    = PackData("org.jfree", "jfreechart", "1.0.17")
         val repo =  "http://central.maven.org/maven2"
         val path = "./downloads"
         Packget.downloadArtifact(p, "jar", path) run(repo)         
      }}}

    */ 
  def downloadArtifact(pack: PackData,
                       ext: String,
                       path: String): Reader[String, Unit] =
    for (url <- pack.getArtifactURI(ext)){
      val file  = Utils.join(path, pack.getArtifactFile(ext))
      println(s"Downloading file ${file}.")
      Utils.downloadFile(url, file)
      println(s"File ${file} downloaded. Ok.")
    }


  /** Download package from cache directory if it is not downloaded yet.*/
  def downloadPackageToCache(cache: String, pack: PackData):  Reader[String, Unit] = {
    import scala.concurrent.Future
    import concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration.Duration

    Utils.mkdir(cache)
    val packDir = PackCache.getPackagePath(cache, pack)
    val packJar = PackCache.getArtifactPath(cache, "jar", pack)
    
    for(repoURL  <- Reader.ask[String])
      if (!Utils.fileExists(packJar)){

        // Create package's directory
        Utils.mkdir(packDir)

        // Select package that aren't in the cache yet.
        val packlist = getAllDependencies(pack) run (repoURL) filter { p =>
          val path = PackCache.getPackagePath(cache, p)
          !Utils.fileExists(Utils.join(path, p.getArtifactFile("jar")))
        }

        println("Downloading ---------------------")
        packlist foreach println
        println("----------------------------------")

        val result = Future.traverse(packlist){ p =>
          // 1println("Downloading package " + p)
          val path = PackCache.getPackagePath(cache, p)
          Utils.mkdir(path)
          println("Package path = " + p)

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

  } // End of function downloadPackgeToCache 


  /** Get list of paths to package's jar files in the cache. */
  def getPackJarsFromCache(packList: List[PackData], cache: String, repoUrl: String) = {
    def aux(pack: PackData) = {
      if(!PackCache.exists(cache, pack))
        downloadPackageToCache(cache, pack) run repoUrl
      val packs = getAllDependenciesFromCache(cache, pack)
      packs map { p => PackCache.getArtifactPath(cache, "jar", p) }
    }
    (packList flatMap aux).toSet.toList
  }

  /** Get Classpath string of all required packages' jar files in the
      cache. If the packages aren't available in the cache, they will
      be downloaded from a remote repository.
   */
  def getPackCPathFromCache(packList: List[PackData], cache: String, repoUrl: String) = {
    val sep = System.getProperty("path.separator")
     getPackJarsFromCache(packList, cache, repoUrl)
      .foldLeft(".")((acc, jar) => acc + sep + jar)
  }

  /** Copy packages from cache to a destination directory. The packages
      will be downloaded from a remote repository if not available in
      the cache yet.
    */ 
  def copyPackageFromCache(packList: List[PackData], cache: String, repoUrl: String, dest: String) = {
    Utils.mkdir(dest)
    getPackJarsFromCache(packList, cache, repoUrl)
      .foreach{ file =>
      println(s"Copying ${new java.io.File(file).getName} to ${dest}")
      Utils.copyFileTo(file, dest)
    }
  }


} // ------ End of object Packget ------ // 




object PackSearch{

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

