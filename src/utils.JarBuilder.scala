package jarget.utils 

// import jarget.utils._

/** Provide functions to build Uber jars (aka fat-jars)*/
object JarBuilder{
  
  import java.util.jar.{JarOutputStream, JarEntry, JarFile}


  def addFile(zos: JarOutputStream, entry: String, file: String){
    import java.io._
    val is = new BufferedInputStream(new FileInputStream(file))
    val e = new java.util.jar.JarEntry(entry)
    zos.putNextEntry(e)
    Utils.copyStream(is, zos)
    is.close()
  }


  def addDirectory(zos: JarOutputStream, path: String){
    import java.nio.file.{Paths, Path, Files}
    val root = Paths.get(path)
    val name = new java.io.File(path).getName()
    Files.walk(Paths.get(path)).filter(_.toFile.isFile).forEach{p =>
      val entry = new java.io.File(name, root.relativize(p).toString).toString
      addFile(zos, entry, p.toString)
    }
  }



  def addJarContent(
    zos:         JarOutputStream,
    jarFile:     String,
    addManifest: Boolean = false
  ){
    import scala.collection.JavaConverters._
    val jar = new java.util.jar.JarFile(jarFile)
    jar.entries
      .asScala
      .filter(!_.isDirectory)
      .foreach { e =>
      if (addManifest || e.getName() != "META-INF/MANIFEST.MF" ){
        val is = jar.getInputStream(e)
        //val je = new java.util.jar.JarEntry(e.getName())
        //je.setMethod(java.util.zip.ZipOutputStream.DEFLATED)
        e.setCompressedSize(-1)
        zos.putNextEntry(e)
        Utils.copyStream(is, zos)
        is.close()
      }
    }
    jar.close()
  }


  def addJarsFromDir(
    zos:   JarOutputStream,
    path:  String
  ) = {
    new java.io.File(path)
      .listFiles()
      .filter(_.getName.endsWith(".jar"))
      .foreach{ p =>
      //println("Adding file " + p)
      addJarContent(zos, p.getPath)
      //println("Added file " + p)
    }
  }


  def makeJarWith(file: String, executable: Boolean = false)(fn: JarOutputStream => Unit) = {
    var os:  JarOutputStream  = null
    var fo:  java.io.FileOutputStream = null
    try {
      fo = new java.io.FileOutputStream(file)
      os = new JarOutputStream(fo)
      if (executable) fo.write(jarHeader.getBytes())
      fn(os)
    } catch {
      case ex: java.io.IOException => ex.printStackTrace()
    } finally if (os != null) {
        //fo.close()
      os.flush()
      os.close()

      // Set the output file as executable if in Unix
      if(executable) Runtime.getRuntime().exec("chmod u+x " + file)
    }
  }


  /**
      Build an uber jar (aka fat jar) bundling an application with
      all its dependencies:

      @param output     - output file
      @param main       - Main jar file containing main class
      @param paths      - List of paths to directories containing jar files
      @param jarFiles   - List of jar files to be added to the output jar
      @param files      - List of asset files to be added to the output uber jar 
      @param filesEntry - List of asset files such as file1.txt:texts/entry.txt /tmp/file2.txt:entry2.txt 
      @param resources  - Append a list of resource directories to the jar files.
      @param scalaLib   - If true bundles the scala run-time scala-library.jar with the app.
      @param executable - If true makes unix self-executable jar file that can be run as script ./app.jar
    */
  def makeUberJar(
    output:      String,
    main:        String,
    paths:       List[String] = List(),
    jarFiles:    List[String] = List(),
    files:       List[String] = List(),
    filesEntry:  List[String] = List(),
    resources:   List[String] = List(),
    scalaLib:    Boolean      = false,
    executable:  Boolean      = false
  ) = makeJarWith(output, executable){ jos =>

    if (scalaLib) Utils.getScalaHome() match {
      case None    => throw new Exception("Error: SCALA_HOME path not found")
      case Some(p) => {
        val lib = new java.io.File(p, "lib/scala-library.jar").getPath
        addJarContent(jos, lib)
      }
    }

    addJarContent(jos, main, true)
    paths     foreach { p => addJarsFromDir(jos, p) }
    jarFiles  foreach { p => addJarContent(jos, p) }
    files     foreach { p => addFile(jos, p, p) }
    resources foreach { p => addDirectory(jos, p) }

    filesEntry foreach { p =>
      p.split(":") match {
        case Array(file, entry)
            => addFile(jos, entry, file)
        case _
            => throw new IllegalArgumentException(s"Error: Invalid argument '${p}', it must separated by semicolon '(:')")
    }}
  }


  def makeExecutableJar(jarFile: String, outFile: String) = {

    var fi: java.io.FileInputStream  = null 
    var fo: java.io.FileOutputStream = null

    try {
      fi = new java.io.FileInputStream(jarFile)
      fo = new java.io.FileOutputStream(outFile)
      fo.write(jarHeader.getBytes())
      Utils.copyStream(fi, fo)
      Runtime.getRuntime().exec("chmod u+x " + outFile)
    } catch {
      case ex: java.io.IOException => ex.printStackTrace()
    } finally {
      if (fi != null) fi.close()
      if (fo != null) fo.close()
    }
  }

} // -------- End of object JarUtils -------- //

