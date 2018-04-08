package jarget.utils 

// import jarget.utils._

/** Provide functions to build Uber jars (aka fat-jars)*/
object JarBuilder{
  
  import java.util.jar.{JarOutputStream, JarEntry, JarFile}

  /** Jar wrapper */
  abstract sealed trait JWrapper
  case object JWrapperEmpty extends JWrapper
  /** Unix Executable wrapper */
  case object JWrapperUEXE extends JWrapper
  /** Windows CLI (Command Line Interface) wrapper */
  case object JWrapperWCLI extends JWrapper
  /** Windows GUI (Graphical User Interface) wrapper */
  case object JWrapperWGUI extends JWrapper

  def parseWrapper(name: String): JWrapper = name match {
    case "empty" => JWrapperEmpty
    case "uexe"  => JWrapperUEXE
    case "wcli"  => JWrapperWCLI
    case "wgui"  => JWrapperWGUI
    case  _
        => throw new java.lang.IllegalArgumentException("Error: Invalid jar wrapper option.")
  }

  def getWrapperStream(wrapper: JWrapper, cls: Class[_]) =
      wrapper match {
        case JWrapperEmpty
            => None
        case JWrapperUEXE
            => Option(cls.getResourceAsStream("/assets/unixLoader.sh"))
        case JWrapperWCLI
            => Option{Utils.failIfNull(
              cls.getResourceAsStream("/assets/loaderCLI.exe"),
                "Error: file loaderCLI.exe not found in resource files."
            )}
        case JWrapperWGUI
            => Option{Utils.failIfNull(
                cls.getResourceAsStream("/assets/loaderGUI.exe"),
                "Error: file loaderGUI.exe not found in resource files."
              )}
      }


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

  /** Create a jar file with or without an executable wrapper.
    *  @param file    - Output uber jar file or executable wrapper with jar payload.
    *  @param wrapper - Type of executable wrapper (default empty).
    *  @param cls     - Class where the resource files will extracted.
    *  @param fn      - Function that writes to jar output stream.
    */
  def makeJarWith(
    file:    String,
    wrapper: JWrapper = JWrapperEmpty,
    cls:     Class[_]
  )(jarWriter: JarOutputStream => Unit) = {
    var os:  JarOutputStream  = null
    var fo:  java.io.FileOutputStream = null
    try {
      fo = new java.io.FileOutputStream(file)
      os = new JarOutputStream(fo)
      for(st <- getWrapperStream(wrapper, cls))
        Utils.copyStream(st, fo)
      jarWriter(os)
    } finally if (os != null) {
      // Ensure that file handlers are closed
      os.flush()
      os.close()
      // Set the output file as executable if in Unix
      if(wrapper == JWrapperUEXE) Runtime.getRuntime().exec("chmod u+x " + file)
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
    cls:         Class[_],
    output:      String,
    main:        String,
    paths:       List[String] = List(),
    jarFiles:    List[String] = List(),
    files:       List[String] = List(),
    filesEntry:  List[String] = List(),
    resources:   List[String] = List(),
    scalaLib:    Boolean      = false,
    wrapper:     JWrapper     = JWrapperEmpty
  ) = makeJarWith(output, wrapper, cls){ jos =>

    // Try to append scala-library.jar scala runtime to the output jar file.
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

  /** Turn an Uber-Jar into a Unix executable, windows cli executable or GUI executable.
    * A Unix executable is a small shell script with a jar payload.
    */
  def makeExecutable(cls: Class[_], jarFile: String, outFile: String, wrapper: JWrapper = JWrapperUEXE ) = {
    var fi: java.io.FileInputStream  = null 
    var fo: java.io.FileOutputStream = null
    try {
      fi = new java.io.FileInputStream(jarFile)
      fo = new java.io.FileOutputStream(outFile)
      for(st <- getWrapperStream(wrapper, cls))
        Utils.copyStream(st, fo)
      Utils.copyStream(fi, fo)
      if(wrapper == JWrapperUEXE)
        Runtime.getRuntime().exec("chmod u+x " + outFile)
    } finally {
      if (fi != null) fi.close()
      if (fo != null) fo.close()
    }
  }

} // -------- End of object JarUtils -------- //

