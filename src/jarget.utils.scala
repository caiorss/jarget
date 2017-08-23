package jarget.utils

object Utils{

  import scala.concurrent.Future
  import concurrent.ExecutionContext.Implicits.global

  def mkdir(path: String) = {
    new java.io.File(path).mkdirs()
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


  /** Get path to program available in PATH variable */
  def getProgramPath(program: String) : Option[java.io.File] = {
    val sep   = java.io.File.pathSeparator
    val paths = System.getenv("PATH").split(sep)
    for {
      appPath <-  paths find { p => new java.io.File(p, program).exists }
    } yield new java.io.File(appPath, program)
  }

  /** Emulate Unix system call execl */
  def execl(
    program: String,                           // Program that will be run
    args:    List[String] = List(),            // Program arguments
    env:     List[(String, String)] = List()   // Environment variables
  ) = {

    val builder = new java.lang.ProcessBuilder()
    val evars    = builder.environment()
    builder.command(program)

    // set process' commands
    args foreach builder.command.add

    // set process' enviroment variables
    env foreach { case (k, v) => evars.put(k, v) }

    val iostat = builder.inheritIO().start()
    System.exit(iostat.waitFor())
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

  /** Get jar's META-INF/MANIFEST.MF content */
  def getManifest(jarFile: String): String = {
    val jar = new java.util.jar.JarFile(jarFile)
    val bs = new java.io.ByteArrayOutputStream()
    jar.getManifest().write(bs)
    val out = new String(bs.toByteArray(), "UTF-8")
    jar.close()
    bs.close()
    out
  }

  /** Show jar's META-INF/MANIFEST.MF file*/
  def showManifest(file: String): Unit = {
    val jar = new java.util.jar.JarFile(file)
    val man = jar.getManifest()
    man.write(System.out)
    jar.close()
  }

  def getFilesAsList(jarFile: String) = {
    import scala.collection.JavaConverters._
    val jar = new java.util.jar.JarFile(jarFile)
    val files = jar.entries().asScala.toList
    jar.close()
    files
  }

  def showFiles(jarFile: String) = {
    import scala.collection.JavaConverters._
    val jar = new java.util.jar.JarFile(jarFile)
    jar.entries().asScala filter(!_.isDirectory) foreach println
    jar.close()
  }


  /** Get only asset files, disregarding all *.class files and directory entries. */
  def getAssetFiles(jarFile: String) = {
    val files = getFilesAsList(jarFile)
    files filter (f => !f.isDirectory() && !f.getName().endsWith(".class"))
  }

  /** Returns 'Main-Class' parameter from jar file */
  def getMainClass(jarFile: String): Option[String] = {
    val jar = new java.util.jar.JarFile(jarFile)
    val cls = Option(
      jar.getManifest()
        .getMainAttributes()
        .getValue("Main-Class")
    )
    jar.close()
    cls
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


  def extract(jarFile: String, dest: String, verbose: Boolean = false) = {
    import scala.collection.JavaConverters._

    val jar = new java.util.jar.JarFile(jarFile)

    def extractFile(file: String) = {
      val is = jar.getInputStream(jar.getEntry(file))
      val fout = new java.io.File(dest, file)
      val os = new java.io.FileOutputStream(fout)
      while(is.available() > 0){
        os.write(is.read())
      }
      is.close()
      os.close()
    }

    def mkdir(path: String) = {
      new java.io.File(path).mkdirs()
    }

    def joinPath(path1: String, path2: String) = {
      new java.io.File(path1, path2).getPath
    }

    // Create directories
    jar.entries().asScala
      .toStream
      .filter (_.isDirectory)
      .map(_.getName())
      .foreach{ p => mkdir(joinPath(dest, p)) }

    // Copy files
    jar.entries().asScala
      .toStream
      .filter(!_.isDirectory)
      .map(_.getName())
      .foreach{ p =>
      if (verbose) println("Extracting file: " + p)
      extractFile(p)
    }

    jar.close()

  } // --- End of function extract ------- //


  /** Get classpath with all jar files from a directory.

      Example: If the directory app/lib
      has the files lib1.jar, lib2.jar and so on.

      The function getClasspath("app/lib")
      will return a string containing
      'app/lib/lib1.jar:app/lib/lib2.jar:app/lib/lib3.jar' ...

      The path separator is (:) for Unix-like OS's and (;) for
      Windows.
    */
  def getClasspath(path: String): String = {
    val files = new java.io.File(path)
      .listFiles()
      .toStream
      .filter(_.getName().endsWith(".jar"))
    
    val sep = System.getProperty("path.separator")

    files.foldLeft("."){ (acc: String, file: java.io.File) =>
       file.getPath() + sep + acc
    }
  }

  /** Run a program with a given set of arguments by
      setting the CLASSPATH enviroment variable with
      all jars from the 'path' directory.

      Example: if the directory path is 'app/lib' and
      contains the files lib1.jar, lib2.jar and lib3.jar
      it will run the 'program' with CLASSPATH enviroment
      variable set to 'app/lib/lib1.jar:app/lib/lib2.jar...'

    */
  def runWithClassPath(program: String, args: List[String] = List(), path: String = null) = {
    val cpath = getClasspath(path)
    if (path == null)
      Utils.execl(program, args)
    else
      Utils.execl(program, List("-cp", cpath) ++ args)
  }


} // -------- End of object JarUtils ------------ //
