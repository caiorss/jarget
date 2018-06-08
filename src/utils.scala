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

 =============================================================== */

package jarget.utils

/** General utility functions */
object Utils{

  import scala.concurrent.Future
  import concurrent.ExecutionContext.Implicits.global

  /** Create directory and its parents */
  def mkdir(path: String) = {
    new java.io.File(path).mkdirs()
  }

  /** Join paths */
  def join(path1: String, path2: String) = {
    new java.io.File(path1, path2).getPath()
  }

  def joinPathList(paths: String*) = 
    paths.fold(null)((acc, p) => new java.io.File(acc, p).toString)  

  /** Check if file exists */ 
  def fileExists(file: String) =
    new java.io.File(file).isFile()

  def dirExists(dir: String) =
    new java.io.File(dir).isDirectory()

  /** Download file from a URL with user feedback. */
  def downloadFile(
    fileUrl:    String,
    file:       String,
    bufferSize: Int = 1024,
    verbose:    Boolean = false
  ){
    def copyStream(
      from:     java.io.InputStream,
      to:       java.io.OutputStream,
      fileSize: Long = -1L
    ) = {
      val buffer = new Array[Byte](bufferSize)
      // number of bytes read
      var nbytes = 0
      var total: Long = 0L
      val sizeInMB = fileSize.toDouble / (1024 * 1024)
      while({ nbytes = from.read(buffer) ; nbytes} > 0){
        total = total + nbytes
        if(verbose)
          print(f"Downloading file = $file - ${100.0 * total / fileSize }%.0f%% of $sizeInMB%.2f MB\r")
        to.write(buffer, 0, nbytes)
      }
    }
    val u   = new java.net.URL(fileUrl)
    val os = new java.io.FileOutputStream(file)
    var is: java.io.InputStream  = null
    try {
      val conn = u.openConnection()
      conn.connect()
      is = conn.getInputStream()
      val fileSize = conn.getHeaderFieldLong("Content-Length", -1L)
      copyStream(is, os, fileSize)
    } finally {
      // Ensure that file handlers are closed
      // in order to avoid resource leaking.
      if(is != null) is.close()
      os.close()
    }
  }

  /** 
      Copy file to directory. 

      @param file    - File that will be copied. 
      @param destDir - Path to destination directory 

      Example: The command below will copy file lsb-release to file
              /tmp/lsb-release.

     {{{
          copyFileTo("/etc/lsb-release", "/tmp")
      }}}
    */ 
  def copyFileTo(file: String, destDir: String) = {
    val src = new java.io.File(file)
    val dst = new java.io.File(destDir, src.getName)
    var srcChannel: java.nio.channels.FileChannel = null
    var dstChannel: java.nio.channels.FileChannel = null
    try {
      srcChannel = new java.io.FileInputStream(src).getChannel()
      dstChannel = new java.io.FileOutputStream(dst).getChannel()
      dstChannel.transferFrom(srcChannel, 0, srcChannel.size())
    } finally {
      srcChannel.close()
      dstChannel.close()
    }
  }

  /** Delete all files and subdirectories of a directory recursively */ 
  def deleteDirectory(path: String, verbose: Boolean = false) =
    if (new java.io.File(path).isDirectory){
      import java.nio.file.{Files, Paths, Path}
      val root = Paths.get(path)
      Files.walk(root)
        .filter(_.toFile.isFile)
        .forEach{ p =>
        if (verbose) println("Removing file: " + p)
        p.toFile().delete()
      }
      Files.walk(root)
        .filter(_.toFile().isDirectory)
        .toArray
        .map(_.asInstanceOf[java.nio.file.Path].toFile)
        .reverse
        .foreach{p =>
        if (verbose) println("Deleting directory: " + p)
        p.delete()
      }
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

  /** Open URL with default web browser */
  def openUrl(uri: String){
    import java.awt.Desktop
    import java.io.IOException
    import java.net.URI
    import java.net.URISyntaxException
    val u = new URI(uri)
    val desktop = Desktop.getDesktop()
    desktop.browse(u)
  }

  def copyStream(from: java.io.InputStream, to: java.io.OutputStream) =
    try {
      // number of bytes read
      var n = 0
      // Buffer with 1024 bytes or 1MB
      val buf = new Array[Byte](1024)
      while( {n = from.read(buf) ; n} > 0 ){
        to.write(buf, 0, n)
      }
    } finally {
      from.close()
    }

  /** Throw an exception with error message if obj parameter is null, otherwise returns it.
    * It only should be used on situations where obj is never supposed to be null.
    * For instance, when getting an input stream of a resource file.
   */
  def failIfNull[A](obj: A, errorMsg: String) = {
    if(obj == null)
      throw new Exception(errorMsg)
    else
      obj
  }

  def getScalaVersion() = util.Properties.versionNumberString

  /**  Pretty print a collection of tuples as a table.
    Parameters:
    @param rows     - Collection of tuples to be printed.
    @param title    - Tuple containing the titles of left and right side.
    @param line     - Flag that if set to true, prints a new line between each row.
    @param margin   - Margin from left side of screen as number of spaces.
    @param sep      - Number of spaces between left side and right side
    @param maxRside - Maximum number of characters to printed on right side.
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


  /** Show system's enviroment variables in tabular form 
      Example: 

    {{{ 
        scala> showEnvironmentVars()
           Environment Variable         Value
           ------------------------     --------------------------------------------------
           PATH                         /usr/local/sbin:/usr/local/bin:/usr/bin:/usr/li...
           XAUTHORITY                   /home/archbox/.Xauthority
           LC_MEASUREMENT               en_US.UTF-8
           LC_TELEPHONE                 en_US.UTF-8
           GDMSESSION                   xfce
           XDG_DATA_DIRS                /usr/local/share:/usr/share
           LC_TIME                      en_US.UTF-8
          .... .... .... .... .... 

    }}}

   */
  def showEnvironmentVars() {
    import scala.collection.JavaConverters._
    printTupleAsTable(
      rows     = System.getenv.asScala.toSeq,
      title    = ("Environment Variable", "Value"),
      margin   = 2,
      maxRside = 50
    )
  }

  /** Show java properties in tabular format */
  def showJavaProperties() {
    import scala.collection.JavaConverters._
    printTupleAsTable(
      rows     = System.getProperties.asScala.toSeq,
      title    = ("Java Property", "Value"),
      margin   = 2,
      maxRside = 50
    )
  }


  /** Get path to program available in PATH variable 
      Example: 
      {{{
        scala> getProgramPath("scala")
        res1: Option[java.io.File] = Some(/home/archbox/opt/scala/bin/scala)

        scala> getProgramPath("java")
        res2: Option[java.io.File] = Some(/usr/bin/java)

        scala> getProgramPath("sh")
        res3: Option[java.io.File] = Some(/usr/bin/sh)

        scala> getProgramPath("shx")
        res4: Option[java.io.File] = None
      }}}
    */
  def getProgramPath(program: String) : Option[java.io.File] = {
    val sep   = java.io.File.pathSeparator
    val paths = System.getenv("PATH").split(sep)
    for {
      appPath <-  paths find { p => new java.io.File(p, program).exists }
    } yield new java.io.File(appPath, program)
  }

  /** Get path to scala home, directory where scala is installed */
  def getScalaHome(): Option[String] = {
    val p1 = Option(System.getenv("SCALA_HOME"))
    val p2 = getProgramPath("scala").map(_.getParentFile.getParent)
    p1 orElse p2 
  }

  /** Emulate Unix system call execl */
  def execl(
    program: String,                           // Program that will be run
    args:    List[String] = List(),            // Program arguments
    env:     List[(String, String)] = List(),  // Environment variables
    verbose: Boolean = false 
  ) = {
    if(verbose)
      println(s"""Running: $$ $program ${args.mkString(" ")}""")
    val builder = new java.lang.ProcessBuilder()
    val evars    = builder.environment()
    builder.command(program)
    // set process' commands
    args foreach builder.command.add
    // set process' enviroment variables
    env foreach { case (k, v) => evars.put(k, v) }
    val iostat = builder.inheritIO().start()
    // Return sub-process status code
    iostat.waitFor()
  }

  /** Print file contents */
  def showFile(file: String): Unit = {
    var in: java.io.FileInputStream = null
    try {
      in = new java.io.FileInputStream(file)
      while (in.available > 0) System.out.write(in.read)
    } catch {
      case ex: java.io.IOException => ex.printStackTrace()
    } finally {
      if (in != null) in.close()
    }
  }

  /** Read resource file from  a jar file or file in classpath 
      Example: 

      {{{
        scala> readResourceFile(getClass(), "/version.txt")
        res5: Option[String] =
        Some(1.3)

        scala> readResourceFile(getClass(), "/version.txt") foreach println
        1.3
      }}}
    */
  def readResourceFile(cls: Class[_], file: String): Option[String] = {
    def readBufferedReader(bf: java.io.BufferedReader) = {
      val builder = new StringBuilder()
      var line = ""
      while({line = bf.readLine() ; line} != null){        
        builder.append(line + "\n")
      }
      bf.close()
      builder.toString()
    }
    for {
      //s = getClass().getResourceAsStream(file)
      st    <- Option(cls.getResourceAsStream(file))
      is    = new java.io.InputStreamReader(st)
      bf    = new java.io.BufferedReader(is)
      text  = readBufferedReader(bf)
    } yield text.trim()
  }

  def readResourceProperties(file: String) =
    jarget.reader.Reader { cls: Class[_] =>
      val pdata = for {
        res <- Option(cls.getResourceAsStream(file))
        prop = {
          val p = new java.util.Properties()
          p.load(res)
          p
        }
      } yield prop

      assert(!pdata.isEmpty, "Fatal Error: Resource file is not supposed to be empty.")
      pdata.get 
    }
  

} /* ---- End of object Utils ------- */


/** Provides functions to inspect and check jar files */
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


  /** Get all packages of a jar file */
  def getPackages(jarFile: String): Set[String] = {
    import scala.collection.JavaConverters._
    def getParentPath(p: String) = {
      new java.io.File(p).getParent()
    }
    val jar = new java.util.jar.JarFile(jarFile)
    val entries = (
      jar.entries.asScala
        map (_.getName())
        filter (_.endsWith(".class"))
        map getParentPath
        map (_.replace("/", "."))
        toSet
    )
    jar.close()
    entries
  }


  /** Get all classes within a package. */
  def getPackageClasses(jarFile: String, packName: String): List[String] = {
    import scala.collection.JavaConverters._
    def getParentPath(p: String) = {
      new java.io.File(p).getParent()
    }
    val jar  = new java.util.jar.JarFile(jarFile)
    val path = packName.replace(".", "/")
    val entries = (
      jar.entries.asScala
        map    (_.getName())
        filter (p => p.endsWith(".class") && p.startsWith(path) && !p.contains('$'))
        map    (_.stripSuffix(".class").replace("/", "."))

        toList
    )
    jar.close()
    entries
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


  def runWithClassPath2(program: String, args: List[String] = List(), cpath: String = null) = {
    if (cpath == null)
      Utils.execl(program, args)
    else
      Utils.execl(program, List("-cp", cpath) ++ args)
  }


} // -------- End of object JarUtils ------------ //


