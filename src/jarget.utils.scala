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


  def downloadFile(fileUrl: String, file: String) = {
    val url = new java.net.URL(fileUrl)
    val rbc = java.nio.channels.Channels.newChannel(url.openStream())
    val fos = new java.io.FileOutputStream(file)
    fos.getChannel().transferFrom(rbc, 0, java.lang.Long.MAX_VALUE)
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

  def copyStream(from: java.io.InputStream, to: java.io.OutputStream){
    // number of bytes read
    var n = 0
    // Buffer with 1024 bytes or 1MB
    val buf = new Array[Byte](1024)
    while( {n = from.read(buf) ; n} > 0 ){
      to.write(buf, 0, n)
    }
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

/** Provide functions to build Uber jars (aka fat-jars)*/
object JarBuilder{

  import Utils.copyStream
  import java.util.jar.{JarOutputStream, JarEntry, JarFile}

  def addFile(zos: JarOutputStream, entry: String, file: String){
    import java.io._
    val is = new BufferedInputStream(new FileInputStream(file))
    val e = new java.util.jar.JarEntry(entry)
    zos.putNextEntry(e)
    copyStream(is, zos)
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
        copyStream(is, zos)
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
    val header = """
#!/usr/bin/env sh
if [[ -z "$JAVA_HOME" ]]
then
    java -jar "$0" "$@"
else
    "$JAVA_HOME/bin/java" -jar "$0" "$@"
fi
exit 0
""".trim() + "\n"

    try {
      fo = new java.io.FileOutputStream(file)
      os = new JarOutputStream(fo)
      if (executable) fo.write(header.getBytes())
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

    val header = """
#!/usr/bin/env sh
if [[ -z "$JAVA_HOME" ]]
then
    java -jar "$0" "$@"
else
    "$JAVA_HOME/bin/java" -jar "$0" "$@"
fi
exit 0
""".trim() + "\n"

    try {
      fi = new java.io.FileInputStream(jarFile)
      fo = new java.io.FileOutputStream(outFile)
      fo.write(header.getBytes())
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


object OptParseUtils {

  class ArgResult(cmd: String, xs: List[String]){

    def getOne() = xs  match {
      case List(a) => a
      case List()  => throw new IllegalArgumentException("Error: Expected argument of " + cmd)
      case _       => throw new IllegalArgumentException("Error: Unexpected argument of " + cmd)
    }

    def getMany() = xs

    def getOneOrMany() = xs match {
      case List() => throw new IllegalArgumentException("Error: Expected at least one argument of " + cmd)
      case _      => xs
    }

    def getNumArgs(n: Int) = xs.length match {
      case k if n == k
          => xs
      case _
          => throw new IllegalArgumentException(s"Error: ${cmd} expects ${n} arguments.")
    }

    def getFlag() = xs match {
      case List() => true
      case _      => throw new IllegalArgumentException("Error: Unexpected argument of " + cmd)
    }

  }

  case class ArgSpec(
    name:        String,
    mandatory:   Boolean,
    description: String,
    action:      ArgResult => Unit
  )

  /** Transform command line arguments into groups */
  def parseCmdArgs(arglist: List[String]): Map[String, List[String]] = {

    def isSwitch(term: String) = term.startsWith("-")

    def findIndicesWhere[A](predicate: A => Boolean, xs: List[A]) = {
      var idx  = -1
      var idxs = List[Int]()
      for (x <- xs) {
        idx = idx + 1
        if (predicate(x))
          idxs = idx::idxs
      }
      idxs.reverse
    }

    val i = {
      val n = arglist.indexWhere(x => x == "--", 0)
      if (n == -1) arglist.length else n
    }
    val cmds = arglist.slice(0, i)
    val extCmd = arglist.slice(i, arglist.length) match {
      case List() => List()
      case hd::tl => List((hd, tl))
    }

    val indices = findIndicesWhere(isSwitch, cmds)
    val out = indices match {
      case List() => extCmd
      case _      => {

        val xs      = (indices ++ List(cmds.length))
          .sliding(2)
          .map{ case List(a, b) => (a, b)}
          .toList
        val groups = xs.foldLeft(List(): List[(String, List[String])]){ (acc, x) =>
          val (a, b): (Int, Int) = x
          val cmd        = cmds(a)
          val args       = cmds.slice(a+1, b)
          (cmd, args)::acc
        }
        (groups.reverse ++ extCmd)
      }
    }
    out.toMap
  }


  def parseCmdArgsEq(arglist: List[String]) :  Map[String, List[String]] = {
    val xs = arglist map { (opt: String) =>
      opt.split("=") match {
        case Array(arg)        => (arg, List[String]())
        case Array("",  _)     => throw new Exception("Error bad formated argument")
        case Array(arg, value) => (arg, value.split(":").toList)
        case _                 => throw new Exception("Error bad formated argument")
      }
    }
    xs.toMap
  }

} // --- End of Module OptParseUtils ---- //


class OptParse{
  import OptParseUtils._
  import scala.collection.mutable.ListBuffer
  private var opts = ListBuffer[ArgSpec]()

  def addOption(
    name:         String,
    action:       ArgResult => Unit
  ){
    opts append ArgSpec(name, false, "", action)
  }

  def addOption(
    name:         String,
    description:  String,    
    action:       ArgResult => Unit
  ){
    opts append ArgSpec(name, false, description, action)
  }

  def addOption(
    name:         String,
    description:  String,    
    mandatory:    Boolean,
    action:       ArgResult => Unit
  ){
    opts append ArgSpec(name, mandatory, description, action)
  }

  def getOptions() = opts.toList

  def showHelp() = {
    println("Options\n")
    opts foreach { o =>
      println(s" ${o.name}\t${o.description}")
    }
  }

  def parseArgs(arglist: List[String]) = {
    val data   = parseCmdArgs(arglist)
    val params = opts.map(_.name).toSet
    if (arglist.isEmpty)
      this.showHelp()
    else {

      data.keys foreach { k =>
         if (!params.contains(k))
           throw new IllegalArgumentException("Error: Invalid argument " + k)
       }

       opts foreach { opt =>
         data.get(opt.name) match {
           case Some(args)
               => opt.action(new ArgResult(opt.name, args))
           case None
               => {
                 if (opt.mandatory)
                   throw new IllegalArgumentException("Error: missing mandatory argument " + opt.name)
               }
         }
       }
    }
  } // --- End of method parseargs --- //

} // End of class OptParse
