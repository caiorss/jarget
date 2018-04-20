package jarget.optParser

private object OptFun{
  /** Print Table of rows (list of strings) as table */
  def printTableOfRows(xs: Seq[List[String]], space: Int = 1, left: Int = 2) = {
    val sizes = xs.transpose.map{col => col.map(_.length).max}
    val n = sizes.length
    val leftSpace = " " * left
    for(row <- xs){
      print(leftSpace)
      for(i <- 0 to n - 1){
        val nspaces = sizes(i) - row(i).length + 2
        print(row(i) + " " * nspaces)
      }
      println()
    }
  }
}

/** Command line switch */
class OptSwitch(
  name:      String = "",
  shortName: String = "",
  argName:   String = "",  
  desc:      String = ""
){
  private var _name:      String = name 
  private var _shortName: String = shortName  
  private var _desc:      String = desc

  override def toString() =
    s"name = $name - shortName = $shortName"

  def getName() =
    _name

  def getShortName() =
    _shortName

  def getArgName() =
    argName 

  def setName(name: String) = {
    _name = name
    this 
  }

  def getDesc() =
    _desc

  def setDesc(desc: String) = {
    _desc = desc
    this 
  }
}

/** Result of command line parsing */ 
class OptResult(
  operands:   List[String],
  switches:   Map[String, List[String]],
  properties: Map[String, String]
){

  /** Get operands that are arguments without command line switches and 
      are also not Java properties (which starts with -D<prop>=<value>) 
    */
  def getOperands() = operands

  /** Get command line switches -<key>=<value> or -<flag> */
  def getSwitches() = switches

  /** Get java properties -D<name>=value */
  def getProperties() = properties

  def getOperand(index: Int, default: String, errorMsg: String = "") = {
    if(index >= operands.size)
      default
    else
      operands(index)
  }

  def getOperandOrExit(index: Int, errorMsg: String = "") = {
    if(index >= operands.size || index < 0){
      println(if (errorMsg == "") s"Error: expected operand $index" else errorMsg)
      System.exit(1)
    }
    operands(index)
  }

  def getListStr(name: String) = 
    switches.get(name) getOrElse List()

  def getStr(name: String, default: String): String = {
    // val namet = options.find(n => n.getSize() )
    val res = switches.get(name)
    res.map{ r => r(0) }.getOrElse(default)
  }

  def getInt(name: String, default: Int): Int = {
    val res = switches.get(name)
    res.map{ r => r(0).toInt } getOrElse default
  }

  def getFlag(name: String, default: Boolean = false) = {
    val res = switches.get(name)
    if(!res.isEmpty) true else default
  }

  override def toString() = {
    val sw = new java.io.StringWriter()
    val pw = new java.io.PrintWriter(sw)
    pw.println("Operands =  " + this.getOperands())
    pw.println("Arguments = " + this.getSwitches())
    sw.toString
  }

} /** End of class OptResult */


trait IOptCommand{
  def getCommandName():   String
  def getCommandDesc():   String
  def getCommandUsage():  String

  /** Show command help */
  def showHelp():         Unit

  /** Parse command line and execute action */
  def parseRun(argList: List[String]): Unit

} /* End of trait IOptCommand */


class Separator(name: String) extends IOptCommand{
  def getCommandName()  = s"\n[$name]\n"
  def getCommandDesc()  = ""
  def getCommandUsage() = ""
  def showHelp() = ()
  def parseRun(argList: List[String]) = ()
}

/** Sub command action that executes without any switch. */
class OptCommandAction(name: String, desc: String)(action: => Unit){
  def getCommandName()  = name
  def getCommandDesc()  = desc 
  def getCommandUsage() = ""
  def showHelp() = println(desc)
  def parseRun(argList: List[String]) =
    action 
}


/** Main command or subcommand like log from $ git log. 
    @param name     Subcommand name such as log from - $ git log
    @param usage    Short usage string similar to usage: [OPTIONS] [ARGS]
    @param desc     Brief one-line description about the subcommand / command. 
    @param longDesc Long description about the subcommand / command.
    @param example  Examples about the command.
    @param helpFlag If true, shows the command help when invoked without arguments. 
  */
class OptCommand (
  name:     String = "",  
  usage:    String = "",  
  desc:     String = "",  
  longDesc: String = "",
  example:  String = "",
  helpFlag: Boolean = false
  ) extends IOptCommand {

  import scala.collection.mutable.ListBuffer
  val switchMark = "-"
  val switchSeparator = "="
  private val options  = ListBuffer[OptSwitch]()
  private var operands = List[String]()
  private var _action = (res: OptResult) => println("results  = " + res)

  def getCommandName()  = name
  def getCommandDesc()  = desc
  def getCommandUsage() = usage

  def addOpt(
    name:      String,
    shortName: String  = "",
    argName:   String  = "",
    desc:      String  = "",
    ) = {
    this.options.append(new OptSwitch(name, shortName, argName, desc))
    this
  }

  /** Set command / function to be called after results are parsed. */
  def setAction(action: OptResult => Unit) = {
    _action = action
    this
  }    

  def getOptions() =
    options.toList

  /** Print help information for the user. */
  def showHelp() = {
    val name  = this.getCommandName()
    val desc  = this.getCommandDesc()
    val usage = this.getCommandUsage()
    if(desc != "") {
      println(desc)
      println()
    }
    if(longDesc != "") println(longDesc)    
    if(name != "") println(s" Usage: $name $usage")

    if(!options.isEmpty){
      println() ; println("OPTIONS:")
    }
    
    /** Print options (Command line switches) */
    val rows = options.toList map {o =>
      val argName = o.getArgName()      
      List(
        "-" + o.getName + (if (argName != "") ("=" + argName) else "") + ", ",
        "-" + o.getShortName(),
        o.getDesc()
      )
    }
    OptFun.printTableOfRows(rows)
    if(example != "") {
      println()
      println(example)
    }

  } // --- EoF func showHelp() ---- //

  /** Parse command line arguments */
  def parse(argList: List[String]): OptResult = {
    /* Java properties, aka switches like -Dserver.storage=/path */
    val properties =
      argList
        .filter(_.startsWith("-D"))
        .map{s => s.stripPrefix("-D").split("=", 2) match {
          case Array(key)         => (key, "")
          case Array(key, "")     => (key, "")            
          case Array(key, value)  => (key, value)
          case p => throw new RuntimeException("Error: Invalid property: " + p)
        }
      }.toMap


    /* Get command line switches of type -<switch>=<value> or -<switch>
       for instance,  -o=output.exe, -i=input.jar -flag
     */
    val switchesLST = argList
      .takeWhile(_ != "--")
      .filter(s => s.startsWith(switchMark) && !s.startsWith("-D"))
      .map { p => p.split(switchSeparator, 2) match {
        case Array(k, v) => (k.stripPrefix(switchMark), v)
        case Array(k)    => (k.stripPrefix(switchMark), "")
        case _           => null
      }
    }
    // println("switchesLST = " + switchesLST )
    var switches: Map[String, List[String]] = switchesLST
      .groupBy{case (k, v) => k}
      .map{case (k, xs) =>
        val name = options
          .find(n => n.getName() == k || n.getShortName() == k)
          .map(_.getName)
          .getOrElse(k)
        (name, xs map (_._2))
    }

    // Arguments after --
    val restArgs = argList.dropWhile(_ != "--")

    restArgs match {
      case List()      => () // ignore
      case List("--")  => () // Ignore
      case "--"::rest  => switches += "--" -> rest
      case _           => ()
    }

    val operands: List[String] =
      argList
        .takeWhile(_ != "--")
        .filter(!_.startsWith(switchMark))

    // Validate result
    val switchKeys = switches.map(_._1).toSet
    //println("switchKeys = " + switchKeys)
    val keys = (options.map(_.getName) ++ options.map(_.getShortName)).toSet ++ Set("--")
    val diff = switchKeys.diff(keys)

    if(!(diff.isEmpty || diff == keys))
      throw new RuntimeException("Error: invalid options " + diff)

    new OptResult(operands, switches, properties)

  } // -- EoF fun. parse ---- //


  def parseRun(argList: List[String]): Unit =
    argList match {
      case List()
          => if(this.helpFlag)
            this.showHelp()
          else
            _action(this.parse(argList))
      case _
          =>
        _action(this.parse(argList))
  }


} // ---- End of class OptCommand ---- // 


/** Command line parser with git and busybox like sub-commands or services. */
class OptParser(
  programName: String = "",
  usage:       String = "",
  desc:        String = "",
  ){
  import scala.collection.mutable.{Map, ListBuffer}
  private val commands = ListBuffer[String]()
  private val parsers = Map[String, IOptCommand]()

  def add(opt: IOptCommand) = {
    commands.append(opt.getCommandName())
    parsers += opt.getCommandName() -> opt
    this 
  }

  def showHelp() = {
    val rows = commands.toList map {name =>
      val c = parsers(name)
      List(c.getCommandName(), c.getCommandDesc())
    }
    println(desc)
    if(usage == "")
      println(s"Usage: $$ $programName [COMMAND] [OPTIONS] [<ARGS> ...]")
    else
      println(usage)
    println()
    println("Commands:\n")
    OptFun.printTableOfRows(rows)
  }

  def parse(args: List[String]) =
    args match {
      case List()
          => {           
            showHelp()
            System.exit(0)
          }

      case List("-h") | List("-help")
          => {           
            showHelp()
            System.exit(0)
          }        

      // Show sub-command help 
      case List(command, "-h") 
          => parsers.get(command) match {
            case Some(cmd) =>
              cmd.showHelp()
            case None => {
              println(s"Error: invalid command: $command.")
              System.exit(1)
            }
          }

      // Show sub-command help         
      case List(command, "-help") 
          => parsers.get(command) match {
            case Some(cmd) =>
              cmd.showHelp()
            case None => {
              println(s"Error: invalid command: $command.")
              System.exit(1)
            }
          }

      case command::rest 
        => parsers.get(command) match {
          case Some(cmd)
              => cmd.parseRun(rest)
          case None
              => {
                println(s"Error: invalid command: $command")
                System.exit(1)
              }
        }
    }
}

