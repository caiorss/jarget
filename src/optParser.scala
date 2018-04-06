package jarget.optParser

private object OptUtils{
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

class CmdOpt(
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


class OptResult(
  operands: List[String],
  switches: Map[String, List[String]],
  options: scala.collection.mutable.ListBuffer[CmdOpt]
){

  def getOperands() = operands
  def getSwitches() = switches

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
    pw.println("Operands ")
    this.getOperands() foreach pw.println
    pw.println("Arguments ")
    this.getSwitches() foreach pw.println
    sw.toString
  }

}


class OptSet(name: String = "", usage: String = "", desc: String = ""){
  import scala.collection.mutable.ListBuffer
  val switchMark = "-"
  val switchSeparator = "="
  private val options  = ListBuffer[CmdOpt]()
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
    this.options.append(new CmdOpt(name, shortName, argName, desc))
    this
  }

  def setAction(action: OptResult => Unit) = {
    _action = action
    this
  }    

  def getOptions() =
    options.toList

  def showHelp() = {
    val name  = this.getCommandName()
    val desc  = this.getCommandDesc()
    val usage = this.getCommandUsage()

    if(name != "") {
      println(s"Usage: $name $usage")
      println()
    }
    if(desc != "") { println(desc); println()}

    val rows = options.toList map {o =>
      val argName = o.getArgName()      
      List(
        "-" + o.getName + (if (argName != "") ("=" + argName) else ""),
        "-" + o.getShortName() + (if (argName != "") ("=" + argName) else ""),
        o.getDesc()
      )
    }
    OptUtils.printTableOfRows(rows)
  }

  def parse(argList: List[String]) = {

    val switchesLST = argList.filter(_.startsWith(switchMark)) map { p => 
      p.split(switchSeparator, 2) match {
        case Array(k, v) => (k.stripPrefix(switchMark), v)
        case Array(k)    => (k.stripPrefix(switchMark), "")
        case _           => null
      }
    }
    val switches = switchesLST
      .groupBy{case (k, v) => k}
      .map{case (k, xs) =>
        val name = options
          .find(n => n.getName() == k || n.getShortName() == k)
          .map(_.getName)
          .getOrElse(k)
        (name, xs map (_._2))
    }

    val operands = argList.filter(!_.startsWith(switchMark))

    // Validate result
    val switchKeys = switches.map(_._1).toSet
    val keys = (options.map(_.getName) ++ options.map(_.getShortName)).toSet
    val diff = switchKeys.diff(keys)

    if(!(diff.isEmpty || diff == keys))
      throw new RuntimeException("Error: invalid options " + diff)
    new OptResult(operands, switches, options)
  }
}


class OptParser{
  import scala.collection.mutable.Map
  private val parsers = Map[String, (OptSet, OptResult => Unit)]()

  def add(opt: OptSet)(handler: OptResult => Unit) =
    parsers += opt.getCommandName() -> (opt, handler)

  def showCommands() = {
    println("Available Commands")
    for(cmd <- parsers.keys) 
      println(" - " + cmd + " <args> <operands> ")    
  }


  def parse(args: List[String]) =
    args match {
      case List()
          => {
            println("Show help to user")
            showCommands()
            System.exit(0)
          }
      case List(command)
          => parsers.get(command) match {
            case Some((cmd, handler)) =>
              cmd.showHelp()
            case None => {
              println(s"Error: invalid $command not found.")
              System.exit(1)
            }
          }
      case command::rest 
        => parsers.get(command) match {
          case Some((cmd, handler))
              => handler(cmd.parse(rest))
          case None
              => {
                println(s"Error: Command $command not found.")
                System.exit(1)
              }
        }
    }
}

