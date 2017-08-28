package jarget.logger

object Log{

  import java.util.logging.{Logger, Level}

  val logger = Logger.getLogger("jarget.logger")

  def setLevel() = {
    val level = Option(System.getProperty("jarget.log"))
    level match {
      case Some("ALL")     => logger.setLevel(Level.ALL)
      case Some("OFF")     => logger.setLevel(Level.OFF)        
      case Some("INFO")    => logger.setLevel(Level.INFO)
      case Some("CONFIG")  => logger.setLevel(Level.CONFIG)
      case Some("FINE")    => logger.setLevel(Level.FINE)
      case Some("WARNING") => logger.setLevel(Level.WARNING)
      case Some("SEVERE")  => logger.setLevel(Level.SEVERE)
      case None            => logger.setLevel(Level.OFF)
      case _               => ()
    }
  }

}
