package jarget.crypto


object Digest{

  def withResource[A](res: { def close(): Unit})(handler: => Unit){
    try {
      handler
    } catch {
      case ex: java.io.IOException => ex.printStackTrace()
    } finally {
      res.close()
    }
  }

  def readBytesWhile(
    in:   java.io.InputStream,
    size: Int = 1024
  )(handler: (Array[Byte], Int) => Unit) = {
    val buf    = new Array[Byte](size)
    var nread = 0
    while({nread = in.read(buf) ; nread} > 0 ){
      handler(buf, nread)
    }
  }

  def bytesToHexStr(bytes: Array[Byte]) = {
    val buf    = new StringBuffer()
    val digits = Array(
      '0', '1', '2', '3',
      '4', '5', '6', '7',
      '8', '9',
      'a', 'b', 'c',
      'd', 'e', 'f'
    )
    for (b <- bytes) {
      val bb   = (b + 0xff + 1) & 0xff
      val dSup = bb >> 4
      val dInf = bb & 0x0F
      buf.append(digits(dSup))
      buf.append(digits(dInf))
    }
    buf.toString
  }


  def fileDigestSum(algorithm: String, file: String) = {
    val md  = java.security.MessageDigest.getInstance(algorithm)
    val fis = new java.io.FileInputStream(file)
    withResource(fis){
      readBytesWhile(fis){ (data, n) => md.update(data, 0, n)}
    }
    bytesToHexStr(md.digest())
  }

  def stringDigestSum(algorithm: String, str: String) = {
    val md  = java.security.MessageDigest.getInstance(algorithm)
    md.update(str.getBytes())
    bytesToHexStr(md.digest())
  }

}

