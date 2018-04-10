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

