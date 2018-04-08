#!/bin/sh
DEPS=com.google.zxing/core/2.2,com.google.zxing/javase/2.2
exec jarget script -ps="$DEPS" -- "$0" "$@"
!#

object QRCode { 

  import javax.imageio.ImageIO;
  import java.io._
  import java.util.HashMap
  import com.google.zxing.{BarcodeFormat, BinaryBitmap, EncodeHintType, MultiFormatReader}
  import com.google.zxing.{MultiFormatWriter, NotFoundException, Result, WriterException}
  import com.google.zxing.client.j2se.MatrixToImageWriter
  import com.google.zxing.common.{ BitMatrix, HybridBinarizer}
  import com.google.zxing.qrcode.decoder.ErrorCorrectionLevel

  def writeToFile(
    data:    String,
    file:    String  = "out.png",
    width:   Int     = 200,
    height:  Int     = 200,
    charset: String  = "UTF-8") = {
    val hintMap = {
      val h = new HashMap[EncodeHintType, ErrorCorrectionLevel]();
      h.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.L);
      h
    }
    val matrix = new MultiFormatWriter().encode(
      new String(data.getBytes(charset), charset),
      BarcodeFormat.QR_CODE, width, height, hintMap)
    MatrixToImageWriter.writeToFile(matrix, "png", new File(file));
  }


  def writeToImage(
    data:    String,
    width:   Int     = 200,
    height:  Int     = 200,
    charset: String  = "UTF-8" ): java.awt.image.BufferedImage = {
    val hintMap = {
      val h = new HashMap[EncodeHintType, ErrorCorrectionLevel]();
      h.put(EncodeHintType.ERROR_CORRECTION, ErrorCorrectionLevel.L);
      h
    }
    val matrix = new MultiFormatWriter().encode(
       new String(data.getBytes(charset), charset),
      BarcodeFormat.QR_CODE, width, height, hintMap)
    MatrixToImageWriter.toBufferedImage(matrix)
  }


  /** Show QR code in a JFrame */
  def show(
    data:    String,
    width:   Int         = 200,
    height:  Int         = 200,
    charset: String      = "UTF-8",
    title:   String      = "QRCode",
    exitOnClose: Boolean = false
  ) = {
    import javax.swing.{ImageIcon, JFrame, JLabel, JPanel}
    val bimg  = writeToImage(data, width, height, charset)
    val frame = new javax.swing.JFrame("Frame 1")
    frame.setSize(400, 400)
    val pic = new javax.swing.JLabel(new ImageIcon(bimg))
    frame.add(pic)
    if (exitOnClose)
      frame.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)
    frame.setVisible(true)
  }

} // ------- End of Object QRCode ------------ // 


val testUrl = "https://msdn.microsoft.com/en-us/library/ff798384.aspx"

args.toList match { 
  case List("-show", data)
      => QRCode.show(data, exitOnClose = true)
  case List("-file", data, file)
      => QRCode.writeToFile(data, file)

  case List("-test1")
      => {
        println("Generating QRcode for testing URL: " + testUrl)
        QRCode.show(testUrl, exitOnClose = true)
      }

  case List("-test2")
      => {
        println("Generating QRcode image file images/qrcodeTest.png for testing URL: " + testUrl)
        QRCode.writeToFile(testUrl, "images/qrcodeTest.png")
      }

  case _
      => {
        println("Valid commands")
        println("$ jqrcode -file <file> <data>")
        println("$ jqrcode -show <data>")
      }   
}
