package license

import java.io.File
import java.io.PrintWriter

object FileUtility {

  val lnseparator = "\r\n"
  
  def writeToFileAsString(f: File, v: AnyRef, encoding: String) {

    val pw = new PrintWriter(f, encoding)

    try {

      pw.print(v.toString)

    } finally {

      pw.close()
    }

  }

}