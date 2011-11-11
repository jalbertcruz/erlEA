package license_aplicator

import java.io.File

import license.ConfigurationCljJava
import license.FileUtility._

class Clj2src(cljScript: String, expression: String) extends ConfigurationCljJava(cljScript, expression) {

  def apply_license_to_file(f: File) {
    val exc = isExcluded(f)
    if (!exc) {
      if (f.isDirectory()) {
        for (file <- f.listFiles()) {
          apply_license_to_file(file)
        }
      } else {
        
        val prefix = Prefix(f)
        
        if (!prefix.equals("")) {
          // Pongo el encabezado con los prefijos adecuados.
          var res = prefix;

          for (string <- header.trim().split("\n")) {
            res += lnseparator + prefix + string.trim()
          }

          res += lnseparator + prefix;

          val data = scala.io.Source.fromFile(f, "utf-8").mkString
          
          writeToFileAsString(f, res + lnseparator + data, "UTF-8")

        }
      }
    }
  }
  def apply_license {
    apply_license_to_file(root)
  }
}