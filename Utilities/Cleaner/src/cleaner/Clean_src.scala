package cleaner

import java.io.File
import license.ConfigurationCljJava
import license.FileUtility._

class Clean_src(cljScript: String, expression: String) extends ConfigurationCljJava(cljScript, expression) {

  def clean_comments_from_file(f: File) {
    val exc = isExcluded(f)
    if (!exc) {
      if (f.isDirectory) {
        for (file <- f.listFiles()) {
          clean_comments_from_file(file)
        }
      } else {
        val prefix = Prefix(f)
        if (prefix != "") {
          // Elimino las lineas que comiencen como comentarios

          val res = scala.io.Source.fromFile(f, "utf-8").getLines.drop(header_lines_count - 1).filter((l: String) => {
            if (f.getName().endsWith(".erl")) {
              !l.trim().startsWith("%%")
            } else true
          }).mkString(lnseparator)

          writeToFileAsString(f, res, "UTF-8")

        }

      }
    }
  }

  def clean {
    clean_comments_from_file(root)
  }
}