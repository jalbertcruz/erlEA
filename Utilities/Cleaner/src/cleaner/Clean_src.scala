// 
// Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
// Copyright 2011 by José Albert Cruz Almaguer.
// 
// This program is licensed to you under the terms of version 3 of the
// GNU Affero General Public License. This program is distributed WITHOUT
// ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
// MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
// AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
// 
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
              //!l.trim().startsWith("%%")
              true
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