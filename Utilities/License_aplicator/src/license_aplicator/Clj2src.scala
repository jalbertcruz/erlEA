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
package license_aplicator

import java.io.File

import license.ConfigurationCljJava
import license.FileUtility._

class Clj2src extends ConfigurationCljJava("config.clj", "doc.config/configuration") {

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
    apply_license_to_file(parent)
  }
}