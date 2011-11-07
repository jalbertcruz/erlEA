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