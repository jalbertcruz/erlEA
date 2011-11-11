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
package update_erl_from_clj

import java.io.File

object Update_erl_from_clj extends App {

//  for (file <- (new File("../../EA/src")).listFiles) {
    for (file <- (new File(args(0))).listFiles) {
    if (file.getName.endsWith(".erl")) {
      val cc = new Clj2erlConversor(args(1), args(2))
      cc.genModuleErldoc(new File(args(0)), file.getName.substring(0, file.getName.length - 4), args(3));
    }
  }

}