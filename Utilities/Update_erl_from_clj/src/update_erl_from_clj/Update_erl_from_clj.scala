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