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

import collection.mutable.HashTable
import java.io.File
import collection.immutable.Map
import collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import clojure.lang._
import license.FileUtility._

class ConfigurationCljJava(cljScript: String, expression: String) {

  protected[this] var associations: Map[String, String] = Map[String, String]()
  protected[this] var parent: File = null
  protected[this] var excluded: ListBuffer[File] = new ListBuffer[File]()
  protected[this] var header: String = _
  protected[this] var header_lines_count: Int = _

  def Excluded(f: File): Boolean = !excluded.contains(f)

  RT.loadResourceScript(cljScript)

  var map: PersistentHashMap = Compiler.eval(RT.readString("(eval '" + expression + ")")).asInstanceOf[PersistentHashMap]

  // Determinación de los excluidos:
  var pv: PersistentVector = map.get(Keyword.intern("excluded")).asInstanceOf[PersistentVector]

  var it = pv.iterator();
  while (it.hasNext) {
    val g = it.next().asInstanceOf[String]
    excluded += new File(g)
  }

  pv = map.get(Keyword.intern("associations")).asInstanceOf[PersistentVector]

  it = pv.iterator();
  while (it.hasNext) {
    val g = it.next().asInstanceOf[PersistentVector]
    associations += (g.get(0).asInstanceOf[String] -> g.get(1).asInstanceOf[String])
  }

  header = map.get(Keyword.intern("header")).asInstanceOf[String]

  header_lines_count = header.split("\n").length

  parent = new File(map.get(Keyword.intern("target")).asInstanceOf[String])

  protected[this] def Prefix(f: File) = {
    var res = "";
    associations.keys.foreach {
      k =>
        if (f.getName.endsWith(k))
          res = associations(k)
    }
    res
  }
  
   protected[this] def isExcluded(f: File): Boolean = {
        var exc = false
        val it = excluded.iterator()
        while(it.hasNext() && !exc) {
            var file = it.next()
            if (file.equals(f)) {
                exc = true
            }
        }
        exc
    }

}