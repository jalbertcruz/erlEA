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

import clojure.lang.APersistentMap
import clojure.lang.RT
import clojure.lang.PersistentVector
import clojure.lang.Keyword
import clojure.lang.PersistentHashMap

import license.ConfigurationCljJava
import license.FileUtility._
import java.io.File
import java.util.Set
import java.util.regex.Matcher
import java.util.regex.Pattern

class Clj2erlConversor extends ConfigurationCljJava ("config.clj", "doc.config/configuration") {

   def genModuleErldoc( moduleName: String ){
        
        val erlf = new File(target, moduleName + ".erl");
        
//        System.out.println("Procesando: " + erlf.getName())
        if (isExcluded(erlf)) {
            System.out.println("Exluido: " + erlf.getName())
            return;
        }

        RT.loadResourceScript(moduleName + ".clj");

        val pv = clojure.lang.Compiler.eval(RT.readString("(eval '" + "doc." + moduleName + "/module-" + moduleName + ")")).asInstanceOf[PersistentVector]

        var hm : Map[String, String] = Map[String, String]()

        var map = pv.get(0).asInstanceOf[APersistentMap]

        val mdoc = getModuleErldoc(map)

        hm += ("-module(" -> mdoc)

        map = pv.get(1).asInstanceOf[APersistentMap]

        hm ++= getActorEntries(
                 map.get(Keyword.intern("actor")).asInstanceOf[APersistentMap])
                 
        hm ++= getFunctionsEntries(
                map.get(Keyword.intern("functions")).asInstanceOf[PersistentVector])

        val lines = scala.io.Source.fromFile(erlf, "utf-8").getLines

        val p: Pattern = Pattern.compile("^\\s*")

        var res = ""

        for (linea <- lines) {
             val it = hm.keySet.iterator
              while (it.hasNext) {
			    val k = it.next().asInstanceOf[String]
                if (linea.trim().startsWith(k)) {
                    val m: Matcher = p.matcher(linea)
                    m.find
                    res += m.group + hm(k).replace(lnseparator, lnseparator + m.group) + lnseparator
                }
			  }

            res += linea + lnseparator

        }

        writeToFileAsString(erlf, res, "UTF-8")
        
//        System.out.println(erlf.getName());

    }

    private[this] def getActorEntries(pmap: APersistentMap): Map[String, String] = {
      
         var res = Map[String, String]()

         val it = pmap.keySet.iterator()
         
         while (it.hasNext) {
           
		    val k = it.next().asInstanceOf[String]
			    
            val spts = pmap.get(k).toString().trim().split("\n")
            
            val spts1 = spts.map(_.trim)
            
            val ent = "%% " + spts1.mkString( lnseparator + "%% ")

            res += (k -> ent)
		    
         }

        res
    }

    def getModuleErldoc( pmap: APersistentMap ): String = {
        
    	var res = "%% " + lnseparator

    	val it = pmap.keySet.iterator
    	
        while (it.hasNext) {
          
           val k = it.next.asInstanceOf[Keyword]
           
           val spts = pmap.get(k).toString().trim().split("\n")
           
           val spts1 = spts.map(_.trim)

           res += "%% @" + k.getName + " " + spts1(0)

           res += spts1.drop(1).mkString(lnseparator + "%% ")
           
           res += lnseparator + "%% "
           
         }
        
        res
    }

    private[this] def getFunctionsEntries(vec: PersistentVector): Map[String, String] = {
      
        var funcEntries = Map[String, String]()
        
        val it = vec.iterator
        
        while (it.hasNext) {

            var hm =  it.next.asInstanceOf[PersistentHashMap]

            val key = hm.get(Keyword.intern("index")).asInstanceOf[String]
            var res = "%% " + lnseparator
            // Eliminación de la entrada "index"
            hm = hm.without(Keyword.intern("index")).asInstanceOf[PersistentHashMap]
            
            val it1 = hm.keySet.iterator
            
            while(it1.hasNext) {
              
                val k = it1.next.asInstanceOf[Keyword]
                
                val spts = hm.get(k).toString().trim().split("\n")
                val spts1 = spts.map(_.trim)
                
                res += "%% @" + k.getName() + " " + spts1(0)
                
                
                res += spts.drop(1).mkString( lnseparator + "%% ") + lnseparator + "%% "
            }

            funcEntries +=(key -> res)
        }

        funcEntries
    }
    
}