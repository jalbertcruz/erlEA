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
package model

/**
 * @param migrantsSelection Selección de los individuos migrantes.
 *
 * @param replaceSelection 
 * 
 * Selección de los índices de individuos a reemplazar cuando llega una migración (comenzando en cero).
 *
 * @param neighbor Deme destino de los migrantes.
 *
 */
class Deme(

  var migrantsSelection: () => List[Individual],

  var replaceSelection: () => List[Int],

  var neighbor: Deme) extends CanonicalGA {

  private[this] var cantGeneraciones = 0

  /**
   * Criterio para decidir cuándo migrar.
   */
  def migrate = cantGeneraciones % 3 == 0

  override def evolve {

    super.evolve

    cantGeneraciones += 1

    if (migrate) doMigration()

  }

  /**
   *  @param mig 
   *  
   *  Listado de índices, ordenados de menor a mayor.
   *  
   *  @return 
   *  
   *  El primero de los números será el índice del primer seleccionado, y a partir del segundo
   *  estará la distancia a la que se encuentra la posición i de la i-1 
   */
  private[this] def convIndexes(mig: List[Int]) = {
    val r1 = Array[Int](mig.length)
    r1(0) = mig(0)
    var i = 1
    while (i < mig.length) {
      r1(i) = mig(i) - mig(i - 1)
      i += 1
    }
    r1.toList
  }

  
  private[this] def eliminar(el: List[Individual], ind: List[Int]): List[Individual] = {
    
    if (!ind.isEmpty) {

      val ci = ind.head
      val i = ind.tail
      val (a, b) = el.splitAt(ci - 1)
      a ++ eliminar(b.tail, i)

    } else el
  }

  /**
   * @param mig Listado de los individuos recibidos. 
   */
  def recieveMigration(mig: List[Individual]) {

    val rsel = replaceSelection()

    assert(mig.length == rsel.length)

    val i = convIndexes(rsel)

    val (a, b) = population.splitAt(i(0) + 1)

    val res = a.init ++ eliminar(b, i.tail)

    population = res ++ mig

  }

  def doMigration() {

    val mig = migrantsSelection()

    neighbor.recieveMigration(mig)

  }

}