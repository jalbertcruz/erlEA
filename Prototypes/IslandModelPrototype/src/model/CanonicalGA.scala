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

trait CanonicalGA {

  var population: List[Individual] = _

  var selectParents: (List[Individual]) => List[Individual] = _

  var recombination: (List[Individual]) => List[Individual] = _

  var mutation: (List[Individual]) => List[Individual] = _

  var selectNewPopulation: (List[Individual], List[Individual]) => List[Individual] = _

  def evolve {

    var p1 = selectParents(population)

    p1 = recombination(p1)

    p1 = mutation(p1)

    population = selectNewPopulation(population, p1)

  }
}