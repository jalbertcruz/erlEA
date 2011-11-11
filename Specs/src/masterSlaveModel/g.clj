;; 
;; Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
;; Copyright 2011 by José Albert Cruz Almaguer.
;; 
;; This program is licensed to you under the terms of version 3 of the
;; GNU Affero General Public License. This program is distributed WITHOUT
;; ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
;; MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
;; AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.
;; 
(ns masterSlaveModel.g)

(def module-g
  [
    {:doc ; El primer elemento del array es un map cuyas llaves son las etiquetas del erldoc
     "
     Módulo principal. Encargado de brindar los servicios,
     registra un proceso bajo el nombre de <g>.
     "
     }

    {:actor {; El segundo es un map con las entradas del actor

             "{addW, N, FF} ->" "Mensaje que dado un número <N> y una función <FF> le manda un mensaje equivalente al <colector> (aumenta la cantiadd de trabajores)."

             "{remW, N} ->" "Mensaje que dado un número <N> le manda un mensaje equivalente al <colector> (disminuye la cantidad de trabajadores)."

             "{chfitness, NF} ->" "Mensaje que dada una función <NF> le manda un mensaje equivalente al <colector>
             (camiando la función de fitness que se usa en los trabajadores)."

             "{calc, Generaciones} ->" "Aplicación de un caso de pruebas con 150 individuos y cromosomas de 8 genes."

             "{calc, Poblacion, Generaciones} ->" "Mensaje mediante el que se le pide a <g> que calcule para una población <Población> y una cantidad de generaciones <Generaciones> la población resultante."

             "{itera, Seleccion, N} when N == 0 ->" "Se han realizado todas las iteraciones (mensaje privado)"

             "{itera, Seleccion, N} when N > 0 ->" ""

             "{calculado, Res} -> " ""
             }


     :functions [; El tercero tiene la info de las funciones
                 {:index "s(NTrabajadores)->"
                  :doc "
     Función de inicio, es la que debe usarse para levantar el proceso principal (denominado <g>), el colector (de nombre <colector>) y los procesos trabajadores (se crean inicialmente dada la cantidad especificada por parámetros).
     "
                  }

                 {:index "s()->"
                  :doc "
      Función de inicio por defecto, construye 150 procesos trabajadores y levanta el colector y el proceso principal.
      "
                  }

                 {:index "iterar(Seleccion, FSeleccionPares, FCruce, FMutar)->"
                  :doc "
      Dada una selección de cromosomas y funciones para obtener los pares a cruzar, el cruce y la mutación. Devuelve la nueva población.
      "
                  }

                 {:index "imprimirPoblacion(P)->"
                  :doc "
      En el caso de esta funcion he decidido no usar los esclavos dado que es utilitaria (ver los individuos)...
      "
                  }


                 {:index "cruce({P1, P2})->"
                  :doc "
      Cruce usando dos puntos, se genera uno de ellos y luego el segundo se obtiene de forma también aleatoria hacia el extremo derecho del primero.
      "
                  }

                 {:index "mutar(S)->"
                  :doc "
      Dado un cromosoma realiza una mutación en un gen aleatorio.
      "
                  }

                 {:index "gp(Tam, NCromosomas)->"
                  :doc "
      Generador de poblaciones, por razones de comodidad en esta versión se generan múltiplos de 3.
      "
                  }

                 {:index "fitness(Max, [])->"
                  :doc "
      Cálculo de la longitud de la subcadena con la cantidad máxima de 1s.
      "
                  }
                 ]
     }
    ])