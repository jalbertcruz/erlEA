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
(ns masterSlaveModel.trabajador)

(def module-trabajador

  [
    {:doc "
     Módulo que implementa el comportamiento de los trabajadores.
     "}

    {:actor {
              "{calcula, Cromosoma} ->" "Mensaje que le ordena a un trabajador realizar el cálculo."
              }

     :functions []
     }

    ])