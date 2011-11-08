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
(ns doc.config)

(def configuration
  {
    :target "../../EA/src/"

    :root "../../"

    :header ; Encabezado a poner
    "
    Author José Albert Cruz Almaguer <jalbertcruz@gmail.com>
    Copyright 2011 by José Albert Cruz Almaguer.

    This program is licensed to you under the terms of version 3 of the
    GNU Affero General Public License. This program is distributed WITHOUT
    ANY EXPRESS OR IMPLIED WARRANTY, INCLUDING THOSE OF NON-INFRINGEMENT,
    MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Please refer to the
    AGPL (http://www.gnu.org/licenses/agpl-3.0.txt) for more details.

    "

    ; Inicio de línea por tipo de fichero, solo te tendrán en cuenta los ficheros con estas extensiones.
    :associations [
                    [".erl" "%% "]
                    [".clj" ";; "]
                    [".java" "// "]
                    [".py" "## "]
                    [".rb" "## "]
                    [".bat" "rem "]
                    [".scala" "// "]
                    ]

    :excluded [".gitignore" ".git/" "LICENSE" "README.txt" "NOTICE" "mochijson2.erl"]

    }
  )