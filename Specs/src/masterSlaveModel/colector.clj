(ns masterSlaveModel.colector)

(def module-colector

  [
    {:doc "
     Módulo encargado de administrar los procesos esclavos que harán el cálculo del fitness. Durante su inicio se registra un proceso bajo el nombre de <colector> encargado de brindar los servicios del módulo.
     "}

    {:actor {"{chfitness, NF} ->" "Cambio de la función fitness en los procesos trabajadores."

             "{addW, N, FF} ->" "Aumento de los trabajadores (en la cantidad <N> y usando la función <FF>)."

             "{remW, N} ->" "Eliminación de trabajadores."

             "{filtrar, Poblacion, Generaciones} ->" "Servicio de cálculo de los fitness por los trabajadores."

             "{calculado, Cromosoma, Fitness, Pid} ->" "Mensaje que le llega al colector cada vez que un trabajador realiza su tarea."

             }

     :functions []
     }
    ])