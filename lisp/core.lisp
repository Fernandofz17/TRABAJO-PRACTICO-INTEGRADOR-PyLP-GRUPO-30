;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SISTEMA DE SEMÁFOROS INTELIGENTES
;; TRABAJO PRÁCTICO INTEGRADOR - PyLP 2026
;;
;; CONFIGURACIÓN EXTERNA MEDIANTE JSON
;;
;; Este sistema utiliza un archivo externo llamado
;; "config.json" para almacenar los tiempos de duración
;; de cada estado del semáforo.
;;
;; Ejemplo:
;;
;; {
;;   "rojo": 90,
;;   "amarillo": 6,
;;   "verde": 120
;; }
;;
;; La función LEER-CONFIGURACION carga el contenido del
;; archivo utilizando la librería CL-JSON, mientras que
;; la función OBTENER-TIEMPO permite acceder al tiempo
;; asociado a cada color.
;;
;; Gracias a este mecanismo, los parámetros del sistema
;; se encuentran desacoplados del código fuente, por lo
;; que es posible modificar la duración de los estados
;; sin necesidad de alterar ni recompilar el programa.
;;
;; Requisitos:
;; - SBCL
;; - Quicklisp
;; - CL-JSON
;;
;; Para cargar la librería:
;; (ql:quickload :cl-json)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional (COND)
;; IMPACTO: No destructiva
;; ========================================================

(defun transicion (color-actual cambiar-a)
  (cond
    ((and (eq color-actual 'en-rojo)
          (eq cambiar-a 'verde))
     (list 'en-rojo "cambiar-a-verde"))

    ((and (eq color-actual 'en-verde)
          (eq cambiar-a 'amarillo))
     (list 'en-verde "cambiar-a-amarillo"))

    ((and (eq color-actual 'en-amarillo)
          (eq cambiar-a 'rojo))
     (list 'en-amarillo "cambiar-a-rojo"))

    (t
     (list color-actual 'accion-por-defecto))))
