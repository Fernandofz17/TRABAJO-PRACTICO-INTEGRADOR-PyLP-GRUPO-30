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



;; ========================================================
;; FUNCIÓN: timer-semaforo
;; NATURALEZA: Pura
;; ESTRATEGIA: Cálculo matemático
;; IMPACTO: No destructiva
;; ========================================================
;; DESCRIPCIÓN DEL FUNCIONAMIENTO
;;
;; Esta función determina automáticamente qué color del
;; semáforo debe encontrarse activo para un instante de
;; tiempo determinado expresado en formato Unix.
;;
;; A diferencia de una implementación con tiempos fijos,
;; las duraciones de cada estado no se encuentran
;; codificadas directamente en el programa. Los valores
;; son obtenidos dinámicamente desde el archivo externo
;; "config.json" mediante las funciones
;; LEER-CONFIGURACION y OBTENER-TIEMPO.
;;
;; Gracias a este diseño, cualquier modificación realizada
;; sobre los tiempos de rojo, amarillo o verde impactará
;; automáticamente en el comportamiento del sistema sin
;; necesidad de modificar ni recompilar el código fuente.
;;
;; La función calcula la duración total del ciclo y luego
;; utiliza la operación modular (MOD) para determinar en
;; qué posición del ciclo se encuentra el instante
;; consultado.
;;
;; Con la configuración actual de prueba config.json:
;;
;; {
;;   "rojo": 90,
;;   "amarillo": 6,
;;   "verde": 120
;; }
;;
;; el ciclo total es de 216 segundos y la secuencia de
;; estados queda definida como:
;;
;; ROJO (0-89 s)
;; VERDE (90-209 s)
;; AMARILLO (210-215 s)
;;
;; Luego el ciclo vuelve a comenzar desde ROJO.
;;
;; Ejemplos:
;;
;; (timer-semaforo 0)   => ROJO
;; (timer-semaforo 90)  => VERDE
;; (timer-semaforo 210) => AMARILLO
;; (timer-semaforo 216) => ROJO
;;
;; Esta función participa además del principio de
;; composición funcional, ya que reutiliza las funciones
;; OBTENER-TIEMPO y LEER-CONFIGURACION para obtener los
;; parámetros necesarios para su cálculo.
;; ========================================================
(defun timer-semaforo (tiempo-unix)
  (let* ((rojo (obtener-tiempo :rojo))
         (amarillo (obtener-tiempo :amarillo))
         (verde (obtener-tiempo :verde))
         (ciclo (+ rojo amarillo verde))
         (instante (mod tiempo-unix ciclo)))

    (cond
      ;; 0 a 89
      ((< instante rojo)
       'rojo)

      ;; 90 a 209
      ((< instante (+ rojo verde))
       'verde)

      ;; 210 a 215
      (t
       'amarillo))))
;; ========================================================
;; FUNCIÓN: log-cambio
;; NATURALEZA: Impura
;; ESTRATEGIA: Formateo de salida
;; IMPACTO: No destructiva
;; ========================================================

(defun log-cambio (tiempo color-anterior color-nuevo)
  (format t
          "Tiempo ~A: la luz ha cambiado de ~A a ~A~%"
          tiempo
          color-anterior
          color-nuevo))

;; ========================================================
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Predicado (COND)
;; IMPACTO: No destructiva
;; ========================================================

(defun recomendacion-ciclo (duracion)
  (cond
    ((< duracion 35)
     "Ciclo demasiado corto")

    ((<= duracion 150)
     "Ciclo dentro del rango recomendado")

    (t
     "Ciclo demasiado largo")))

;; ========================================================
;; FUNCIÓN: duracion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Cálculo aritmético
;; IMPACTO: No destructiva
;; ========================================================

(defun duracion-ciclo ()
  (+ (obtener-tiempo :rojo)
     (obtener-tiempo :amarillo)
     (obtener-tiempo :verde)))


;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Cálculo aritmético
;; IMPACTO: No destructiva
;; ========================================================

(defun ciclos-por-tiempo (minutos)
  (floor (/ (* minutos 60)
            (duracion-ciclo))))

;; ========================================================
;; FUNCIÓN: distribucion-temporal
;; NATURALEZA: Pura
;; ESTRATEGIA: Cálculo aritmético
;; IMPACTO: No destructiva
;; ========================================================

(defun distribucion-temporal ()
  (let* ((rojo (obtener-tiempo :rojo))
         (amarillo (obtener-tiempo :amarillo))
         (verde (obtener-tiempo :verde))
         (ciclo (duracion-ciclo)))

    (list
      (cons 'rojo (* 100.0 (/ rojo ciclo)))
      (cons 'amarillo (* 100.0 (/ amarillo ciclo)))
      (cons 'verde (* 100.0 (/ verde ciclo))))))

;; ========================================================
;; FUNCIÓN: leer-configuracion
;; NATURALEZA: Impura
;; ESTRATEGIA: Lectura de archivo JSON
;; IMPACTO: No destructiva
;; ========================================================

(defun leer-configuracion ()
  (with-open-file (stream "config.json"
                          :direction :input)
    (cl-json:decode-json stream)))


;; ========================================================
;; FUNCIÓN: obtener-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Búsqueda en lista asociativa
;; IMPACTO: No destructiva
;; ========================================================

(defun obtener-tiempo (color)
  (cdr (assoc color
              (leer-configuracion)
              :test #'eq)))


