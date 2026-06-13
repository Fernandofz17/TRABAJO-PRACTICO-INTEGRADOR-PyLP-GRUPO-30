;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SISTEMA DE SEMÁFOROS INTELIGENTES
;; TRABAJO PRÁCTICO INTEGRADOR - PyLP 2026
;;
;; ITERACION 2 - INTERMITENCIA DE SEGURIDAD
;;
;; Se agregan estados intermedios de
;; AMARILLO-INTERMITENTE durante 3 segundos
;; entre las transiciones principales.
;;
;; Secuencia completa:
;;
;; ROJO
;; ↓
;; AMARILLO-INTERMITENTE (3 s)
;; ↓
;; VERDE
;; ↓
;; AMARILLO-INTERMITENTE (3 s)
;; ↓
;; ROJO
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|ACLARACION: La logica de las funciones es la misma que la utilizada en la Iteracion uno, por eso
se omiten comentarios explicaciticos de aca funcion, a fin de no extender el codigo innecesariamente
, solo se aplica intermitencia solicitada en la iteracion 2, eso teniendo en cuenta la Extension 1.
En cuanto a al Extension 2 se agrega la funcion encargada de la persistencia de datos, que crea el informe.|#



;; ========================================================
;; FUNCIÓN: transicion
;; NATURALEZA: Pura
;; ESTRATEGIA: Condicional (COND)
;; IMPACTO: No destructiva
;;
;; Modela las transiciones válidas incluyendo los
;; nuevos estados de intermitencia.
;; ========================================================

(defun transicion (color-actual cambiar-a)
  (cond

    ((and (eq color-actual 'rojo)
          (eq cambiar-a 'amarillo-intermitente))
     (list 'rojo "cambiar-a-amarillo-intermitente"))

    ((and (eq color-actual 'amarillo-intermitente)
          (eq cambiar-a 'verde))
     (list 'amarillo-intermitente "cambiar-a-verde"))

    ((and (eq color-actual 'verde)
          (eq cambiar-a 'amarillo-intermitente))
     (list 'verde "cambiar-a-amarillo-intermitente"))

    ((and (eq color-actual 'amarillo-intermitente)
          (eq cambiar-a 'rojo))
     (list 'amarillo-intermitente "cambiar-a-rojo"))

    (t
     (list color-actual 'accion-por-defecto))))


;; ========================================================
;; FUNCIÓN: timer-semaforo
;; NATURALEZA: Pura
;; ESTRATEGIA: Cálculo matemático
;; IMPACTO: No destructiva
;;
;; Determina automáticamente el estado del
;; semáforo según el tiempo Unix recibido.
;; ========================================================

(defun timer-semaforo (tiempo-unix)

  (let* ((rojo (obtener-tiempo :rojo))
         (verde (obtener-tiempo :verde))
         (intermitencia (obtener-tiempo :intermitencia))

         (ciclo (+ rojo
                   intermitencia
                   verde
                   intermitencia))

         (instante (mod tiempo-unix ciclo)))

    (cond

      ;; ROJO
      ((< instante rojo)
       'rojo)

      ;; INTERMITENCIA ANTES DE VERDE
      ((< instante (+ rojo intermitencia))
       'amarillo-intermitente)

      ;; VERDE
      ((< instante (+ rojo intermitencia verde))
       'verde)

      ;; INTERMITENCIA ANTES DE ROJO
      (t
       'amarillo-intermitente))))


;; ========================================================
;; FUNCIÓN: log-cambio
;; NATURALEZA: Impura
;; ESTRATEGIA: Formateo de salida
;; IMPACTO: No destructiva
;;
;; Registra en consola los cambios de estado.
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
;;
;; Evalúa si la duración del ciclo se encuentra
;; dentro de los parámetros recomendados.
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
;;
;; Calcula la duración total de un ciclo completo.
;; ========================================================

(defun duracion-ciclo ()

  (+ (obtener-tiempo :rojo)
     (obtener-tiempo :verde)
     (* 2 (obtener-tiempo :intermitencia))))


;; ========================================================
;; FUNCIÓN: ciclos-por-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Cálculo aritmético
;; IMPACTO: No destructiva
;;
;; Calcula cuántos ciclos completos ocurren
;; en una cantidad determinada de minutos.
;; ========================================================

(defun ciclos-por-tiempo (minutos)

  (floor (/ (* minutos 60)
            (duracion-ciclo))))


;; ========================================================
;; FUNCIÓN: distribucion-temporal
;; NATURALEZA: Pura
;; ESTRATEGIA: Cálculo aritmético
;; IMPACTO: No destructiva
;;
;; Calcula el porcentaje de tiempo dedicado
;; a cada estado del sistema.
;; ========================================================

(defun distribucion-temporal ()

  (let* ((rojo (obtener-tiempo :rojo))
         (verde (obtener-tiempo :verde))
         (intermitencia (obtener-tiempo :intermitencia))
         (ciclo (duracion-ciclo)))

    (list

      (cons 'rojo
            (* 100.0 (/ rojo ciclo)))

      (cons 'verde
            (* 100.0 (/ verde ciclo)))

      (cons 'amarillo-intermitente
            (* 100.0 (/ (* 2 intermitencia)
                        ciclo))))))


;; ========================================================
;; FUNCIÓN: leer-configuracion
;; NATURALEZA: Impura
;; ESTRATEGIA: Lectura de archivo JSON
;; IMPACTO: No destructiva
;;
;; Carga la configuración desde config.json.
;; ========================================================

(defun leer-configuracion ()
  (with-open-file (stream "config_iteracion2.json"
                          :direction :input)
    (cl-json:decode-json stream)))


;; ========================================================
;; FUNCIÓN: obtener-tiempo
;; NATURALEZA: Pura
;; ESTRATEGIA: Búsqueda en lista asociativa
;; IMPACTO: No destructiva
;;
;; Obtiene el tiempo asociado a un estado.
;; ========================================================

(defun obtener-tiempo (color)

  (cdr
   (assoc color
          (leer-configuracion)
          :test #'eq)))



#| ITERACION 2/ EXTENSION 2|#

;; ========================================================
;; FUNCIÓN: informe
;; NATURALEZA: Impura
;; ESTRATEGIA: Escritura en archivo de texto plano
;; IMPACTO: Genera un archivo externo en el sistema
;;
;; DESCRIPCIÓN:
;; Esta función implementa un mecanismo de persistencia
;; para almacenar el historial de eventos del sistema
;; semafórico.
;;
;; Recibe una lista de registros y genera un archivo
;; llamado "informe-ejecucion-semaforo.txt" que contiene
;; el detalle de la ejecución del programa.
;;
;; Se utiliza WITH-OPEN-FILE para administrar de forma
;; segura la apertura y cierre del archivo. La iteración
;; sobre los eventos se realiza mediante DOLIST,
;; escribiendo cada registro en una línea independiente.
;;
;; Esta funcionalidad permite conservar evidencia de los
;; cambios de estado del semáforo aun después de finalizar
;; la ejecución del programa.
;;
;; EJEMPLO DE USO:
;;
;; (informe
;;  '("Tiempo 0: ROJO"
;;    "Tiempo 90: VERDE"
;;    "Tiempo 210: AMARILLO-INTERMITENTE"))
;;
;; ARCHIVO GENERADO:
;;
;; Informe de Ejecución del Sistema Semafórico
;; =========================================
;; Tiempo 0: ROJO
;; Tiempo 90: VERDE
;; Tiempo 210: AMARILLO-INTERMITENTE
;; --- Fin del Informe ---
;;
;; ========================================================

(defun informe (datos)

  (with-open-file
      (stream
       "informe-ejecucion-semaforo.txt"
       :direction :output
       :if-exists :supersede
       :if-does-not-exist :create)

    (format stream
            "Informe de Ejecución del Sistema Semafórico~%")

    (format stream
            "=========================================~%~%")

    (dolist (evento datos)
      (format stream "~A~%" evento))

    (format stream
            "~%--- Fin del Informe ---~%")))
