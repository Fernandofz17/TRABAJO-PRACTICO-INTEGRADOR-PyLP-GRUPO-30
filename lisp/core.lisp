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
;; DESCRIPCIÓN DE LA FUNCIÓN TRANSICION
;;
;; Esta función modela las transiciones permitidas entre
;; los distintos estados del semáforo. Recibe como entrada
;; el estado actual y el color de destino solicitado.
;;
;; Mediante la estructura condicional COND verifica si la
;; transición respeta la secuencia definida por el sistema:
;;
;;   EN-ROJO      -> VERDE
;;   EN-VERDE     -> AMARILLO
;;   EN-AMARILLO  -> ROJO
;;
;; Cuando la transición es válida, retorna una lista
;; compuesta por el estado actual y una acción descriptiva
;; indicando el cambio a realizar.
;;
;; Si la combinación de estados no corresponde a una
;; transición permitida, la función devuelve el estado
;; actual junto con el símbolo ACCION-POR-DEFECTO.
;;
;; Ejemplos:
;;
;; (transicion 'en-rojo 'verde)
;; => (EN-ROJO "cambiar-a-verde")
;;
;; (transicion 'en-verde 'rojo)
;; => (EN-VERDE ACCION-POR-DEFECTO)
;;
;; La función es considerada pura porque no modifica
;; variables globales, no realiza operaciones de entrada/
;; salida y siempre produce el mismo resultado para los
;; mismos parámetros de entrada.
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
;; DESCRIPCIÓN:
;; Registra en la consola cada cambio de estado del semáforo.
;; El parámetro TIEMPO representa un timestamp Unix (Epoch Time),
;; es decir, la cantidad de segundos transcurridos desde
;; el 1 de enero de 1970 a las 00:00:00 UTC.
;; Su objetivo es permitir la reconstrucción histórica
;; de los estados del semáforo para tareas de auditoría
;; y análisis forense del tráfico.

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
;; FUNCIÓN: recomendacion-ciclo
;; NATURALEZA: Pura
;; ESTRATEGIA: Predicado (COND)
;; IMPACTO: No destructiva
;;
;; DESCRIPCIÓN:
;; Evalúa la duración total de un ciclo semafórico y
;; genera una recomendación basada en criterios de
;; ingeniería de tránsito.
;;
;; Según la consigna, los ciclos menores a 35 segundos
;; o mayores a 150 segundos suelen resultar incómodos
;; para los conductores y pueden afectar la percepción
;; del flujo vehicular.
;;
;; La función recibe como parámetro la duración total
;; del ciclo y clasifica el resultado en tres categorías:
;;
;; - Menor a 35 segundos:
;;   "Ciclo demasiado corto"
;;
;; - Entre 35 y 150 segundos:
;;   "Ciclo dentro del rango recomendado"
;;
;; - Mayor a 150 segundos:
;;   "Ciclo demasiado largo"
;;
;; Esta función no modifica datos ni depende de estados
;; externos, por lo que siempre produce el mismo
;; resultado para la misma duración recibida.
;;
;; Ejemplos:
;; (recomendacion-ciclo 20)
;; => "Ciclo demasiado corto"
;;
;; (recomendacion-ciclo 120)
;; => "Ciclo dentro del rango recomendado"
;;
;; (recomendacion-ciclo 216)
;; => "Ciclo demasiado largo"
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
;; DESCRIPCIÓN:
;; Calcula la duración total de un ciclo completo del
;; semáforo sumando los tiempos configurados para los
;; estados rojo, amarillo y verde.
;;
;; Los valores son obtenidos dinámicamente desde el
;; archivo de configuración externo (config.json)
;; mediante la función OBTENER-TIEMPO.
;;
;; Gracias a este diseño, cualquier modificación en la
;; duración de los estados se refleja automáticamente
;; sin necesidad de alterar el código fuente.
;;
;; Ejemplo con la configuración:
;; {
;;   "rojo": 90,
;;   "amarillo": 6,
;;   "verde": 120
;; }
;;
;; Duración total:
;; 90 + 6 + 120 = 216 segundos
;;
;; Esta función es utilizada por otras funciones del
;; sistema, como TIMER-SEMÁFORO, CICLOS-POR-TIEMPO y
;; DISTRIBUCION-TEMPORAL, favoreciendo la composición
;; funcional y evitando la duplicación de cálculos, extender innecesesariamente el código.
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
;;
;; DESCRIPCIÓN:
;; Calcula la cantidad de ciclos completos que realiza
;; el semáforo dentro de un intervalo de tiempo expresado
;; en minutos.
;;
;; La función convierte los minutos recibidos a segundos
;; y divide dicho valor por la duración total de un ciclo,
;; obtenida mediante la función DURACION-CICLO.
;;
;; Se utiliza FLOOR para conservar únicamente los ciclos
;; completos, descartando cualquier fracción restante.
;;
;; Esta información puede utilizarse para tareas de
;; planificación, simulación de tráfico, análisis de
;; capacidad de la vía y estimación de mantenimiento.
;;
;; Ejemplo con un ciclo de 216 segundos:
;;
;; (ciclos-por-tiempo 15)
;; 15 minutos = 900 segundos
;; 900 / 216 = 4.16
;; Resultado: 4 ciclos completos
;;
;; (ciclos-por-tiempo 60)
;; 3600 / 216 = 16.66
;; Resultado: 16 ciclos completos
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
;; DESCRIPCIÓN:
;;
;; La función distribucion-temporal calcula el porcentaje
;; de tiempo que cada color del semáforo permanece activo
;; dentro de un ciclo completo.
;;
;; Para ello obtiene las duraciones configuradas para los
;; estados rojo, amarillo y verde desde el archivo JSON,
;; calcula la duración total del ciclo mediante la función
;; duracion-ciclo y luego aplica la fórmula:
;;
;;   porcentaje = (tiempo-color / tiempo-total) * 100
;;
;; El resultado se devuelve como una lista asociativa
;; (alist), donde cada color queda vinculado a su
;; porcentaje de participación dentro del ciclo.
;;
;; Ejemplo con la configuración:
;; rojo = 90 s
;; amarillo = 6 s
;; verde = 120 s
;;
;; ciclo total = 216 s
;;
;; (distribucion-temporal)
;; => ((ROJO . 41.67)
;;     (AMARILLO . 2.78)
;;     (VERDE . 55.56))
;;
;; Esta información resulta útil para tareas de análisis,
;; planificación y optimización del flujo vehicular,
;; permitiendo conocer qué proporción del tiempo total
;; permanece activa cada señal del semáforo.
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
;; DESCRIPCIÓN:
;;
;; La función leer-configuracion es la encargada de cargar
;; los parámetros externos del sistema desde el archivo
;; "config.json".
;;
;; Utiliza la macro WITH-OPEN-FILE para abrir el archivo
;; en modo lectura y garantizar que el recurso sea cerrado
;; automáticamente al finalizar la operación, incluso si
;; ocurre algún error durante el proceso.
;;
;; Una vez abierto el archivo, la biblioteca CL-JSON
;; interpreta el contenido del documento JSON y lo
;; convierte a una estructura de datos nativa de
;; Common Lisp, permitiendo que otras funciones del
;; sistema accedan a los tiempos configurados para cada
;; estado del semáforo.
;;
;; Ejemplo de archivo:
;;
;; {
;;   "rojo": 90,
;;   "amarillo": 6,
;;   "verde": 120
;; }
;;
;; La función devuelve una estructura asociativa con los
;; pares clave-valor obtenidos del archivo.
;;
;; Esta función representa el punto de conexión entre la
;; configuración externa y la lógica interna del sistema,
;; permitiendo modificar los tiempos del semáforo sin
;; alterar el código fuente.
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
;; DESCRIPCIÓN:
;;
;; La función obtener-tiempo recupera la duración asociada
;; a un color específico del semáforo a partir de la
;; configuración almacenada en el archivo JSON.
;;
;; Para ello, invoca primero a leer-configuracion, que
;; carga los datos externos, y luego utiliza ASSOC para
;; buscar la clave correspondiente al color solicitado.
;;
;; Una vez encontrada la asociación, CDR extrae y retorna
;; únicamente el valor numérico asociado a dicha clave.
;;
;; Ejemplos:
;;
;; (obtener-tiempo :rojo)      ; => 90
;; (obtener-tiempo :amarillo)  ; => 6
;; (obtener-tiempo :verde)     ; => 120
;;
;; Esta función actúa como una capa de abstracción entre
;; la lógica del sistema y el formato de almacenamiento,
;; evitando que el resto de las funciones conozcan cómo
;; se encuentran organizados los datos dentro del archivo
;; de configuración.
;;
;; Aunque la operación de búsqueda es determinística, el
;; resultado depende del contenido actual del archivo
;; externo, por lo que su comportamiento está vinculado a
;; una fuente de datos externa.
;; ========================================================

(defun obtener-tiempo (color)
  (cdr (assoc color
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


