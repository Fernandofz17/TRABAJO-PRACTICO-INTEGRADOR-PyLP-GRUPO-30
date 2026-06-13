

(* ========
Definición de Tipos Algebraicos (ADTs)
========
*)
type color =
  | Rojo
  | Amarillo
  | Verde

type accion = 
  | CambiarAVerde 
  | CambiarAAmarillo 
  | CambiarARojo 
  | AccionInvalida

(*
======================================================
FUNCIÓN: transicion
NATURALEZA: Pura 
ESTRATEGIA: Pattern Matching (match ... with)
IMPACTO: No destructiva

Descripción:
Recibe el estado actual y el estado deseado. Mediante pattern matching
evalúa si la transición es legal. 
Retorna una tupla: (Nuevo_Estado, Accion_Realizada).
======================================================
*)
let transicion (color_actual: color) (cambiar_a: color) : color * accion =
  match (color_actual, cambiar_a) with
  | (Rojo, Verde) -> (Verde, CambiarAVerde)
  | (Verde, Amarillo) -> (Amarillo, CambiarAAmarillo)
  | (Amarillo, Rojo) -> (Rojo, CambiarARojo)
  | _ -> (color_actual, AccionInvalida)


(*
======================================================
FUNCIÓN: timer
NATURALEZA: Pura
ESTRATEGIA: Condicionales (Orientado a Expresiones)
IMPACTO: No destructiva

Descripción:
Calcula el estado del semáforo según el timestamp.
======================================================
*)
let timer (tiempo_unix: int) : color =
  let ciclo_total = 216 in
  let instante = tiempo_unix mod ciclo_total in

  (* 0 a 89: 90 segundos de Rojo *)
  if instante < 90 then 
    Rojo
  (* 90 a 209: 120 segundos de Verde *)
  else if instante < 210 then 
    Verde
  (* 210 a 215: 6 segundos de Amarillo *)
  else 
    Amarillo 


(*
======================================================
CASOS DE PRUEBA (Actualizados para la consola)
======================================================
*)
let prueba_1 = transicion Rojo Verde;;        (* (Verde, CambiarAVerde) *)
let prueba_2 = transicion Verde Amarillo;;    (* (Amarillo, CambiarAAmarillo) *)
let prueba_3 = transicion Amarillo Rojo;;     (* (Rojo, CambiarARojo) *)
let prueba_mala = transicion Rojo Amarillo;;  (* (Rojo, AccionInvalida) *)

let prueba_t1 = timer 0;;   (* Rojo *)
let prueba_t2 = timer 100;; (* Verde *)
let prueba_t3 = timer 212;; (* Amarillo *)

