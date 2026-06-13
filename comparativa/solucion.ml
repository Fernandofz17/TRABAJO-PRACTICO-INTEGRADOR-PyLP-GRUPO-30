(* ======================================================
   FASE 3: ESTUDIO COMPARATIVO - OCAML
   SISTEMA DE SEMÁFOROS INTELIGENTES
   ====================================================== *)

(* ========
   1. Definición de Tipos Algebraicos (ADTs)
   En lugar de usar strings (textos libres), creamos tipos 
   estrictos. Esto evita errores de tipeo y aprovecha el 
   tipado fuerte de OCaml.
   ======== *)

type color =
  | Rojo
  | Amarillo
  | Verde

type accion = 
  | CambiarAVerde 
  | CambiarAAmarillo 
  | CambiarARojo 
  | AccionInvalida

(* ======================================================
   FUNCIÓN: transicion
   NATURALEZA: Pura 
   ESTRATEGIA: Pattern Matching (match ... with)
   IMPACTO: No destructiva
   ====================================================== *)

(* 
   Nota de Inferencia de Tipos:
   OCaml puede deducir los tipos solo, pero acá los declaramos explícitamente
   (color_actual: color) para que el código se documente a sí mismo.
*)
let transicion (color_actual: color) (cambiar_a: color) : color * accion =
  
  (* El 'match ... with' evalúa la combinación de colores (una tupla).
     Como es un lenguaje orientado a expresiones, este bloque no modifica 
     variables, sino que "se transforma" en el resultado final. *)
  match (color_actual, cambiar_a) with
  
  (* Si es Rojo y quiere pasar a Verde -> Retorna tupla (Verde, Acción) *)
  | (Rojo, Verde) -> (Verde, CambiarAVerde)
  
  (* Si es Verde y quiere pasar a Amarillo -> Retorna tupla (Amarillo, Acción) *)
  | (Verde, Amarillo) -> (Amarillo, CambiarAAmarillo)
  
  (* Si es Amarillo y quiere pasar a Rojo -> Retorna tupla (Rojo, Acción) *)
  | (Amarillo, Rojo) -> (Rojo, CambiarARojo)
  
  (* El guion bajo '_' es el comodín. Atrapa cualquier combinación inválida
     (ej: de Rojo a Amarillo) para que el programa no explote. *)
  | _ -> (color_actual, AccionInvalida)


(* ======================================================
   FUNCIÓN: timer
   NATURALEZA: Pura
   ESTRATEGIA: Condicionales (Orientado a Expresiones)
   IMPACTO: No destructiva
   ====================================================== *)

let timer (tiempo_unix: int) : color =
  (* Definimos el ciclo total sumando los tiempos: 90 + 120 + 6 = 216 *)
  let ciclo_total = 216 in
  
  (* El operador 'mod' nos da el resto de la división. 
     Esto crea un loop infinito matemático de 0 a 215. *)
  let instante = tiempo_unix mod ciclo_total in

  (* En OCaml el if/else es una expresión que devuelve un valor directo.
     Evaluamos en orden temporal lógico (Rojo -> Verde -> Amarillo) *)
     
  if instante < 90 then 
    Rojo      (* Los primeros 90 segundos son Rojo *)
    
  else if instante < 210 then 
    Verde     (* Del segundo 90 al 209 (120s) son Verde *)
    
  else 
    Amarillo  (* Los últimos 6 segundos (210 a 215) son Amarillo *)


(* ======================================================
   CASOS DE PRUEBA
   Al ejecutarlos en la consola interactiva, el compilador 
   mostrará los resultados evaluados automáticamente.
   ====================================================== *)

(* Pruebas de Transición *)
let prueba_1 = transicion Rojo Verde;;        (* Retorna: (Verde, CambiarAVerde) *)
let prueba_2 = transicion Verde Amarillo;;    (* Retorna: (Amarillo, CambiarAAmarillo) *)
let prueba_3 = transicion Amarillo Rojo;;     (* Retorna: (Rojo, CambiarARojo) *)
let prueba_mala = transicion Rojo Amarillo;;  (* Retorna: (Rojo, AccionInvalida) *)

(* Pruebas del Temporizador *)
let prueba_t1 = timer 0;;     (* Retorna: Rojo *)
let prueba_t2 = timer 100;;   (* Retorna: Verde *)
let prueba_t3 = timer 212;;   (* Retorna: Amarillo *)
let prueba_t4 = timer 216;;   (* Retorna: Rojo (se reinicia el ciclo) *)


