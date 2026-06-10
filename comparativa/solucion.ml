# (*

FUNCIÓN: transicion
NATURALEZA: Pura
ESTRATEGIA: Pattern Matching (match ... with)
IMPACTO: No destructiva

Descripción:
Recibe el estado actual del semáforo y el estado al que se
desea realizar la transición. Mediante pattern matching se
evalúan las combinaciones posibles de estados y se devuelve
la acción correspondiente. Si la transición no es válida,
se retorna una acción por defecto.

La función es pura porque siempre produce el mismo resultado
para los mismos argumentos y no modifica ningún estado
externo.
========

*)

type color =
| Rojo
| Amarillo
| Verde

let transicion color_actual cambiar_a =
match (color_actual, cambiar_a) with
| (Rojo, Verde) -> (Rojo, "cambiar-a-verde")
| (Verde, Amarillo) -> (Verde, "cambiar-a-amarillo")
| (Amarillo, Rojo) -> (Amarillo, "cambiar-a-rojo")
| _ -> (color_actual, "accion-por-defecto")

# (*

FUNCIÓN: timer
NATURALEZA: Pura
ESTRATEGIA: Cálculo matemático + condicionales
IMPACTO: No destructiva

Descripción:
Determina el estado del semáforo para un instante de tiempo
determinado. Utiliza la operación módulo para calcular la
posición dentro del ciclo completo del semáforo y, en función
del resultado, identifica si corresponde el estado Rojo,
Amarillo o Verde.

La función es pura porque únicamente calcula un resultado a
partir de su entrada sin producir efectos secundarios.
======================================================

*)

let timer tiempo_unix =
let instante = tiempo_unix mod 216 in
if instante < 90 then
Rojo
else if instante < 96 then
Amarillo
else
Verde

# (*

# CASOS DE PRUEBA

PRUEBA 1: Transición Rojo -> Verde

transicion Rojo Verde;;

Resultado esperado:
(Rojo, "cambiar-a-verde")

---

PRUEBA 2: Transición Verde -> Amarillo

transicion Verde Amarillo;;

Resultado esperado:
(Verde, "cambiar-a-amarillo")

---

PRUEBA 3: Transición Amarillo -> Rojo

transicion Amarillo Rojo;;

Resultado esperado:
(Amarillo, "cambiar-a-rojo")

---

PRUEBA 4: Inicio del ciclo

timer 0;;

Resultado esperado:
Rojo

---

PRUEBA 5: Inicio del estado Amarillo

timer 90;;

Resultado esperado:
Amarillo

---

PRUEBA 6: Inicio del estado Verde

timer 96;;

Resultado esperado:
Verde

---

PRUEBA 7: Reinicio del ciclo

timer 216;;

Resultado esperado:
Rojo

========================================================
FIN DE LOS CASOS DE PRUEBA
==========================

*)
