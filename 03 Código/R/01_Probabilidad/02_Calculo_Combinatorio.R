# ============================================================================
# Estadística I - CIDE
# Tema 1: Probabilidad
# 1.b) Cálculo combinatorio
# ============================================================================
#
# El cálculo combinatorio nos permite contar el número de resultados
# posibles de un experimento sin tener que enumerarlos uno por uno. Esto es
# clave para calcular probabilidades en espacios equiprobables (regla de
# Laplace, que veremos en el siguiente script).

# ----------------------------------------------------------------------------
# 1. Principio multiplicativo
# ----------------------------------------------------------------------------
# Si una tarea se hace en k etapas, con n_1, n_2, ..., n_k opciones en cada
# etapa, el número total de formas de completarla es el producto de las n_i.

# Ejemplo: un menú de comida corrida con 4 entradas, 3 platos fuertes y
# 2 postres. ¿Cuántos menús distintos se pueden armar?
n_entradas <- 4
n_platos   <- 3
n_postres  <- 2
n_entradas * n_platos * n_postres

# ----------------------------------------------------------------------------
# 2. Permutaciones: orden importa, sin repetición
# ----------------------------------------------------------------------------
# Número de formas de ordenar n objetos distintos: n!
factorial(5)

# Número de formas de ordenar r objetos elegidos de un total de n: P(n, r) = n!/(n-r)!
permutaciones <- function(n, r) factorial(n) / factorial(n - r)

# Ejemplo: de un grupo de 20 estudiantes, ¿de cuántas formas se puede elegir
# una terna ordenada (presidente, secretario, tesorero)?
permutaciones(20, 3)

# ----------------------------------------------------------------------------
# 3. Combinaciones: el orden no importa, sin repetición
# ----------------------------------------------------------------------------
# Número de formas de elegir r objetos de un total de n, sin importar el
# orden: C(n, r) = n! / (r! (n-r)!). En R: choose(n, r)

choose(20, 3)   # comité de 3 personas (sin roles) de un grupo de 20

# combn() además de contar, permite listar explícitamente las combinaciones
combn(x = c("Ana", "Beto", "Caro", "Dana"), m = 2)

# ----------------------------------------------------------------------------
# 4. Permutaciones con repetición
# ----------------------------------------------------------------------------
# Ejemplo: ¿cuántas "palabras" (secuencias) distintas se pueden formar con
# las letras de "ESTADISTICA"? Hay que dividir entre las repeticiones de
# cada letra.

letras <- strsplit("ESTADISTICA", "")[[1]]
tabla_letras <- table(letras)
tabla_letras

n_total <- length(letras)
factorial(n_total) / prod(factorial(tabla_letras))

# ----------------------------------------------------------------------------
# 5. Un caso con contexto económico: carteras de inversión
# ----------------------------------------------------------------------------
# Un analista tiene 10 acciones candidatas y quiere armar una cartera con
# 4 de ellas. ¿Cuántas carteras distintas puede formar si el orden no
# importa? ¿Y si además debe decidir cuál es la posición "ancla" (orden sí
# importa para esa posición)?

carteras_sin_orden <- choose(10, 4)
carteras_sin_orden

carteras_con_ancla <- choose(9, 3) * 10  # elige el ancla (10 opciones) y
                                          # el resto sin orden entre las 9 restantes
carteras_con_ancla

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Una contraseña tiene 4 dígitos (0-9) y se permite repetir dígitos.
#    ¿Cuántas contraseñas distintas existen? ¿Y si no se permite repetir?
# 2. De un grupo de 12 personas se debe formar un equipo de 5 para un
#    proyecto. ¿De cuántas formas se puede hacer? ¿Y si una de esas 5
#    personas debe ser designada como líder?
# 3. Usa combn() para listar (no solo contar) todas las combinaciones de
#    3 elementos del conjunto {1, 2, 3, 4, 5}.
