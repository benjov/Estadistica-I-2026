# ============================================================================
# Estadística I - CIDE
# Tema 1: Probabilidad
# 1.d) Independencia de eventos
# ============================================================================
#
# Dos eventos A y B son independientes si la ocurrencia de uno no cambia la
# probabilidad del otro. Formalmente: P(A ∩ B) = P(A) * P(B).
#
# Nota: independencia es distinto de "mutuamente excluyentes". De hecho, si
# P(A) > 0 y P(B) > 0, dos eventos disjuntos NUNCA son independientes
# (si ocurre A, sabemos con certeza que B no ocurrió).

# ----------------------------------------------------------------------------
# 1. Ejemplo clásico: dos monedas independientes
# ----------------------------------------------------------------------------

omega <- expand.grid(moneda1 = c("A", "S"), moneda2 = c("A", "S"))
omega

A <- omega$moneda1 == "A"          # "la primera moneda cae águila"
B <- omega$moneda2 == "A"          # "la segunda moneda cae águila"

P <- function(evento) mean(evento) # regla de Laplace en un espacio equiprobable

P(A); P(B); P(A & B)
P(A) * P(B) == P(A & B)  # se cumple: son independientes

# ----------------------------------------------------------------------------
# 2. Un caso donde NO hay independencia: extracción sin reemplazo
# ----------------------------------------------------------------------------
# Urna con 5 bolas rojas y 3 azules. Se extraen dos bolas SIN reemplazo.
# A: "la primera bola es roja"; B: "la segunda bola es roja"

# P(A) = 5/8
P_A <- 5 / 8

# P(B | A) = 4/7 (si ya salió una roja, quedan 4 rojas de 7 en total)
P_B_dado_A <- 4 / 7

# P(A ∩ B) = P(A) * P(B|A)  (regla del producto)
P_A_y_B <- P_A * P_B_dado_A
P_A_y_B

# P(B) marginal (por simetría en extracciones sin reemplazo, P(B) = P(A))
P_B <- 5 / 8

P_A * P_B      # si fueran independientes, la intersección sería este valor
P_A_y_B        # pero la intersección real es distinta -> NO son independientes

# ----------------------------------------------------------------------------
# 3. Verificación por simulación
# ----------------------------------------------------------------------------

set.seed(2026)
urna <- c(rep("Roja", 5), rep("Azul", 3))

simular_extraccion <- function() {
  extraccion <- sample(urna, size = 2, replace = FALSE)
  c(primera_roja = extraccion[1] == "Roja",
    segunda_roja = extraccion[2] == "Roja")
}

n_sim <- 100000
resultados <- t(replicate(n_sim, simular_extraccion()))

mean(resultados[, "primera_roja"])                                   # ~ P(A)
mean(resultados[, "segunda_roja"])                                   # ~ P(B)
mean(resultados[, "primera_roja"] & resultados[, "segunda_roja"])    # ~ P(A ∩ B)
mean(resultados[, "primera_roja"]) * mean(resultados[, "segunda_roja"])  # P(A)*P(B), no coincide

# ----------------------------------------------------------------------------
# 4. Independencia de varios eventos
# ----------------------------------------------------------------------------
# A, B y C son mutuamente independientes si se cumple la condición de a pares
# Y ADEMÁS P(A ∩ B ∩ C) = P(A) * P(B) * P(C). La independencia de a pares NO
# implica independencia conjunta (es un error común).

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Repite la simulación de la urna pero CON reemplazo. Verifica que ahora
#    sí se cumple P(A ∩ B) = P(A) * P(B).
# 2. Dos eventos tienen P(A) = 0.3, P(B) = 0.4 y P(A ∩ B) = 0.12.
#    ¿Son independientes? Justifica con la definición.
# 3. Construye un ejemplo (con un dado y dos eventos definidos por ti) en el
#    que A y B sean independientes pero P(A) != P(B).
