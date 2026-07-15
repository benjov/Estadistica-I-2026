# ============================================================================
# Estadística I - CIDE
# Tema 1: Probabilidad
# 1.f) Teorema de Bayes
# ============================================================================
#
# El teorema de Bayes nos permite "invertir" una probabilidad condicional:
# a partir de P(B | A) obtenemos P(A | B).
#
#     P(A_i | B) = [P(B | A_i) * P(A_i)] / sum_j [P(B | A_j) * P(A_j)]
#
# donde {A_1, ..., A_k} es una partición del espacio muestral. El denominador
# es exactamente la ley de probabilidad total que vimos en el script anterior.

# ----------------------------------------------------------------------------
# 1. Ejemplo clásico: una prueba de diagnóstico económico (falso positivo)
# ----------------------------------------------------------------------------
# Un modelo de score crediticio "marca" como riesgo de impago a un cliente.
# Se sabe que:
#   - 5% de los clientes realmente incumplirán su crédito (prevalencia)
#   - Si un cliente va a incumplir, el modelo lo detecta con probabilidad 0.9
#     (sensibilidad)
#   - Si un cliente NO va a incumplir, el modelo lo marca por error con
#     probabilidad 0.10 (falsos positivos)
#
# Pregunta: si el modelo marca a un cliente como riesgo, ¿cuál es la
# probabilidad de que realmente vaya a incumplir?

P_incumple      <- 0.05
P_no_incumple   <- 1 - P_incumple
P_marca_dado_incumple    <- 0.90   # sensibilidad
P_marca_dado_no_incumple <- 0.10   # tasa de falsos positivos

# Ley de probabilidad total: P(marca)
P_marca <- P_marca_dado_incumple * P_incumple +
  P_marca_dado_no_incumple * P_no_incumple
P_marca

# Bayes: P(incumple | marca)
P_incumple_dado_marca <- (P_marca_dado_incumple * P_incumple) / P_marca
P_incumple_dado_marca

# El resultado suele sorprender: aunque el modelo parece "bueno" (90% de
# sensibilidad), la probabilidad de incumplimiento real dado que el modelo
# marcó al cliente es mucho menor a 90%, porque el evento "incumple" es raro
# (prevalencia baja) frente a la tasa de falsos positivos.

# ----------------------------------------------------------------------------
# 2. Función genérica para Bayes con una partición de más de 2 elementos
# ----------------------------------------------------------------------------

bayes <- function(P_A, P_B_dado_A) {
  # P_A: vector con P(A_1), ..., P(A_k)  (debe sumar 1)
  # P_B_dado_A: vector con P(B | A_1), ..., P(B | A_k)
  stopifnot(abs(sum(P_A) - 1) < 1e-8)
  P_B <- sum(P_B_dado_A * P_A)
  (P_B_dado_A * P_A) / P_B
}

# Ejemplo: una empresa produce en 3 plantas con distinta participación y
# distinta tasa de defectos.
P_planta       <- c(PlantaA = 0.5, PlantaB = 0.3, PlantaC = 0.2)
P_defecto_dado_planta <- c(PlantaA = 0.02, PlantaB = 0.05, PlantaC = 0.08)

posterior <- bayes(P_planta, P_defecto_dado_planta)
round(posterior, 4)
# Interpretación: dado que se encontró un producto defectuoso, ¿de qué
# planta es más probable que provenga?

# ----------------------------------------------------------------------------
# 3. Verificación por simulación
# ----------------------------------------------------------------------------

set.seed(2026)
n_sim <- 500000
planta <- sample(names(P_planta), size = n_sim, replace = TRUE, prob = P_planta)
defecto <- rbinom(n_sim, size = 1, prob = P_defecto_dado_planta[planta])

# P(Planta = A | Defecto = 1), estimada por simulación
mean(planta[defecto == 1] == "PlantaA")
posterior["PlantaA"]

# ----------------------------------------------------------------------------
# 4. Actualización secuencial (Bayes con más de una pieza de evidencia)
# ----------------------------------------------------------------------------
# El posterior de hoy puede usarse como el prior de mañana si llega nueva
# evidencia. Esta idea es la base del "aprendizaje bayesiano" y se retoma en
# cursos posteriores de inferencia.

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. En el ejemplo del score crediticio, ¿cómo cambia P(incumple | marca) si
#    la prevalencia de incumplimiento sube a 20%? Recalcula y comenta.
# 2. Usa la función bayes() para un caso con 4 categorías definidas por ti
#    (por ejemplo, 4 proveedores con distintas tasas de retraso en entregas).
# 3. En el ejemplo de las plantas, calcula P(PlantaB | Defecto = 1) y
#    P(PlantaC | Defecto = 1) y verifica que los tres posteriores suman 1.
