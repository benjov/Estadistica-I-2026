# ============================================================================
# Estadística I - CIDE
# Tema 1: Probabilidad
# 1.e) Probabilidad condicional
# ============================================================================
#
# La probabilidad condicional de A dado B se define como:
#     P(A | B) = P(A ∩ B) / P(B),   si P(B) > 0
# Es la probabilidad de A una vez que sabemos que B ya ocurrió: "actualiza"
# nuestra incertidumbre sobre A a la luz de nueva información.

# ----------------------------------------------------------------------------
# 1. Ejemplo con una tabla de contingencia (contexto económico)
# ----------------------------------------------------------------------------
# Encuesta a 500 personas sobre nivel educativo y situación laboral.

tabla <- matrix(c(60, 40,     # Sin licenciatura: Desempleado, Empleado
                   30, 370),  # Con licenciatura: Desempleado, Empleado
                nrow = 2, byrow = TRUE,
                dimnames = list(Educacion = c("Sin_licenciatura", "Con_licenciatura"),
                                 Empleo = c("Desempleado", "Empleado")))
tabla

n_total <- sum(tabla)
tabla_prob <- tabla / n_total   # tabla de probabilidades conjuntas
round(tabla_prob, 3)

# Probabilidades marginales (suman por fila o por columna)
P_educacion <- rowSums(tabla_prob)
P_empleo    <- colSums(tabla_prob)
P_educacion
P_empleo

# P(Desempleado | Sin licenciatura)
P_desempleo_dado_sin_lic <- tabla_prob["Sin_licenciatura", "Desempleado"] / P_educacion["Sin_licenciatura"]
P_desempleo_dado_sin_lic

# P(Desempleado | Con licenciatura)
P_desempleo_dado_con_lic <- tabla_prob["Con_licenciatura", "Desempleado"] / P_educacion["Con_licenciatura"]
P_desempleo_dado_con_lic

# Comparación: condicionar en educación sí cambia la probabilidad de
# desempleo, así que educación y empleo NO son independientes en esta muestra.
P_empleo["Desempleado"]  # probabilidad marginal (sin condicionar)

# ----------------------------------------------------------------------------
# 2. Regla del producto (útil para eventos secuenciales)
# ----------------------------------------------------------------------------
# P(A ∩ B) = P(A) * P(B | A) = P(B) * P(A | B)
# Se generaliza a más eventos: P(A ∩ B ∩ C) = P(A) P(B|A) P(C|A ∩ B)

# Ejemplo: urna con 5 rojas y 3 azules, extracción sin reemplazo de 3 bolas.
# P(roja, roja, azul) en ese orden:
P_1 <- 5 / 8
P_2_dado_1 <- 4 / 7
P_3_dado_12 <- 3 / 6
P_secuencia <- P_1 * P_2_dado_1 * P_3_dado_12
P_secuencia

# Verificación por simulación
set.seed(2026)
urna <- c(rep("Roja", 5), rep("Azul", 3))
n_sim <- 200000
exitos <- replicate(n_sim, {
  extraccion <- sample(urna, size = 3, replace = FALSE)
  identical(extraccion, c("Roja", "Roja", "Azul"))
})
mean(exitos)
P_secuencia

# ----------------------------------------------------------------------------
# 3. Ley de probabilidad total (partición del espacio muestral)
# ----------------------------------------------------------------------------
# Si B1, B2, ..., Bk forman una partición de Omega:
#     P(A) = sum_i P(A | Bi) * P(Bi)
# Usamos la misma tabla de educación-empleo para verificarlo:

P_desempleo_total <- P_desempleo_dado_sin_lic * P_educacion["Sin_licenciatura"] +
  P_desempleo_dado_con_lic * P_educacion["Con_licenciatura"]
P_desempleo_total
P_empleo["Desempleado"]  # debe coincidir

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Con la tabla educación-empleo, calcula P(Con licenciatura | Empleado).
# 2. Construye tu propia tabla 2x2 con datos hipotéticos de "usa transporte
#    público" y "vive en zona metropolitana", y calcula las probabilidades
#    condicionales en ambas direcciones.
# 3. Simula el experimento de extraer 3 bolas SIN reemplazo y estima
#    P(al menos una azul en las 3 extracciones).
