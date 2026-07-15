# ============================================================================
# Estadística I - CIDE
# Tema 3: Esperanza
# 3.a) Esperanza de variables aleatorias  |  3.b) Propiedades de la esperanza
# ============================================================================
#
# La esperanza (valor esperado) de una VA es un promedio ponderado por
# probabilidades: mide el "centro de masa" de su distribución.
#   Discreta:  E[X] = sum_x  x * P(X = x)
#   Continua:  E[X] = integral x * f(x) dx

# ----------------------------------------------------------------------------
# 1. Esperanza de una VA discreta, calculada "a mano" en R
# ----------------------------------------------------------------------------
# X = número de siniestros que reporta un asegurado en un año

x_vals <- 0:4
fmp    <- c(0.55, 0.25, 0.12, 0.06, 0.02)
sum(fmp)  # verificación: debe ser 1

E_X <- sum(x_vals * fmp)
E_X

# Interpretación: si la aseguradora tuviera un número muy grande de
# asegurados con esta misma distribución, el promedio de siniestros por
# persona se acercaría a E[X].

# Verificación por simulación (Ley de los Grandes Números, tema 4)
set.seed(2026)
muestra <- sample(x_vals, size = 200000, replace = TRUE, prob = fmp)
mean(muestra)
E_X

# ----------------------------------------------------------------------------
# 2. Esperanza de una función de X: E[g(X)]
# ----------------------------------------------------------------------------
# No es necesario obtener primero la distribución de Y = g(X); basta con
# E[g(X)] = sum_x g(x) * P(X = x)  (teorema del estadístico inconsciente)

# Ejemplo: el costo para la aseguradora es g(X) = 500 + 300*X^2 (costos fijos
# más costos que crecen más que proporcionalmente con el número de siniestros)
g <- function(x) 500 + 300 * x^2
E_gX <- sum(g(x_vals) * fmp)
E_gX

mean(g(muestra))  # comparación por simulación

# ----------------------------------------------------------------------------
# 3. Esperanza de VA continuas (integración numérica con integrate())
# ----------------------------------------------------------------------------
# X ~ Exponencial(rate = 0.25): tiempo (en años) hasta la siguiente
# reclamación grande.

integrando <- function(x) x * dexp(x, rate = 0.25)
integrate(integrando, lower = 0, upper = Inf)$value
1 / 0.25  # la Exponencial(rate) tiene E[X] = 1/rate

# ----------------------------------------------------------------------------
# 4. Propiedades de la esperanza (linealidad)
# ----------------------------------------------------------------------------
# E[aX + b] = a*E[X] + b   para constantes a, b
# E[X + Y] = E[X] + E[Y]   SIEMPRE, incluso si X y Y no son independientes

a_const <- 3; b_const <- 10
E_X * a_const + b_const

set.seed(2026)
X <- rnorm(200000, mean = 12, sd = 3)
mean(a_const * X + b_const)     # simulado
a_const * mean(X) + b_const     # vía linealidad

# Suma de dos VA (no necesariamente independientes)
Y <- 0.5 * X + rnorm(200000, sd = 1)  # Y depende de X
mean(X + Y)
mean(X) + mean(Y)  # coinciden, aunque X y Y estén correlacionadas

# ----------------------------------------------------------------------------
# 5. Un contraejemplo importante: E[g(X)] != g(E[X]) en general
# ----------------------------------------------------------------------------
# Esto solo se cumple si g es lineal. Para g convexa (como el cuadrado),
# la desigualdad de Jensen nos dice que E[g(X)] >= g(E[X]).

mean(X^2)
mean(X)^2
# mean(X^2) > mean(X)^2 siempre que X tenga varianza positiva (tema 3.c)

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Una lotería paga 0 con probabilidad 0.9, $100 con probabilidad 0.08 y
#    $1000 con probabilidad 0.02. Calcula la ganancia esperada de comprar
#    un boleto que cuesta $15 (define g(premio) = premio - 15).
# 2. Verifica con integrate() que si X ~ Uniforme(a, b), entonces
#    E[X] = (a+b)/2, usando a = 2 y b = 8.
# 3. Simula X ~ Poisson(lambda = 5) y verifica numéricamente que
#    E[3X - 7] coincide con 3*E[X] - 7.
