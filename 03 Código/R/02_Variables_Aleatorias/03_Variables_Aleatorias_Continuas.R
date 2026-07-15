# ============================================================================
# Estadística I - CIDE
# Tema 2: Variables Aleatorias
# 2.c) Algunas funciones de variables aleatorias continuas
# ============================================================================
#
# En una VA continua, la probabilidad se acumula mediante una función de
# densidad f(x) tal que P(a <= X <= b) = integral de f entre a y b, y
# P(X = x) = 0 para cualquier valor puntual x.

# ----------------------------------------------------------------------------
# 1. Uniforme continua: todos los valores en [a, b] son "igual de probables"
# ----------------------------------------------------------------------------
# Ejemplo: el tiempo de espera (en minutos) para el próximo camión es
# Uniforme(0, 15).

a <- 0; b <- 15
curve(dunif(x, a, b), from = -2, to = 17, lwd = 2, col = "steelblue",
      main = "Densidad Uniforme(0, 15)", ylab = "f(x)", xlab = "x")

punif(10, a, b) - punif(5, a, b)   # P(5 <= X <= 10)

# ----------------------------------------------------------------------------
# 2. Exponencial: tiempo de espera hasta un evento (tasa constante)
# ----------------------------------------------------------------------------
# Ejemplo: el tiempo (en años) hasta que una empresa entra en impago sigue
# una Exponencial(rate = 0.2), es decir, con vida media de 1/0.2 = 5 años.

tasa <- 0.2
curve(dexp(x, rate = tasa), from = 0, to = 30, lwd = 2, col = "firebrick",
      main = "Densidad Exponencial(rate = 0.2)", ylab = "f(x)", xlab = "x (años)")

pexp(5, rate = tasa)              # P(X <= 5)
1 - pexp(10, rate = tasa)         # P(X > 10)

# Propiedad de "falta de memoria": P(X > 15 | X > 10) = P(X > 5)
(1 - pexp(15, tasa)) / (1 - pexp(10, tasa))
1 - pexp(5, tasa)

# ----------------------------------------------------------------------------
# 3. Normal: la distribución continua más importante en estadística
# ----------------------------------------------------------------------------
# Ejemplo: el ingreso mensual (en miles de pesos) de cierto segmento de
# población sigue aproximadamente una Normal(mu = 12, sigma = 3).

mu <- 12; sigma <- 3
curve(dnorm(x, mu, sigma), from = mu - 4 * sigma, to = mu + 4 * sigma,
      lwd = 2, col = "darkgreen",
      main = "Densidad Normal(mu = 12, sigma = 3)", ylab = "f(x)", xlab = "x")
abline(v = mu, lty = 2)

pnorm(15, mu, sigma) - pnorm(9, mu, sigma)   # P(9 <= X <= 15)
qnorm(0.95, mu, sigma)                        # percentil 95

# Estandarización: Z = (X - mu) / sigma ~ Normal(0, 1)
z_15 <- (15 - mu) / sigma
pnorm(z_15) - pnorm((9 - mu) / sigma)  # debe coincidir con el cálculo anterior

# La regla empírica 68-95-99.7
pnorm(mu + sigma, mu, sigma) - pnorm(mu - sigma, mu, sigma)      # ~68%
pnorm(mu + 2 * sigma, mu, sigma) - pnorm(mu - 2 * sigma, mu, sigma)  # ~95%
pnorm(mu + 3 * sigma, mu, sigma) - pnorm(mu - 3 * sigma, mu, sigma)  # ~99.7%

# ----------------------------------------------------------------------------
# 4. Otras familias continuas útiles: Gamma, Beta y Ji-cuadrada
# ----------------------------------------------------------------------------
# Gamma(shape, rate): generaliza a la Exponencial; útil para modelar montos
curve(dgamma(x, shape = 3, rate = 0.5), from = 0, to = 25, lwd = 2,
      col = "purple", main = "Densidad Gamma(shape=3, rate=0.5)",
      xlab = "x", ylab = "f(x)")

# Beta(alpha, beta): útil para modelar proporciones (soporte en [0, 1])
curve(dbeta(x, shape1 = 2, shape2 = 5), from = 0, to = 1, lwd = 2,
      col = "orange", main = "Densidad Beta(2, 5)", xlab = "x", ylab = "f(x)")

# Ji-cuadrada: aparece de forma natural al sumar normales estándar al
# cuadrado; se retoma en inferencia (Estadística II)
curve(dchisq(x, df = 4), from = 0, to = 20, lwd = 2, col = "brown",
      main = "Densidad Ji-cuadrada(df = 4)", xlab = "x", ylab = "f(x)")

# ----------------------------------------------------------------------------
# 5. De la densidad a la simulación
# ----------------------------------------------------------------------------

set.seed(2026)
muestra_normal <- rnorm(10000, mean = mu, sd = sigma)
hist(muestra_normal, breaks = 40, freq = FALSE, col = "lightgray",
     main = "Histograma simulado vs. densidad teórica",
     xlab = "x")
curve(dnorm(x, mu, sigma), add = TRUE, lwd = 2, col = "darkgreen")

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. El tiempo de vida (en años) de una máquina sigue una Exponencial con
#    media 8 años. Calcula la probabilidad de que dure más de 10 años.
# 2. Si el gasto mensual en transporte de los hogares de una ciudad sigue
#    una Normal(mu = 850, sigma = 120) pesos, ¿qué proporción de hogares
#    gasta más de 1000 pesos? ¿Cuál es el gasto correspondiente al
#    percentil 90?
# 3. Genera 5,000 observaciones de una Gamma(shape=2, rate=0.3) y compara su
#    histograma contra la densidad teórica.
