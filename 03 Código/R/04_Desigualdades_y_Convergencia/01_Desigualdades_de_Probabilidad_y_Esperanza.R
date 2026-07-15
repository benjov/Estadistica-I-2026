# ============================================================================
# Estadística I - CIDE
# Tema 4: Algunas desigualdades importantes
# 4.a) Desigualdades de probabilidad  |  4.b) Desigualdades de esperanza
# ============================================================================
#
# Las desigualdades de este tema acotan probabilidades o esperanzas SIN
# necesitar conocer la distribución exacta de la VA: son "cotas universales"
# muy útiles cuando solo conocemos la media y/o la varianza.

# ----------------------------------------------------------------------------
# 1. Desigualdad de Markov: para X >= 0 y a > 0,  P(X >= a) <= E[X]/a
# ----------------------------------------------------------------------------
# Ejemplo: el tiempo de reparación de un equipo (en horas) tiene media 4.
# Cota superior para P(tiempo >= 10), SIN saber la distribución exacta.

E_X <- 4
a <- 10
cota_markov <- E_X / a
cota_markov

# Comparamos contra un caso concreto: si en realidad X ~ Exponencial con
# esa misma media (rate = 1/E_X), ¿qué tan ajustada es la cota?
tasa <- 1 / E_X
prob_real <- 1 - pexp(a, rate = tasa)
data.frame(cota_markov = cota_markov, probabilidad_real = prob_real)
# La desigualdad de Markov es válida (cota_markov >= probabilidad_real)
# pero suele ser una cota poco ajustada (conservadora).

# ----------------------------------------------------------------------------
# 2. Desigualdad de Chebyshev: P(|X - mu| >= k*sigma) <= 1/k^2
# ----------------------------------------------------------------------------
# Usa media Y varianza, y por eso suele dar cotas más ajustadas que Markov.

mu <- 12; sigma <- 3
k <- 2
cota_chebyshev <- 1 / k^2
cota_chebyshev

# Comparación con una Normal(12, 3), donde sabemos la probabilidad exacta
prob_real_normal <- pnorm(mu - k * sigma, mu, sigma) +
  (1 - pnorm(mu + k * sigma, mu, sigma))
data.frame(cota_chebyshev = cota_chebyshev, probabilidad_real = prob_real_normal)
# De nuevo, Chebyshev es válida como cota superior (0.25 vs. ~0.046 real),
# pero es "universal": funciona para CUALQUIER distribución con esa media y
# varianza, no solo la Normal, lo cual explica que sea conservadora.

# Verificación con una distribución muy distinta a la Normal (Exponencial)
mu_exp <- 1 / tasa
sigma_exp <- 1 / tasa
# mu_exp - k*sigma_exp < 0 aquí, y la Exponencial no tiene soporte negativo,
# así que ese lado de la cola aporta probabilidad 0.
prob_real_exp <- 1 - pexp(mu_exp + k * sigma_exp, rate = tasa)
data.frame(cota_chebyshev = cota_chebyshev, probabilidad_real_exp = prob_real_exp)

# ----------------------------------------------------------------------------
# 3. Chebyshev por simulación: verificar que la cota nunca se viola
# ----------------------------------------------------------------------------

set.seed(2026)
distribuciones <- list(
  Normal = rnorm(200000, mu, sigma),
  Exponencial = rexp(200000, rate = tasa),
  Uniforme = runif(200000, mu - sqrt(3) * sigma, mu + sqrt(3) * sigma)  # misma sigma
)

for (nombre in names(distribuciones)) {
  x <- distribuciones[[nombre]]
  media_x <- mean(x); sd_x <- sd(x)
  prob_simulada <- mean(abs(x - media_x) >= k * sd_x)
  cat(sprintf("%-12s P(|X-mu|>=%dsigma) simulada = %.4f  (cota Chebyshev = %.4f)\n",
              nombre, k, prob_simulada, cota_chebyshev))
}

# ----------------------------------------------------------------------------
# 4. Desigualdad de Jensen: para g convexa, E[g(X)] >= g(E[X])
# ----------------------------------------------------------------------------
# Ejemplo con g(x) = x^2 (convexa)

set.seed(2026)
X <- rgamma(200000, shape = 2, rate = 0.5)
g <- function(x) x^2

mean(g(X))        # E[g(X)]
g(mean(X))        # g(E[X])
mean(g(X)) >= g(mean(X))  # Jensen: se cumple

# Con g cóncava (por ejemplo, log(x)), la desigualdad se invierte:
# E[g(X)] <= g(E[X])
g_concava <- function(x) log(x)
mean(g_concava(X))
g_concava(mean(X))
mean(g_concava(X)) <= g_concava(mean(X))

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Con E[X] = 6 (X >= 0), usa Markov para acotar P(X >= 20).
# 2. Con mu = 500, sigma = 80 (gasto semanal de un hogar), usa Chebyshev
#    para acotar P(|X - 500| >= 160). Compara la cota contra la probabilidad
#    real si asumieras X ~ Normal(500, 80).
# 3. Propón otra función convexa g y verifica numéricamente la desigualdad
#    de Jensen con una distribución de tu elección.
