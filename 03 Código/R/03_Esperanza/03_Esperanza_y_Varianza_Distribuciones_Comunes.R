# ============================================================================
# Estadística I - CIDE
# Tema 3: Esperanza
# 3.d) Esperanza y varianza de algunas variables aleatorias
# ============================================================================
#
# Cada familia de distribuciones tiene fórmulas cerradas para su esperanza y
# varianza en función de sus parámetros. Aquí las verificamos numéricamente
# comparando la fórmula teórica contra una simulación de Monte Carlo.

verificar <- function(nombre, media_teo, var_teo, muestra) {
  data.frame(
    distribucion = nombre,
    media_teorica = media_teo,
    media_simulada = mean(muestra),
    varianza_teorica = var_teo,
    varianza_simulada = var(muestra)
  )
}

set.seed(2026)
n_sim <- 300000
resultados <- list()

# ----------------------------------------------------------------------------
# 1. Bernoulli(p):        E[X] = p            Var(X) = p(1-p)
# ----------------------------------------------------------------------------
p <- 0.3
muestra <- rbinom(n_sim, size = 1, prob = p)
resultados$bernoulli <- verificar("Bernoulli(p=0.3)", p, p * (1 - p), muestra)

# ----------------------------------------------------------------------------
# 2. Binomial(n, p):      E[X] = np           Var(X) = np(1-p)
# ----------------------------------------------------------------------------
n_b <- 20; p_b <- 0.4
muestra <- rbinom(n_sim, size = n_b, prob = p_b)
resultados$binomial <- verificar("Binomial(20, 0.4)", n_b * p_b,
                                  n_b * p_b * (1 - p_b), muestra)

# ----------------------------------------------------------------------------
# 3. Geométrica (soporte 0,1,2,...):  E[X] = (1-p)/p   Var(X) = (1-p)/p^2
# ----------------------------------------------------------------------------
p_g <- 0.15
muestra <- rgeom(n_sim, prob = p_g)
resultados$geometrica <- verificar("Geométrica(p=0.15)", (1 - p_g) / p_g,
                                    (1 - p_g) / p_g^2, muestra)

# ----------------------------------------------------------------------------
# 4. Poisson(lambda):     E[X] = lambda       Var(X) = lambda
# ----------------------------------------------------------------------------
lambda <- 4
muestra <- rpois(n_sim, lambda = lambda)
resultados$poisson <- verificar("Poisson(4)", lambda, lambda, muestra)

# ----------------------------------------------------------------------------
# 5. Uniforme(a, b):      E[X] = (a+b)/2      Var(X) = (b-a)^2/12
# ----------------------------------------------------------------------------
a_u <- 2; b_u <- 8
muestra <- runif(n_sim, min = a_u, max = b_u)
resultados$uniforme <- verificar("Uniforme(2,8)", (a_u + b_u) / 2,
                                  (b_u - a_u)^2 / 12, muestra)

# ----------------------------------------------------------------------------
# 6. Exponencial(rate):   E[X] = 1/rate       Var(X) = 1/rate^2
# ----------------------------------------------------------------------------
tasa <- 0.25
muestra <- rexp(n_sim, rate = tasa)
resultados$exponencial <- verificar("Exponencial(0.25)", 1 / tasa,
                                     1 / tasa^2, muestra)

# ----------------------------------------------------------------------------
# 7. Normal(mu, sigma):   E[X] = mu           Var(X) = sigma^2
# ----------------------------------------------------------------------------
mu_n <- 12; sigma_n <- 3
muestra <- rnorm(n_sim, mean = mu_n, sd = sigma_n)
resultados$normal <- verificar("Normal(12, 3)", mu_n, sigma_n^2, muestra)

# ----------------------------------------------------------------------------
# Tabla resumen
# ----------------------------------------------------------------------------

tabla_resumen <- do.call(rbind, resultados)
rownames(tabla_resumen) <- NULL
print(tabla_resumen, digits = 3)

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Agrega a la tabla la verificación para una Binomial Negativa(r=3, p=0.15),
#    cuya E[X] = r(1-p)/p y Var(X) = r(1-p)/p^2.
# 2. Explica, en tus palabras, por qué en la Poisson la media y la varianza
#    son siempre iguales, y qué implicación práctica tiene esto al modelar
#    conteos "sobredispersos" (varianza mayor a la media) en economía.
# 3. Para la Normal(12, 3), calcula el coeficiente de variación
#    (sd/media) y compáralo contra el de la Exponencial(0.25).
