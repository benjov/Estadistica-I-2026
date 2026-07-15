# ============================================================================
# Estadística I - CIDE
# Tema 5: Convergencia de variables aleatorias
# 5.c) El Teorema del Límite Central
# ============================================================================
#
# El Teorema del Límite Central (TLC) dice que, si X_1, ..., X_n son IID con
# media mu y varianza finita sigma^2, entonces la media muestral
# estandarizada converge en distribución a una Normal(0,1):
#
#     (X_bar_n - mu) / (sigma / sqrt(n))   ->d   Normal(0, 1)
#
# Lo notable: esto se cumple SIN IMPORTAR la distribución original de las
# X_i (siempre que tenga varianza finita). Es la razón por la que la Normal
# aparece tan seguido en estadística e inferencia.

# ----------------------------------------------------------------------------
# 1. TLC partiendo de una distribución muy distinta a la Normal: Exponencial
# ----------------------------------------------------------------------------

set.seed(2026)
tasa <- 0.5
mu <- 1 / tasa
sigma <- 1 / tasa

simular_media_estandarizada <- function(n, distribucion_rng, n_repeticiones = 5000) {
  replicate(n_repeticiones, {
    x <- distribucion_rng(n)
    (mean(x) - mu) / (sigma / sqrt(n))
  })
}

rng_exp <- function(n) rexp(n, rate = tasa)

par(mfrow = c(2, 2))
for (n in c(1, 5, 30, 100)) {
  medias_est <- simular_media_estandarizada(n, rng_exp)
  hist(medias_est, breaks = 40, freq = FALSE, col = "lightgray",
       xlim = c(-4, 4), main = paste("n =", n),
       xlab = "Media estandarizada")
  curve(dnorm(x), add = TRUE, col = "firebrick", lwd = 2)
}
par(mfrow = c(1, 1))
# Con n=1 la forma es idéntica a la Exponencial (muy asimétrica). Conforme n
# crece, la forma se "normaliza" cada vez más rápido.

# ----------------------------------------------------------------------------
# 2. TLC partiendo de una distribución discreta: Bernoulli (base del TLC
#    aplicado a proporciones)
# ----------------------------------------------------------------------------

p <- 0.1  # proporción "rara": el TLC tarda más en verse bien
mu_bern <- p
sigma_bern <- sqrt(p * (1 - p))
rng_bern <- function(n) rbinom(n, size = 1, prob = p)

set.seed(2026)
par(mfrow = c(2, 2))
for (n in c(5, 30, 100, 500)) {
  medias_est <- replicate(5000, {
    x <- rng_bern(n)
    (mean(x) - mu_bern) / (sigma_bern / sqrt(n))
  })
  hist(medias_est, breaks = 40, freq = FALSE, col = "lightgray",
       xlim = c(-4, 4), main = paste("Bernoulli(0.1), n =", n),
       xlab = "Proporción muestral estandarizada")
  curve(dnorm(x), add = TRUE, col = "firebrick", lwd = 2)
}
par(mfrow = c(1, 1))

# ----------------------------------------------------------------------------
# 3. Uso práctico del TLC: aproximar probabilidades sobre X_bar sin conocer
#    la distribución exacta de la suma
# ----------------------------------------------------------------------------
# Ejemplo: el gasto diario de los clientes de una tienda tiene media
# mu = 250 y desviación estándar sigma = 90 (distribución desconocida,
# claramente no Normal: muchos gastos bajos y algunos muy altos). Con una
# muestra de n = 64 clientes, ¿cuál es la probabilidad aproximada de que el
# gasto promedio muestral sea mayor a 270?

mu_gasto <- 250; sigma_gasto <- 90; n_clientes <- 64
error_estandar <- sigma_gasto / sqrt(n_clientes)

z <- (270 - mu_gasto) / error_estandar
1 - pnorm(z)  # aproximación vía TLC

# Verificamos con una simulación usando una distribución asimétrica
# (Log-Normal) que comparte esa media y desviación estándar:
# para una LogNormal, si Y = exp(mu_ln + sigma_ln*Z), entonces:
sigma_ln <- sqrt(log(1 + (sigma_gasto / mu_gasto)^2))
mu_ln <- log(mu_gasto) - sigma_ln^2 / 2

set.seed(2026)
prob_simulada <- mean(replicate(20000, {
  gasto_muestra <- rlnorm(n_clientes, meanlog = mu_ln, sdlog = sigma_ln)
  mean(gasto_muestra) > 270
}))
data.frame(aproximacion_TLC = 1 - pnorm(z), simulacion = prob_simulada)

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Repite el panel de histogramas del TLC usando una Uniforme(0, 1) como
#    distribución base. ¿A partir de qué n la aproximación normal ya se ve
#    razonable?
# 2. El tiempo de espera en una fila tiene media 8 minutos y desviación
#    estándar 5 minutos. Para una muestra de 49 personas, usa el TLC para
#    aproximar P(tiempo de espera promedio > 9 minutos).
# 3. Explica, con tus propias palabras, por qué el TLC es la razón por la
#    que en la práctica solemos usar la distribución Normal para construir
#    intervalos de confianza y pruebas de hipótesis sobre medias
#    (tema que se retoma en Estadística II).
