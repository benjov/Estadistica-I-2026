# ============================================================================
# Estadística I - CIDE
# Tema 5: Convergencia de variables aleatorias
# 5.a) Tipos de convergencia
# ============================================================================
#
# Cuando hablamos de "convergencia" de una secuencia de VA X_1, X_2, ...,
# necesitamos precisar en qué SENTIDO convergen, porque una VA no es un
# número: es una función aleatoria. Ilustramos los dos tipos más usados en
# este curso: convergencia en probabilidad y convergencia en distribución.

# ----------------------------------------------------------------------------
# 1. Convergencia en probabilidad: X_n ->p X
# ----------------------------------------------------------------------------
# Definición: para todo epsilon > 0,  P(|X_n - X| > epsilon) -> 0  cuando n -> Inf
#
# Ejemplo: X_n = media de n observaciones IID Uniforme(0,1). Converge en
# probabilidad a la constante mu = 0.5 (este es, de hecho, un caso de la
# Ley de los Grandes Números que veremos en el siguiente script).

set.seed(2026)
epsilon <- 0.05
tamanos_n <- c(5, 20, 50, 100, 500, 2000)
n_repeticiones <- 5000

prob_fuera_epsilon <- sapply(tamanos_n, function(n) {
  medias <- replicate(n_repeticiones, mean(runif(n)))
  mean(abs(medias - 0.5) > epsilon)
})

plot(tamanos_n, prob_fuera_epsilon, type = "b", pch = 19, col = "steelblue",
     log = "x", xlab = "n (tamaño de muestra, escala log)",
     ylab = "P(|X_n - 0.5| > 0.05)",
     main = "Convergencia en probabilidad de la media muestral")
abline(h = 0, lty = 2, col = "gray50")

# ----------------------------------------------------------------------------
# 2. Convergencia en distribución: X_n ->d X
# ----------------------------------------------------------------------------
# Definición: la FDA de X_n converge puntualmente a la FDA de X en cada
# punto de continuidad de esta última. NO exige que los valores de X_n se
# acerquen a los de X, solo que su DISTRIBUCIÓN se parezca cada vez más.
#
# Ejemplo (adelanto del Teorema del Límite Central, script 04): la media
# estandarizada de n observaciones IID converge en distribución a una
# Normal(0,1), sin importar la distribución original (aquí, Exponencial).

set.seed(2026)
tasa <- 1
graficar_convergencia_distribucion <- function(n) {
  medias_estandarizadas <- replicate(4000, {
    x <- rexp(n, rate = tasa)
    (mean(x) - 1 / tasa) / ((1 / tasa) / sqrt(n))
  })
  hist(medias_estandarizadas, breaks = 40, freq = FALSE, col = "lightgray",
       xlim = c(-4, 4), main = paste("n =", n), xlab = "Media estandarizada")
  curve(dnorm(x), add = TRUE, col = "firebrick", lwd = 2)
}

par(mfrow = c(2, 2))
for (n in c(2, 5, 30, 100)) graficar_convergencia_distribucion(n)
par(mfrow = c(1, 1))

# ----------------------------------------------------------------------------
# 3. Convergencia en probabilidad implica convergencia en distribución
#    (pero no al revés, salvo que el límite sea una constante)
# ----------------------------------------------------------------------------
# Esta relación es un resultado teórico; aquí solo lo dejamos anotado como
# referencia para no confundir ambos conceptos:
#   - Convergencia en probabilidad: habla de qué tan cerca están los VALORES
#     de X_n y X.
#   - Convergencia en distribución: habla de qué tan parecidas son las
#     DISTRIBUCIONES (formas de las curvas), sin comparar valor por valor.

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Repite el ejercicio de convergencia en probabilidad pero con
#    X_i ~ Exponencial(rate = 2) en lugar de Uniforme(0,1). Ajusta el valor
#    al que debería converger la media muestral.
# 2. Repite el histograma de convergencia en distribución usando
#    X_i ~ Bernoulli(p = 0.1) en vez de Exponencial. ¿A partir de qué n la
#    aproximación normal ya se ve razonable, dado que p está lejos de 0.5?
