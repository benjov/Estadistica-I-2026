# ============================================================================
# Estadística I - CIDE
# Tema 2: Variables Aleatorias
# 2.h) Distribuciones multivariadas y muestras IID  |  2.i) Distribuciones multivariadas
# ============================================================================
#
# Generalizamos de dos a k variables aleatorias X_1, ..., X_k. Un caso
# particular MUY importante en estadística es el de una muestra aleatoria
# IID (independiente e idénticamente distribuida): X_1, ..., X_n son
# independientes entre sí y todas comparten la misma distribución. Este es
# el supuesto detrás de casi toda la inferencia estadística.

# ----------------------------------------------------------------------------
# 1. Simular una muestra IID
# ----------------------------------------------------------------------------
# Ejemplo: X_1, ..., X_30 representan el gasto semanal (en pesos) de 30
# hogares elegidos al azar de una población con distribución
# Normal(mu = 500, sigma = 80). Al ser elegidos al azar de la misma
# población, asumimos que son independientes e idénticamente distribuidos.

set.seed(2026)
n <- 30
mu <- 500; sigma <- 80

muestra <- rnorm(n, mean = mu, sd = sigma)
muestra

# Cada X_i, individualmente, tiene la misma distribución Normal(500, 80)
mean(muestra); sd(muestra)  # estimaciones muestrales, no exactamente mu y sigma

# ----------------------------------------------------------------------------
# 2. La distribución conjunta de una muestra IID es el producto de las
#    marginales (por la independencia)
# ----------------------------------------------------------------------------
# f(x_1, ..., x_n) = f(x_1) * f(x_2) * ... * f(x_n)
# Esto es la base de la función de verosimilitud que se usará en
# Estadística II, pero aquí basta con verificar la idea con un ejemplo
# discreto pequeño.

omega_moneda <- c("A", "S")
p_aguila <- 0.5

# Probabilidad conjunta de observar la secuencia (A, A, S) en 3 lanzamientos
# independientes de la misma moneda:
secuencia <- c("A", "A", "S")
prob_secuencia <- prod(ifelse(secuencia == "A", p_aguila, 1 - p_aguila))
prob_secuencia  # = p * p * (1-p), coincide con multiplicar las marginales

# ----------------------------------------------------------------------------
# 3. Distribución muestral de un estadístico: la media muestral
# ----------------------------------------------------------------------------
# Un "estadístico" es cualquier función de la muestra, como la media
# muestral X_bar = (1/n) * sum(X_i). Como depende de variables aleatorias,
# X_bar es en sí misma una variable aleatoria, con su propia distribución
# (la "distribución muestral"). Simulamos su comportamiento repitiendo el
# experimento de "tomar una muestra de tamaño n" muchas veces.

set.seed(2026)
n_repeticiones <- 10000
medias_muestrales <- replicate(n_repeticiones, {
  muestra_i <- rnorm(n, mean = mu, sd = sigma)
  mean(muestra_i)
})

hist(medias_muestrales, breaks = 40, freq = FALSE, col = "lightgray",
     main = "Distribución muestral de X_bar (n = 30)",
     xlab = "Media muestral")
curve(dnorm(x, mean = mu, sd = sigma / sqrt(n)), add = TRUE,
      col = "firebrick", lwd = 2)
# Nota: la distribución muestral de X_bar se concentra mucho más que la
# distribución de una sola observación X_i (compárese sigma con
# sigma/sqrt(n)). Este resultado se formaliza en el tema 4 (Convergencia).

sd(medias_muestrales)      # desviación estándar simulada de X_bar
sigma / sqrt(n)            # error estándar teórico de la media

# ----------------------------------------------------------------------------
# 4. Vector aleatorio: varias VA representadas como una matriz
# ----------------------------------------------------------------------------
# En economía es común trabajar con varios indicadores simultáneamente para
# la misma unidad de observación (hogar, empresa, país). Podemos simular un
# vector aleatorio (X, Y, Z) con cierta estructura de dependencia usando
# una matriz de covarianzas, sin depender de paquetes externos.

set.seed(2026)
media_vec <- c(Ingreso = 12, Gasto = 8, Ahorro = 4)
Sigma <- matrix(c(4,   2,   0.5,
                   2,   3,   0.2,
                   0.5, 0.2, 1.5),
                nrow = 3, byrow = TRUE)

# Descomposición de Cholesky para generar el vector correlacionado a partir
# de normales estándar independientes: X = mu + L %*% Z, con Sigma = L L'
L <- chol(Sigma)  # chol() da la matriz triangular superior U tal que U'U = Sigma
Z <- matrix(rnorm(3 * 5000), nrow = 3)
X_multivariado <- t(media_vec + t(L) %*% Z)
colnames(X_multivariado) <- names(media_vec)

round(colMeans(X_multivariado), 2)   # cercano a media_vec
round(cov(X_multivariado), 2)        # cercano a Sigma

pairs(X_multivariado, pch = 16, col = rgb(0, 0, 1, 0.1),
      main = "Vector aleatorio multivariado simulado")

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Repite la simulación de la distribución muestral de X_bar pero con
#    n = 5 y n = 100. Compara la dispersión de las tres distribuciones
#    muestrales resultantes.
# 2. Verifica numéricamente que, para una muestra IID Bernoulli(p=0.3) de
#    tamaño 4, la probabilidad conjunta de la secuencia (1,0,1,1) es el
#    producto de las probabilidades marginales.
# 3. Cambia la matriz Sigma del vector multivariado para que Ingreso y
#    Ahorro tengan correlación negativa, y regenera la gráfica pairs().
