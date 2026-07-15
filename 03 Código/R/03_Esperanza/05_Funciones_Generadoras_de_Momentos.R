# ============================================================================
# Estadística I - CIDE
# Tema 3: Esperanza
# 3.f) Funciones generadoras de momentos
# ============================================================================
#
# El k-ésimo momento (no centrado) de X es E[X^k]. Los momentos centrados
# usan (X - E[X])^k; el segundo momento centrado es justamente la varianza.
#
# La función generadora de momentos (FGM) se define como
#     M_X(t) = E[e^{tX}]
# y "genera" todos los momentos vía sus derivadas evaluadas en t = 0:
#     M_X^{(k)}(0) = E[X^k]
# Además, si dos VA tienen la misma FGM (en una vecindad de 0), tienen la
# misma distribución: la FGM caracteriza por completo a la distribución.

# ----------------------------------------------------------------------------
# 1. Momentos vía simulación (recordatorio)
# ----------------------------------------------------------------------------

set.seed(2026)
X <- rexp(300000, rate = 0.25)

E_X1 <- mean(X)       # primer momento = media
E_X2 <- mean(X^2)      # segundo momento (no centrado)
E_X3 <- mean(X^3)      # tercer momento (no centrado)

E_X1; E_X2; E_X3

# Momentos centrados: usan (X - E[X])^k
momento_centrado <- function(x, k) mean((x - mean(x))^k)
Var_X <- momento_centrado(X, 2)   # = varianza
Var_X; var(X)

# Coeficiente de asimetría (skewness), un momento centrado estandarizado
skewness <- momento_centrado(X, 3) / (sd(X)^3)
skewness  # la Exponencial es asimétrica a la derecha (skewness > 0)

# ----------------------------------------------------------------------------
# 2. FGM de la Exponencial(rate = lambda): M_X(t) = lambda / (lambda - t), t < lambda
# ----------------------------------------------------------------------------

lambda <- 0.25
M_X <- function(t) lambda / (lambda - t)

# Derivamos numéricamente para obtener los momentos (derivada numérica simple)
derivada_numerica <- function(f, t, h = 1e-5) (f(t + h) - f(t - h)) / (2 * h)
segunda_derivada_numerica <- function(f, t, h = 1e-4) {
  (f(t + h) - 2 * f(t) + f(t - h)) / h^2
}

derivada_numerica(M_X, 0)          # debe aproximar E[X] = 1/lambda
1 / lambda

segunda_derivada_numerica(M_X, 0)  # debe aproximar E[X^2] = 2/lambda^2
2 / lambda^2

# ----------------------------------------------------------------------------
# 3. FGM de la Normal(mu, sigma): M_X(t) = exp(mu*t + sigma^2*t^2/2)
# ----------------------------------------------------------------------------

mu <- 12; sigma <- 3
M_X_normal <- function(t) exp(mu * t + (sigma^2 * t^2) / 2)

derivada_numerica(M_X_normal, 0)   # ~ E[X] = mu
segunda_derivada_numerica(M_X_normal, 0)  # ~ E[X^2] = sigma^2 + mu^2
sigma^2 + mu^2

# ----------------------------------------------------------------------------
# 4. Propiedad clave: la FGM de una suma de VA independientes es el
#    producto de sus FGM individuales
# ----------------------------------------------------------------------------
# Si X ~ Normal(mu1, s1^2) y Y ~ Normal(mu2, s2^2) son independientes,
# la FGM de S = X + Y es M_X(t) * M_Y(t), que resulta ser la FGM de una
# Normal(mu1+mu2, s1^2+s2^2). Verificamos esta propiedad de "cerradura bajo
# sumas" por simulación:

set.seed(2026)
n <- 300000
X <- rnorm(n, mean = 5, sd = 2)
Y <- rnorm(n, mean = 3, sd = 1.5)
S <- X + Y

mean(S); 5 + 3
var(S); 2^2 + 1.5^2

hist(S, breaks = 50, freq = FALSE, col = "lightgray",
     main = "Suma de dos Normales independientes")
curve(dnorm(x, mean = 5 + 3, sd = sqrt(2^2 + 1.5^2)), add = TRUE,
      col = "firebrick", lwd = 2)

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. La FGM de una Poisson(lambda) es M_X(t) = exp(lambda*(e^t - 1)).
#    Usa las funciones de derivada numérica para obtener E[X] y E[X^2] a
#    partir de esta FGM, y compáralos contra lambda y lambda + lambda^2.
# 2. Explica por qué, si dos VA tienen momentos iguales de todos los
#    órdenes, es razonable (bajo condiciones de regularidad) concluir que
#    tienen la misma distribución.
# 3. Verifica por simulación que la suma de dos Poisson independientes,
#    Poisson(lambda1) y Poisson(lambda2), es Poisson(lambda1 + lambda2).
