# ============================================================================
# Estadística I - CIDE
# Tema 2: Variables Aleatorias
# 2.i) Transformaciones de variables aleatorias  (tema optativo)
# ============================================================================
#
# Si Y = g(X) para alguna función g, ¿cómo obtenemos la distribución de Y a
# partir de la distribución de X? Revisamos el caso discreto (directo) y el
# método del Jacobiano para el caso continuo con g estrictamente monótona.

# ----------------------------------------------------------------------------
# 1. Caso discreto: transformar es directo
# ----------------------------------------------------------------------------
# X = número de productos defectuosos en un lote de 10 (Binomial(10, 0.1))
# Y = costo de reproceso = 50 + 20*X (pesos)

n <- 10; p <- 0.1
x_vals <- 0:n
fmp_X <- dbinom(x_vals, n, p)

y_vals <- 50 + 20 * x_vals   # g(x) = 50 + 20x es estrictamente creciente

data.frame(x = x_vals, y = y_vals, prob = round(fmp_X, 4))
# Como g es una función uno-a-uno, la fmp de Y es la MISMA que la de X,
# solo "reetiquetada" con los nuevos valores y = g(x).

# ----------------------------------------------------------------------------
# 2. Caso continuo, método del Jacobiano (g monótona y diferenciable)
# ----------------------------------------------------------------------------
# Si Y = g(X) con g estrictamente monótona e invertible:
#     f_Y(y) = f_X( g^{-1}(y) ) * |d/dy g^{-1}(y)|
#
# Ejemplo: X ~ Uniforme(0, 1), Y = -ln(X) / lambda  (transformación inversa,
# la misma que usa R internamente para simular una Exponencial(lambda))

lambda <- 2
g_inversa <- function(y) exp(-lambda * y)          # x = g^{-1}(y)
derivada_g_inversa <- function(y) lambda * exp(-lambda * y)  # |dx/dy|

f_Y_teorica <- function(y) dunif(g_inversa(y), 0, 1) * derivada_g_inversa(y)

curve(f_Y_teorica(x), from = 0, to = 5, lwd = 2, col = "steelblue",
      main = "Densidad de Y = -ln(X)/lambda, con X ~ Uniforme(0,1)",
      xlab = "y", ylab = "f_Y(y)")
curve(dexp(x, rate = lambda), add = TRUE, col = "firebrick", lty = 2, lwd = 2)
legend("topright", legend = c("Vía Jacobiano", "Exponencial(lambda) directa"),
       col = c("steelblue", "firebrick"), lty = c(1, 2), lwd = 2)

# Verificación por simulación
set.seed(2026)
X_unif <- runif(100000)
Y_transformada <- -log(X_unif) / lambda

hist(Y_transformada, breaks = 50, freq = FALSE, col = "lightgray",
     main = "Y = -ln(X)/lambda simulada vs. densidad teórica")
curve(dexp(x, rate = lambda), add = TRUE, col = "firebrick", lwd = 2)

# ----------------------------------------------------------------------------
# 3. Transformación lineal de una Normal
# ----------------------------------------------------------------------------
# Si X ~ Normal(mu, sigma^2) y Y = a + bX, entonces
#     Y ~ Normal(a + b*mu, b^2 * sigma^2)
# Este resultado (que se demuestra formalmente con el Jacobiano) es la base
# de la estandarización Z = (X - mu)/sigma que vimos en el script anterior.

mu <- 10; sigma <- 2; a <- 5; b <- 3
set.seed(2026)
X <- rnorm(100000, mu, sigma)
Y <- a + b * X

mean(Y); a + b * mu     # deben coincidir aproximadamente
sd(Y); b * sigma        # deben coincidir aproximadamente

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Si X ~ Exponencial(rate = 1), encuentra (a mano) y verifica en R por
#    simulación la densidad de Y = X^2.
# 2. Verifica que si X ~ Uniforme(0,1), entonces Y = a + (b-a)*X tiene
#    distribución Uniforme(a, b), tanto analíticamente como por simulación.
