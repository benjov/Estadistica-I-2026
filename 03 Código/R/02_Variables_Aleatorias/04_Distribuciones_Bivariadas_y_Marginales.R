# ============================================================================
# Estadística I - CIDE
# Tema 2: Variables Aleatorias
# 2.d) Distribuciones bivariadas   |   2.e) Distribuciones marginales
# ============================================================================
#
# Cuando estudiamos dos variables aleatorias X y Y de manera conjunta,
# necesitamos su distribución conjunta: P(X = x, Y = y) en el caso discreto,
# o f(x, y) en el caso continuo. A partir de la conjunta podemos recuperar
# el comportamiento individual de cada variable (distribuciones marginales)
# "sumando" (caso discreto) o "integrando" (caso continuo) sobre la otra.

# ----------------------------------------------------------------------------
# 1. Caso discreto: tabla de probabilidad conjunta
# ----------------------------------------------------------------------------
# Ejemplo: X = número de tarjetas de crédito de una persona (0, 1, 2),
# Y = si tuvo o no un atraso de pago en el último año (0 = no, 1 = sí).

conjunta <- matrix(c(0.20, 0.05,   # X=0: Y=0, Y=1
                      0.25, 0.10,  # X=1: Y=0, Y=1
                      0.30, 0.10), # X=2: Y=0, Y=1
                    nrow = 3, byrow = TRUE,
                    dimnames = list(X = c("0", "1", "2"), Y = c("0", "1")))
conjunta
sum(conjunta)  # debe ser 1

# ----------------------------------------------------------------------------
# 2. Distribuciones marginales: "colapsar" una dimensión de la tabla
# ----------------------------------------------------------------------------

marginal_X <- rowSums(conjunta)  # P(X = x), sumando sobre y
marginal_Y <- colSums(conjunta)  # P(Y = y), sumando sobre x

marginal_X
marginal_Y

barplot(marginal_X, col = "steelblue", main = "Marginal de X (# tarjetas)",
        xlab = "x", ylab = "P(X = x)")
barplot(marginal_Y, col = "firebrick", main = "Marginal de Y (atraso de pago)",
        xlab = "y", ylab = "P(Y = y)")

# ----------------------------------------------------------------------------
# 3. Momentos de la conjunta: covarianza y correlación (se profundiza en el
#    tema 3, Esperanza; aquí solo se ilustra el cálculo desde la conjunta)
# ----------------------------------------------------------------------------

x_vals <- as.numeric(rownames(conjunta))
y_vals <- as.numeric(colnames(conjunta))

E_X <- sum(x_vals * marginal_X)
E_Y <- sum(y_vals * marginal_Y)

E_XY <- sum(outer(x_vals, y_vals) * conjunta)
Cov_XY <- E_XY - E_X * E_Y
Cov_XY

# ----------------------------------------------------------------------------
# 4. Caso continuo: densidad conjunta sobre una malla
# ----------------------------------------------------------------------------
# Ejemplo ilustrativo: X = gasto en alimentos, Y = gasto en transporte
# (ambos estandarizados), con una densidad Normal bivariada.
# Usamos solo funciones base de R para construir la densidad en una malla.

dmvnorm_diag <- function(x, y, mu1 = 0, mu2 = 0, s1 = 1, s2 = 1, rho = 0.5) {
  z1 <- (x - mu1) / s1
  z2 <- (y - mu2) / s2
  densidad <- (1 / (2 * pi * s1 * s2 * sqrt(1 - rho^2))) *
    exp(-1 / (2 * (1 - rho^2)) * (z1^2 - 2 * rho * z1 * z2 + z2^2))
  densidad
}

grid_x <- seq(-3, 3, length.out = 60)
grid_y <- seq(-3, 3, length.out = 60)
densidad_conjunta <- outer(grid_x, grid_y, dmvnorm_diag)

contour(grid_x, grid_y, densidad_conjunta, col = "steelblue", lwd = 1.5,
        main = "Densidad conjunta Normal bivariada (rho = 0.5)",
        xlab = "X (gasto en alimentos, estandarizado)",
        ylab = "Y (gasto en transporte, estandarizado)")

# Marginal de X (integrando numéricamente sobre y): debe verse como una
# Normal(0,1) univariada.
marginal_x_num <- rowSums(densidad_conjunta) * diff(grid_y)[1]
plot(grid_x, marginal_x_num, type = "l", lwd = 2, col = "darkgreen",
     main = "Marginal de X obtenida por integración numérica",
     xlab = "x", ylab = "f_X(x)")
curve(dnorm(x), add = TRUE, col = "firebrick", lty = 2, lwd = 2)
legend("topright", legend = c("Marginal numérica", "Normal(0,1) teórica"),
       col = c("darkgreen", "firebrick"), lty = c(1, 2), lwd = 2)

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Con la tabla conjunta de tarjetas de crédito, calcula
#    P(X = 2 y Y = 1) y compárala con marginal_X["2"] * marginal_Y["1"].
#    ¿Qué te dice esa comparación sobre la relación entre X y Y? (pista:
#    se retoma formalmente en el siguiente script sobre independencia)
# 2. Construye tu propia tabla conjunta 3x3 con contexto económico y calcula
#    ambas marginales.
# 3. Cambia rho en dmvnorm_diag a 0.9 y a -0.5, y observa cómo cambian los
#    contornos de la densidad conjunta.
