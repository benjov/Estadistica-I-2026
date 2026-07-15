# ============================================================================
# Estadística I - CIDE
# Tema 3: Esperanza
# 3.e) Esperanza condicional
# ============================================================================
#
# La esperanza condicional E[Y | X = x] es el valor esperado de Y una vez
# que sabemos que X tomó el valor x; se calcula usando la distribución
# condicional de Y dado X = x (tema 2.g).
#
# Un resultado central es la ley de esperanza total (o "ley de las
# esperanzas iteradas"): E[Y] = E[ E[Y | X] ]
# es decir, el promedio (sobre X) de las esperanzas condicionales recupera
# la esperanza marginal de Y.

# ----------------------------------------------------------------------------
# 1. Caso discreto: tabla conjunta de tarjetas de crédito y atraso de pago
# ----------------------------------------------------------------------------

conjunta <- matrix(c(0.20, 0.05,
                      0.25, 0.10,
                      0.30, 0.10),
                    nrow = 3, byrow = TRUE,
                    dimnames = list(X = c("0", "1", "2"), Y = c("0", "1")))

marginal_X <- rowSums(conjunta)
y_vals <- as.numeric(colnames(conjunta))

# E[Y | X = x] para cada valor de x
E_Y_dado_X <- sapply(rownames(conjunta), function(x) {
  cond <- conjunta[x, ] / marginal_X[x]
  sum(y_vals * cond)
})
E_Y_dado_X
# Interpretación: E[Y | X=x] es aquí la PROPORCIÓN esperada de atraso de
# pago (Y es 0/1), para cada número de tarjetas.

# ----------------------------------------------------------------------------
# 2. Ley de esperanza total: E[Y] = sum_x E[Y | X=x] * P(X=x)
# ----------------------------------------------------------------------------

E_Y_total <- sum(E_Y_dado_X * marginal_X)
E_Y_total

E_Y_directo <- sum(y_vals * colSums(conjunta))
E_Y_directo  # debe coincidir con E_Y_total

# ----------------------------------------------------------------------------
# 3. Caso continuo: regresión de la media condicional
# ----------------------------------------------------------------------------
# La función g(x) = E[Y | X = x] es, de hecho, la base conceptual de la
# regresión: buscamos describir cómo cambia el "centro" de Y conforme
# cambia X. Simulamos un caso simple: Y = 3 + 2X + ruido

set.seed(2026)
n <- 5000
X <- runif(n, min = 0, max = 10)
Y <- 3 + 2 * X + rnorm(n, sd = 4)

# Aproximamos E[Y | X = x] promediando Y dentro de "bins" de X
bins <- cut(X, breaks = seq(0, 10, by = 1))
medias_por_bin <- tapply(Y, bins, mean)
centros_bin <- seq(0.5, 9.5, by = 1)

plot(X, Y, pch = 16, col = rgb(0, 0, 1, 0.1),
     main = "E[Y | X = x] aproximada por bins",
     xlab = "X", ylab = "Y")
points(centros_bin, medias_por_bin, col = "firebrick", pch = 19, cex = 1.3)
lines(centros_bin, medias_por_bin, col = "firebrick", lwd = 2)
abline(a = 3, b = 2, col = "darkgreen", lty = 2, lwd = 2)
legend("topleft", legend = c("E[Y|X=x] estimada por bins", "3 + 2x (teórica)"),
       col = c("firebrick", "darkgreen"), lty = c(1, 2), lwd = 2)

# ----------------------------------------------------------------------------
# 4. Varianza condicional y la descomposición de la varianza total
# ----------------------------------------------------------------------------
# Var(Y) = E[ Var(Y|X) ] + Var[ E(Y|X) ]
# (parte de la variación de Y se explica por variación DENTRO de cada valor
# de X, y otra parte por la variación ENTRE los distintos E[Y|X=x])

var_dentro <- tapply(Y, bins, var)
var_condicional_promedio <- mean(var_dentro, na.rm = TRUE)
var_entre <- var(medias_por_bin, na.rm = TRUE)

var(Y)                                        # varianza total (aprox.)
var_condicional_promedio + var_entre          # descomposición aproximada

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Con la tabla de tarjetas de crédito, calcula E[X | Y = 1] (número
#    esperado de tarjetas entre quienes tuvieron atraso de pago).
# 2. Simula Y = 5 + 0.5*X^2 + ruido para X ~ Uniforme(0,10) y aproxima
#    E[Y|X=x] por bins. ¿Por qué ya no se ve como una línea recta?
# 3. Explica con tus palabras por qué E[ E[Y|X] ] = E[Y] tiene sentido
#    intuitivo (pista: es un promedio ponderado de promedios).
