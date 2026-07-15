# ============================================================================
# Estadística I - CIDE
# Tema 3: Esperanza
# 3.c) Varianza y covarianza
# ============================================================================
#
# La varianza mide qué tan dispersos están los valores de X alrededor de su
# media: Var(X) = E[(X - E[X])^2] = E[X^2] - (E[X])^2
#
# La covarianza mide cómo varían conjuntamente dos VA:
# Cov(X, Y) = E[(X - E[X])(Y - E[Y])] = E[XY] - E[X]E[Y]
# La correlación es la covarianza normalizada: Corr(X,Y) = Cov(X,Y)/(sd(X)sd(Y))

# ----------------------------------------------------------------------------
# 1. Varianza de una VA discreta
# ----------------------------------------------------------------------------

x_vals <- 0:4
fmp    <- c(0.55, 0.25, 0.12, 0.06, 0.02)

E_X  <- sum(x_vals * fmp)
E_X2 <- sum(x_vals^2 * fmp)
Var_X <- E_X2 - E_X^2
Var_X
sd_X <- sqrt(Var_X)
sd_X

set.seed(2026)
muestra <- sample(x_vals, size = 200000, replace = TRUE, prob = fmp)
var(muestra); Var_X   # comparación con la simulación

# ----------------------------------------------------------------------------
# 2. Propiedades de la varianza
# ----------------------------------------------------------------------------
# Var(aX + b) = a^2 * Var(X)   (el desplazamiento "b" no afecta la dispersión)

a_const <- 3; b_const <- 10
a_const^2 * Var_X
var(a_const * muestra + b_const)

# Var(X + Y) = Var(X) + Var(Y) + 2*Cov(X, Y)
# Si X y Y son independientes, Cov(X,Y) = 0 y la fórmula se simplifica a
# Var(X + Y) = Var(X) + Var(Y)

# ----------------------------------------------------------------------------
# 3. Covarianza y correlación: ejemplo con datos simulados
# ----------------------------------------------------------------------------
# Ingreso (X) y gasto en consumo (Y) de 500 hogares, correlacionados
# positivamente (a mayor ingreso, mayor gasto, con algo de ruido)

set.seed(2026)
n <- 500
Ingreso <- rnorm(n, mean = 15, sd = 4)
Gasto   <- 2 + 0.6 * Ingreso + rnorm(n, sd = 1.5)

cov(Ingreso, Gasto)
cor(Ingreso, Gasto)

plot(Ingreso, Gasto, pch = 16, col = rgb(0, 0, 1, 0.3),
     main = paste("Corr(Ingreso, Gasto) =", round(cor(Ingreso, Gasto), 2)),
     xlab = "Ingreso (miles de pesos)", ylab = "Gasto en consumo (miles de pesos)")
abline(lm(Gasto ~ Ingreso), col = "firebrick", lwd = 2)

# ----------------------------------------------------------------------------
# 4. Tres casos de referencia: correlación positiva, negativa y nula
# ----------------------------------------------------------------------------

set.seed(2026)
X <- rnorm(n)
Y_positiva <- 0.8 * X + rnorm(n, sd = 0.6)
Y_negativa <- -0.8 * X + rnorm(n, sd = 0.6)
Y_nula     <- rnorm(n)  # independiente de X

par(mfrow = c(1, 3))
plot(X, Y_positiva, pch = 16, col = rgb(0, 0.4, 0, 0.3),
     main = paste("Corr =", round(cor(X, Y_positiva), 2)))
plot(X, Y_negativa, pch = 16, col = rgb(0.7, 0, 0, 0.3),
     main = paste("Corr =", round(cor(X, Y_negativa), 2)))
plot(X, Y_nula, pch = 16, col = rgb(0, 0, 0.7, 0.3),
     main = paste("Corr =", round(cor(X, Y_nula), 2)))
par(mfrow = c(1, 1))

# ----------------------------------------------------------------------------
# 5. Matriz de varianzas-covarianzas de varias variables
# ----------------------------------------------------------------------------

datos <- data.frame(Ingreso, Gasto, Ahorro = Ingreso - Gasto + rnorm(n, sd = 0.5))
cov(datos)
cor(datos)

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Calcula la varianza de la variable "premio" de la lotería del script
#    anterior (premios 0, 100, 1000 con probabilidades 0.9, 0.08, 0.02).
# 2. Verifica por simulación que Var(X + Y) = Var(X) + Var(Y) cuando X y Y
#    son independientes, y que la igualdad falla cuando están correlacionadas
#    positivamente.
# 3. Con el data.frame `datos`, interpreta el signo de Cov(Gasto, Ahorro).
#    ¿Tiene sentido económico?
