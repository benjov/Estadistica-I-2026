# ============================================================================
# Estadística I - CIDE
# Tema 2: Variables Aleatorias
# 2.f) Variables aleatorias independientes | 2.g) Distribuciones condicionales
# ============================================================================
#
# X y Y son independientes si su distribución conjunta es el producto de
# las marginales para TODO (x, y): P(X=x, Y=y) = P(X=x) * P(Y=y).
#
# La distribución condicional de Y dado X = x se obtiene "cortando" una
# fila (o columna) de la tabla conjunta y renormalizando entre la marginal:
#     P(Y = y | X = x) = P(X = x, Y = y) / P(X = x)

# ----------------------------------------------------------------------------
# 1. Reutilizamos la tabla conjunta del script anterior
# ----------------------------------------------------------------------------

conjunta <- matrix(c(0.20, 0.05,
                      0.25, 0.10,
                      0.30, 0.10),
                    nrow = 3, byrow = TRUE,
                    dimnames = list(X = c("0", "1", "2"), Y = c("0", "1")))

marginal_X <- rowSums(conjunta)
marginal_Y <- colSums(conjunta)

# ----------------------------------------------------------------------------
# 2. ¿Son X y Y independientes? Comparamos la conjunta contra el producto de
#    marginales para cada celda.
# ----------------------------------------------------------------------------

producto_marginales <- outer(marginal_X, marginal_Y)
dimnames(producto_marginales) <- dimnames(conjunta)

round(conjunta, 3)
round(producto_marginales, 3)

max(abs(conjunta - producto_marginales))  # si es 0, son independientes
# En este ejemplo NO son independientes: el número de tarjetas de crédito
# sí está asociado con la probabilidad de atraso de pago.

# ----------------------------------------------------------------------------
# 3. Distribuciones condicionales
# ----------------------------------------------------------------------------
# Distribución de Y dado X = 0 (personas sin tarjetas de crédito)
cond_Y_dado_X0 <- conjunta["0", ] / marginal_X["0"]
cond_Y_dado_X0
sum(cond_Y_dado_X0)  # debe sumar 1

# Distribución de Y dado X = 2 (personas con 2 tarjetas)
cond_Y_dado_X2 <- conjunta["2", ] / marginal_X["2"]
cond_Y_dado_X2

# Comparación gráfica: si X y Y fueran independientes, ambas distribuciones
# condicionales serían idénticas a la marginal de Y.
barplot(rbind(marginal_Y, cond_Y_dado_X0, cond_Y_dado_X2),
        beside = TRUE, col = c("gray70", "steelblue", "firebrick"),
        legend.text = c("Marginal de Y", "Y | X=0", "Y | X=2"),
        main = "Distribuciones condicionales vs. marginal de Y",
        xlab = "y (0 = sin atraso, 1 = con atraso)")

# ----------------------------------------------------------------------------
# 4. Ejemplo con VA continuas independientes: simulación
# ----------------------------------------------------------------------------
# Si X y Y son independientes, simularlas por separado y unirlas produce la
# misma distribución conjunta que si las simuláramos "juntas".

set.seed(2026)
n <- 5000
X_continua <- rnorm(n, mean = 12, sd = 3)   # p.ej. ingreso
Y_continua <- rexp(n, rate = 0.5)           # p.ej. tiempo de espera, independiente de X

cor(X_continua, Y_continua)  # debe ser cercano a 0 (independientes -> no correlacionadas)

plot(X_continua, Y_continua, pch = 16, col = rgb(0, 0, 1, 0.15),
     main = "X y Y simuladas de forma independiente",
     xlab = "X (ingreso)", ylab = "Y (tiempo de espera)")

# ----------------------------------------------------------------------------
# 5. Contraste: dos variables dependientes
# ----------------------------------------------------------------------------
# Y2 se construye a partir de X, así que claramente no son independientes.

Y2_dependiente <- 0.8 * X_continua + rnorm(n, sd = 2)
cor(X_continua, Y2_dependiente)

plot(X_continua, Y2_dependiente, pch = 16, col = rgb(1, 0, 0, 0.15),
     main = "X y Y2: variables dependientes",
     xlab = "X", ylab = "Y2")

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Calcula la distribución condicional de X dado Y = 1 (personas con
#    atraso de pago) y compárala contra la marginal de X.
# 2. Construye una tabla conjunta 2x2 en la que X y Y SÍ sean independientes
#    (verifica la condición del producto de marginales) y otra en la que
#    claramente no lo sean.
# 3. Simula dos variables Poisson independientes con distintas lambda y
#    verifica que su correlación muestral es cercana a 0.
