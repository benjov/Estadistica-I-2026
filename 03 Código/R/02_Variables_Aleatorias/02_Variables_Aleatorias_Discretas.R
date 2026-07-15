# ============================================================================
# Estadística I - CIDE
# Tema 2: Variables Aleatorias
# 2.b) Algunas funciones de variables aleatorias discretas
# ============================================================================
#
# Revisamos las familias de VA discretas más usadas en economía y ciencias
# sociales: Bernoulli, Binomial, Geométrica, Binomial Negativa, Hipergeométrica
# y Poisson. Para cada una usamos las funciones d/p/q/r de R.

# ----------------------------------------------------------------------------
# 1. Bernoulli: un solo ensayo con éxito/fracaso
# ----------------------------------------------------------------------------
# X ~ Bernoulli(p): P(X=1) = p, P(X=0) = 1-p
# R no tiene funciones dedicadas; es un caso particular de Binomial(n=1, p)

p_exito <- 0.3
dbinom(x = c(0, 1), size = 1, prob = p_exito)  # P(X=0), P(X=1)

# ----------------------------------------------------------------------------
# 2. Binomial: número de éxitos en n ensayos independientes tipo Bernoulli
# ----------------------------------------------------------------------------
# Ejemplo: de 20 empresas encuestadas, cada una exporta con probabilidad 0.4,
# de forma independiente. X = número de empresas exportadoras.

n <- 20; p <- 0.4

dbinom(x = 8, size = n, prob = p)          # P(X = 8)
pbinom(q = 8, size = n, prob = p)          # P(X <= 8)
1 - pbinom(q = 8, size = n, prob = p)      # P(X > 8)
qbinom(p = 0.5, size = n, prob = p)        # mediana de X

set.seed(2026)
muestra_binom <- rbinom(n = 10000, size = n, prob = p)
barplot(table(muestra_binom) / length(muestra_binom), col = "steelblue",
        main = "Binomial(n=20, p=0.4): simulación vs. teoría",
        xlab = "x", ylab = "Frecuencia relativa")
points(x = seq_len(n + 1) - 0.5, y = dbinom(0:n, n, p), col = "firebrick",
       pch = 19, type = "b")
legend("topright", legend = c("Simulación", "Teórica"),
       fill = c("steelblue", NA), border = c("black", NA),
       col = c(NA, "firebrick"), pch = c(NA, 19), lty = c(NA, 1))

# ----------------------------------------------------------------------------
# 3. Geométrica: número de ensayos hasta el primer éxito
# ----------------------------------------------------------------------------
# Ejemplo: probabilidad de que un cliente acepte una oferta es 0.15 en cada
# llamada, independiente entre llamadas. X = número de llamadas ADICIONALES
# antes del primer éxito (parametrización de R: soporte en 0, 1, 2, ...)

p_aceptar <- 0.15
dgeom(x = 0:5, prob = p_aceptar)     # P(X = 0), ..., P(X = 5)
pgeom(q = 4, prob = p_aceptar)       # P(X <= 4), es decir, éxito en <= 5 llamadas
mean(rgeom(100000, prob = p_aceptar))  # media simulada, compárese con (1-p)/p
(1 - p_aceptar) / p_aceptar            # media teórica

# ----------------------------------------------------------------------------
# 4. Binomial negativa: número de fracasos antes del r-ésimo éxito
# ----------------------------------------------------------------------------
# Ejemplo: número de rechazos antes de conseguir 3 clientes que acepten.
r_exitos <- 3
dnbinom(x = 0:10, size = r_exitos, prob = p_aceptar)
mean(rnbinom(100000, size = r_exitos, prob = p_aceptar))
r_exitos * (1 - p_aceptar) / p_aceptar  # media teórica

# ----------------------------------------------------------------------------
# 5. Hipergeométrica: muestreo SIN reemplazo de una población finita
# ----------------------------------------------------------------------------
# Urna/población con m = 15 "éxitos" y n = 25 "fracasos" (total 40). Se
# extrae una muestra de k = 10 sin reemplazo. X = número de éxitos.

m <- 15; nn <- 25; k <- 10
dhyper(x = 0:k, m = m, n = nn, k = k)
phyper(q = 4, m = m, n = nn, k = k)   # P(X <= 4)

# Comparación con la Binomial (aproximación válida si la población es
# grande respecto a la muestra, lo cual NO es el caso aquí):
plot(0:k, dhyper(0:k, m, nn, k), type = "b", col = "steelblue", pch = 19,
     xlab = "x", ylab = "P(X = x)", main = "Hipergeométrica vs. Binomial")
lines(0:k, dbinom(0:k, size = k, prob = m / (m + nn)), type = "b",
      col = "firebrick", pch = 17)
legend("topright", legend = c("Hipergeométrica", "Binomial (aprox.)"),
       col = c("steelblue", "firebrick"), pch = c(19, 17))

# ----------------------------------------------------------------------------
# 6. Poisson: número de eventos raros en un intervalo de tiempo/espacio
# ----------------------------------------------------------------------------
# Ejemplo: una sucursal bancaria recibe en promedio lambda = 4 reclamos por
# día. X = número de reclamos en un día cualquiera.

lambda <- 4
dpois(x = 0:10, lambda = lambda)
ppois(q = 6, lambda = lambda)          # P(X <= 6)
1 - ppois(q = 6, lambda = lambda)      # P(X > 6)

set.seed(2026)
muestra_pois <- rpois(10000, lambda = lambda)
mean(muestra_pois); var(muestra_pois)  # en Poisson, media y varianza teóricas son iguales a lambda

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Si el 60% de los hogares de una colonia tiene internet de fibra óptica,
#    y se encuestan 15 hogares al azar, calcula P(exactamente 10 tienen
#    fibra) y P(al menos 12 tienen fibra) usando la Binomial.
# 2. Un call center recibe en promedio 3 llamadas por minuto (Poisson).
#    Calcula la probabilidad de recibir más de 5 llamadas en un minuto dado.
# 3. Compara con una gráfica la Binomial(n=50, p=0.05) contra una
#    Poisson(lambda=2.5) (aproximación de Poisson a Binomial cuando n es
#    grande y p es chica).
