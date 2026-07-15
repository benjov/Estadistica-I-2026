# ============================================================================
# Estadística I - CIDE
# Tema 5: Convergencia de variables aleatorias
# 5.b) La Ley de los Grandes Números
# ============================================================================
#
# La Ley de los Grandes Números (LGN) dice que, si X_1, ..., X_n son IID con
# E[X_i] = mu, entonces la media muestral X_bar_n converge (en probabilidad,
# LGN débil) a la constante mu conforme n -> Inf.
#
# Es la justificación formal de por qué "promediar muchas observaciones" es
# una buena forma de estimar una media poblacional, y por qué la frecuencia
# relativa se acerca a la probabilidad teórica (como vimos en el tema 1).

# ----------------------------------------------------------------------------
# 1. Ilustración con dados: la media muestral converge a E[X] = 3.5
# ----------------------------------------------------------------------------

set.seed(2026)
n_max <- 5000
lanzamientos <- sample(1:6, size = n_max, replace = TRUE)
media_acumulada <- cumsum(lanzamientos) / seq_len(n_max)

plot(media_acumulada, type = "l", col = "steelblue", lwd = 1.5,
     xlab = "Número de lanzamientos (n)", ylab = "Media acumulada",
     main = "Ley de los Grandes Números: media de n lanzamientos de un dado")
abline(h = 3.5, col = "firebrick", lty = 2, lwd = 2)
legend("topright", legend = c("Media muestral acumulada", "E[X] = 3.5"),
       col = c("steelblue", "firebrick"), lty = c(1, 2), lwd = 2)

# ----------------------------------------------------------------------------
# 2. Varias trayectorias simultáneas: todas convergen al mismo valor, pero
#    con distinta "ruta"
# ----------------------------------------------------------------------------

set.seed(2026)
n_trayectorias <- 8
plot(NULL, xlim = c(1, n_max), ylim = c(2, 5),
     xlab = "n", ylab = "Media acumulada",
     main = "Varias trayectorias de la media muestral (dado)")
abline(h = 3.5, col = "black", lty = 2, lwd = 2)
for (i in 1:n_trayectorias) {
  lanz_i <- sample(1:6, size = n_max, replace = TRUE)
  media_i <- cumsum(lanz_i) / seq_len(n_max)
  lines(media_i, col = rgb(0, 0, 1, 0.4))
}

# ----------------------------------------------------------------------------
# 3. La LGN con distribuciones de "cola pesada": cuidado, no siempre
#    converge rápido
# ----------------------------------------------------------------------------
# Con una t de Student de pocos grados de libertad, la convergencia de la
# media muestral es mucho más errática (aunque sigue cumpliéndose la LGN si
# la media poblacional existe, lo que requiere df > 1).

set.seed(2026)
df_t <- 3  # con df > 1 la media poblacional de la t existe y es 0
muestra_t <- rt(n_max, df = df_t)
media_acumulada_t <- cumsum(muestra_t) / seq_len(n_max)

plot(media_acumulada_t, type = "l", col = "darkorange",
     xlab = "n", ylab = "Media acumulada",
     main = paste("Media acumulada de una t-Student(df =", df_t, ")"))
abline(h = 0, col = "firebrick", lty = 2, lwd = 2)

# ----------------------------------------------------------------------------
# 4. Aplicación económica: estimar una proporción poblacional por simulación
# ----------------------------------------------------------------------------
# La LGN también justifica el método de Monte Carlo: podemos aproximar
# probabilidades/esperanzas difíciles de calcular analíticamente, simulando
# muchas repeticiones y promediando.

# Ejemplo: estimar P(ganancia de un portafolio > 0) cuando el rendimiento
# combina dos activos con rendimientos correlacionados (sin fórmula cerrada
# sencilla para la distribución de la suma).

set.seed(2026)
n_sim <- 100000
rendimiento_A <- rnorm(n_sim, mean = 0.02, sd = 0.10)
rendimiento_B <- 0.5 * rendimiento_A + rnorm(n_sim, mean = 0.01, sd = 0.08)
portafolio <- 0.6 * rendimiento_A + 0.4 * rendimiento_B

mean(portafolio > 0)  # estimación Monte Carlo de P(ganancia > 0)

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Repite la gráfica de media acumulada, pero con una moneda sesgada
#    (P(águila) = 0.7, valores 0/1) y verifica que converge a 0.7.
# 2. Estima por Monte Carlo la probabilidad de que la suma de 4 dados sea
#    mayor a 18, y compárala (si puedes) contra el cálculo exacto usando
#    combinatoria del tema 1.
# 3. Explica en tus palabras por qué la trayectoria de la t-Student(df=3)
#    se ve "más errática" que la del dado, a pesar de que ambas cumplen la LGN.
