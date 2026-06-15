# ----------------------------------------------
# Primera sesión: Introducción y ejemplos básicos
# ----------------------------------------------

# 1. Estimación del valor de pi
set.seed(123)  # Fijamos una semilla para replicabilidad

n <- 10000  # Número de simulaciones

x <- runif(n, min = -1, max = 1)  # Valores uniformes en [-1, 1]

y <- runif(n, min = -1, max = 1)  # Valores uniformes en [-1, 1]

inside_circle <- x^2 + y^2 <= 1  # Puntos dentro del círculo

pi_estimation <- 4 * mean(inside_circle)  # Estimación de pi

cat("Estimación de pi:", pi_estimation, "\n")

# Visualización
plot( x, y, col = ifelse(inside_circle, "blue", "red"), pch = 20, 
      main = "Estimación de pi usando Monte Carlo")


# 2. Estimación de la media poblacional (distribución no normal)

set.seed(456)

n <- 1000

samples <- rexp(n, rate = 0.5)  # Muestra de una distribución exponencial

true_mean <- 1 / 0.5  # Media teórica

estimated_mean <- mean(samples)

cat("Media teórica:", true_mean, "\n")

cat("Media estimada:", estimated_mean, "\n")

# Visualización
hist( samples, breaks = 30, 
      main = "Distribución de la muestra", col = "lightblue",
      xlab = "Valores", ylab = "Frecuencia")
abline(v = true_mean, col = "red", lwd = 2, lty = 2)

# ----------------------------------------------
# Segunda sesión: Problemas avanzados
# ----------------------------------------------

# 3. Valoración de una opción financiera (Fórmula Black-Scholes)
set.seed(789)

simulate_option_price <- function(S0, K, r, sigma, T, n) {
  # Simula precios usando el modelo Black-Scholes
  Z <- rnorm(n)
  
  ST <- S0 * exp((r - 0.5 * sigma^2) * T + sigma * sqrt(T) * Z)
  
  payoff <- pmax(ST - K, 0)
  
  option_price <- exp(-r * T) * mean(payoff)
  
  return(option_price)
}

# Parámetros de ejemplo
S0 <- 100  # Precio inicial del activo
K <- 110   # Precio de ejercicio
r <- 0.05  # Tasa libre de riesgo
sigma <- 0.2  # Volatilidad
T <- 1     # Tiempo hasta el vencimiento (1 año)
n <- 10000 # Número de simulaciones

option_price <- simulate_option_price(S0, K, r, sigma, T, n)
cat("Precio estimado de la opción:", option_price, "\n")

# 4. Simulación de una serie de tiempo económica
set.seed(321)

simulate_gbm <- function(S0, mu, sigma, T, steps) {
  dt <- T / steps
  W <- cumsum(c(0, rnorm(steps, mean = 0, sd = sqrt(dt))))
  t <- seq(0, T, length.out = steps + 1)
  S <- S0 * exp((mu - 0.5 * sigma^2) * t + sigma * W)
  return(data.frame(Time = t, Price = S))
}

# Parámetros
S0 <- 100  # Valor inicial
mu <- 0.05  # Tasa de crecimiento
sigma <- 0.2  # Volatilidad
T <- 1  # Tiempo total (1 año)
steps <- 250  # Pasos (días de mercado)

gbm <- simulate_gbm(S0, mu, sigma, T, steps)
plot(gbm$Time, gbm$Price, type = "l", col = "blue", lwd = 2,
     main = "Simulación de una serie de tiempo (GBM)",
     xlab = "Tiempo", ylab = "Precio")

# ----------------------------------------------
# Simulación: Convergencia hacia la normalidad
# ----------------------------------------------

set.seed(123)  # Fijamos una semilla para reproducibilidad

# Función para simular la distribución de las medias muestrales
simulate_sample_means <- function(n, reps, min = 0, max = 1) {
  # Generar 'reps' medias muestrales a partir de una distribución uniforme
  sample_means <- replicate(reps, mean(runif(n, min = min, max = max)))
  return(sample_means)
}

# Parámetros de la simulación
sample_sizes <- c(10, 50, 100, 500)  # Tamaños muestrales
reps <- 1000  # Número de repeticiones por tamaño muestral

# Crear un gráfico para ilustrar las distribuciones
par(mfrow = c(2, 2))  # Dividir la ventana gráfica en 2x2

for (n in sample_sizes) {
  # Simular las medias muestrales
  sample_means <- simulate_sample_means(n, reps)
  
  # Graficar el histograma de las medias muestrales
  hist(sample_means, breaks = 30, probability = TRUE, col = "lightblue",
       main = paste("Tamaño muestral:", n),
       xlab = "Media muestral", ylab = "Densidad")
  
  # Añadir la densidad teórica de la normal correspondiente
  theoretical_mean <- (0 + 1) / 2  # Media de la distribución uniforme
  theoretical_sd <- sqrt(((1 - 0)^2) / 12 / n)  # Desviación estándar de la media
  curve(dnorm(x, mean = theoretical_mean, sd = theoretical_sd), 
        col = "red", lwd = 2, add = TRUE)
}

# Restaurar la configuración gráfica
par(mfrow = c(1, 1))

# ----------------------------------------------
