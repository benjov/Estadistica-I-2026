# ============================================================================
# Estadística I - CIDE
# Tema 1: Probabilidad
# 1.c) Definición de probabilidad
# ============================================================================
#
# Revisamos dos formas de llegar a la probabilidad de un evento:
#   1. La regla de Laplace (probabilidad clásica), válida en espacios
#      equiprobables: P(A) = |A| / |Omega|.
#   2. La interpretación frecuentista: P(A) es el límite al que converge la
#      frecuencia relativa de A al repetir el experimento muchas veces.
# Ambas deben ser consistentes con los axiomas de Kolmogorov:
#   P(A) >= 0 ; P(Omega) = 1 ; si A y B son disjuntos, P(A U B) = P(A) + P(B)

# ----------------------------------------------------------------------------
# 1. Regla de Laplace en un espacio equiprobable
# ----------------------------------------------------------------------------

omega_dado <- 1:6
A <- c(2, 4, 6)  # "cae un número par"

P_laplace <- function(evento, omega) length(evento) / length(omega)
P_laplace(A, omega_dado)

# Con dos dados (36 resultados equiprobables)
omega_2dados <- expand.grid(dado1 = 1:6, dado2 = 1:6)
omega_2dados$suma <- omega_2dados$dado1 + omega_2dados$dado2

evento_suma7 <- which(omega_2dados$suma == 7)
length(evento_suma7) / nrow(omega_2dados)

# ----------------------------------------------------------------------------
# 2. Verificación de los axiomas
# ----------------------------------------------------------------------------

par   <- c(2, 4, 6)
impar <- c(1, 3, 5)

P_laplace(par, omega_dado) + P_laplace(impar, omega_dado)   # P(par) + P(impar)
P_laplace(union(par, impar), omega_dado)                     # P(par U impar)
# Como par e impar son disjuntos, ambas cantidades deben coincidir y sumar 1.

# ----------------------------------------------------------------------------
# 3. Interpretación frecuentista: simulación
# ----------------------------------------------------------------------------
# Si el modelo teórico es correcto, al repetir el experimento muchas veces
# la frecuencia relativa del evento debe acercarse a la probabilidad teórica.
# (Esta idea es, de hecho, un caso particular de la Ley de los Grandes
# Números que veremos en el tema 4.)

set.seed(2026)
n_lanzamientos <- c(10, 100, 1000, 10000, 100000)

frecuencia_relativa <- sapply(n_lanzamientos, function(n) {
  lanzamientos <- sample(omega_dado, size = n, replace = TRUE)
  mean(lanzamientos %in% A)  # proporción de veces que cae par
})

data.frame(n = n_lanzamientos, frecuencia_relativa = frecuencia_relativa,
           probabilidad_teorica = P_laplace(A, omega_dado))

plot(n_lanzamientos, frecuencia_relativa, log = "x", type = "b", pch = 19,
     col = "steelblue", ylim = c(0, 1),
     xlab = "Número de lanzamientos (escala log)",
     ylab = "Frecuencia relativa de 'número par'",
     main = "Convergencia de la frecuencia relativa a P(A) = 0.5")
abline(h = P_laplace(A, omega_dado), col = "firebrick", lty = 2, lwd = 2)
legend("topright", legend = c("Frecuencia relativa", "P(A) teórica"),
       col = c("steelblue", "firebrick"), pch = c(19, NA), lty = c(NA, 2))

# ----------------------------------------------------------------------------
# 4. Un espacio NO equiprobable: cuidado con aplicar Laplace sin pensar
# ----------------------------------------------------------------------------
# La suma de dos dados puede ir de 2 a 12, pero esos 11 resultados NO son
# equiprobables (hay más formas de obtener un 7 que un 2 o un 12).

tabla_sumas <- table(omega_2dados$suma) / nrow(omega_2dados)
barplot(tabla_sumas, col = "darkseagreen",
        main = "Distribución de la suma de dos dados",
        xlab = "Suma", ylab = "Probabilidad")

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Usando la regla de Laplace, calcula P(la suma de dos dados es mayor a 9).
# 2. Simula 50,000 lanzamientos de dos dados y compara la frecuencia
#    relativa de "la suma es 7" contra la probabilidad teórica.
# 3. Explica por qué la regla de Laplace no se puede aplicar directamente al
#    espacio muestral {2, 3, ..., 12} de las sumas, y sí al espacio de los
#    36 pares (dado1, dado2).
