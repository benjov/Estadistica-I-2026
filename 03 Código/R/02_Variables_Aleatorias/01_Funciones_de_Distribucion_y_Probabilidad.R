# ============================================================================
# Estadística I - CIDE
# Tema 2: Variables Aleatorias
# 2.a) Funciones de distribución y de probabilidad
# ============================================================================
#
# Una variable aleatoria (VA) es una función que asigna un número real a
# cada resultado del espacio muestral: X: Omega -> R.
#
# La función de distribución acumulada (FDA), F(x) = P(X <= x), está
# definida para cualquier VA (discreta o continua) y determina por completo
# su comportamiento probabilístico.
#
# En R, para cada familia de distribuciones "nombre" existen 4 funciones:
#   d<nombre>(x, ...)  densidad o masa de probabilidad en x
#   p<nombre>(q, ...)  FDA: P(X <= q)
#   q<nombre>(p, ...)  función cuantil (inversa de la FDA)
#   r<nombre>(n, ...)  genera n observaciones aleatorias

# ----------------------------------------------------------------------------
# 1. Una VA discreta simple: la suma de dos dados
# ----------------------------------------------------------------------------

omega <- expand.grid(dado1 = 1:6, dado2 = 1:6)
X <- omega$dado1 + omega$dado2  # variable aleatoria: la suma

# Función de masa de probabilidad (fmp): P(X = x) para cada valor posible
fmp <- table(X) / length(X)
fmp

barplot(fmp, col = "steelblue",
        main = "Función de masa de probabilidad de X = suma de dos dados",
        xlab = "x", ylab = "P(X = x)")

# ----------------------------------------------------------------------------
# 2. Función de distribución acumulada (FDA) de X
# ----------------------------------------------------------------------------

valores_x <- as.numeric(names(fmp))
fda <- cumsum(fmp)
fda

plot(valores_x, fda, type = "s", lwd = 2, col = "firebrick",
     xlab = "x", ylab = "F(x) = P(X <= x)",
     main = "Función de distribución acumulada de X")
points(valores_x, fda, pch = 19, col = "firebrick")

# Propiedades de F(x) que se pueden verificar aquí:
#  - Es no decreciente
all(diff(fda) >= 0)
#  - F(x) -> 1 cuando x -> valor máximo
max(fda)
#  - Es una función escalonada (VA discreta): salta en cada valor posible de X

# ----------------------------------------------------------------------------
# 3. De la FDA a probabilidades de eventos
# ----------------------------------------------------------------------------
# P(X <= 5): usamos la FDA directamente
fda["5"]

# P(X > 8) = 1 - F(8)
1 - fda["8"]

# P(3 <= X <= 6) = F(6) - F(2)
fda["6"] - fda["2"]

# ----------------------------------------------------------------------------
# 4. Vista previa: una VA continua (se profundiza en el script 03)
# ----------------------------------------------------------------------------
# Para una VA continua, la FDA es una función suave (no escalonada) y la
# probabilidad de cualquier valor puntual es cero: P(X = x) = 0.
# Ejemplo: X ~ Uniforme(0, 1)

curve(punif(x, min = 0, max = 1), from = -0.2, to = 1.2, lwd = 2,
      col = "darkgreen", ylab = "F(x)", xlab = "x",
      main = "FDA de una variable continua: Uniforme(0, 1)")

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Define la VA "número de águilas en 3 lanzamientos de moneda" a partir
#    de su espacio muestral con expand.grid(), y obtén su fmp y su FDA.
# 2. Con la FDA de la suma de dos dados, calcula P(X = 7 o X = 11) usando
#    solamente la fda (sin recalcular la fmp).
# 3. Verifica en R que F(x) definida arriba cumple F(-Inf) = 0, en el
#    sentido de que F(x) = 0 para cualquier x menor al valor mínimo de X.
