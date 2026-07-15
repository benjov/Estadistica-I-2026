# ============================================================================
# Estadística I - CIDE
# Tema 1: Probabilidad
# 1.a) Espacios muestrales y eventos
# ============================================================================
#
# Un experimento aleatorio es cualquier proceso cuyo resultado no se puede
# predecir con certeza. El espacio muestral (Omega) es el conjunto de todos
# los resultados posibles, y un evento es cualquier subconjunto de Omega.
#
# En R podemos representar espacios muestrales con vectores, listas o
# data frames, según la complejidad del experimento.

# ----------------------------------------------------------------------------
# 1. Espacio muestral de un experimento simple: lanzar un dado
# ----------------------------------------------------------------------------

omega_dado <- 1:6
omega_dado

# Evento A: "cae un número par"
A <- c(2, 4, 6)

# Evento B: "cae un número mayor a 4"
B <- c(5, 6)

# ¿A y B son subconjuntos de Omega?
all(A %in% omega_dado)
all(B %in% omega_dado)

# ----------------------------------------------------------------------------
# 2. Operaciones entre eventos (álgebra de conjuntos)
# ----------------------------------------------------------------------------

# Unión: A o B ocurren
union(A, B)

# Intersección: A y B ocurren simultáneamente
intersect(A, B)

# Complemento de A respecto a Omega: A no ocurre
complemento_A <- setdiff(omega_dado, A)
complemento_A

# Diferencia: elementos de A que no están en B
setdiff(A, B)

# ¿Son A y B mutuamente excluyentes (disjuntos)?
length(intersect(A, B)) == 0

# ----------------------------------------------------------------------------
# 3. Espacio muestral de un experimento compuesto: lanzar dos dados
# ----------------------------------------------------------------------------
# Cuando el experimento tiene varias etapas, expand.grid() construye el
# producto cartesiano de los resultados posibles de cada etapa.

omega_2dados <- expand.grid(dado1 = 1:6, dado2 = 1:6)
head(omega_2dados)
nrow(omega_2dados)  # |Omega| = 6 x 6 = 36 resultados equiprobables

# Evento C: "la suma de los dados es 7"
omega_2dados$suma <- omega_2dados$dado1 + omega_2dados$dado2
C <- omega_2dados[omega_2dados$suma == 7, ]
C

# Evento D: "ambos dados muestran el mismo número" (una pareja)
D <- omega_2dados[omega_2dados$dado1 == omega_2dados$dado2, ]
D

# ----------------------------------------------------------------------------
# 4. Diagramas de Venn (representación gráfica de eventos)
# ----------------------------------------------------------------------------
# Base R no trae diagramas de Venn, pero podemos representar dos círculos con
# symbols() para ilustrar la idea de unión e intersección.

plot.new()
plot.window(xlim = c(0, 10), ylim = c(0, 10))
symbols(x = c(4, 6), y = c(5, 5), circles = c(2.2, 2.2), add = TRUE,
        inches = FALSE, fg = c("steelblue", "firebrick"), lwd = 2)
text(x = c(3, 7), y = 8.2, labels = c("A", "B"), cex = 1.3)
title(main = "Diagrama de Venn: eventos A y B")

# ----------------------------------------------------------------------------
# Para practicar
# ----------------------------------------------------------------------------
# 1. Define el espacio muestral de lanzar una moneda tres veces (usa
#    expand.grid con valores "A" (águila) y "S" (sol)).
# 2. Define el evento "al menos dos águilas" y cuenta cuántos resultados
#    contiene.
# 3. Con el experimento de los dos dados, encuentra el evento "la suma es
#    un número par" y verifica si es o no mutuamente excluyente con el
#    evento D (parejas) definido arriba.
