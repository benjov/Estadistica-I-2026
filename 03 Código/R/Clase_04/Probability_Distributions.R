# 
# Probability Distributions
#
#****************************************************************************
# Import necessary libraries


#****************************************************************************
#
# Una distribución de probabilidad es una forma de representar la probabilidad de un 
# evento o el valor de un parámetro y es fundamental para la teoría estadística. 
# Algunas de las distribuciones más utilizadas se resumen en la Siguiente Tabla, 
# junto con los sufijos de las funciones en R que corresponden a cada distribución.
#

# Crear un dataframe con los datos
tabla_distribuciones <- data.frame( 
  Tipo = c( "Continua", "Continua", "Continua", "Continua", 
            "Discreta", "Discreta", "Discreta" ),
  Distribucion = c( "Normal", "Lognormal", "Uniforme", "Beta", 
                    "Binomial", "Multinomial", "Poisson" ),
  Uso_comun = c( "Modela la frecuencia relativa de resultados que son simétricos alrededor de una media, pueden ser negativos",
                 "Modela la frecuencia relativa de resultados que están distribuidos de forma logarítmica",
                 "Modela valores que están entre dos extremos y que ocurren con la misma frecuencia",
                 "Modela valores que están entre 0 y 1",
                 "Modela el número de éxitos de un número dado de ensayos cuando solo hay dos posibles resultados y todos los ensayos tienen la misma probabilidad de éxito",
                 "Lo mismo que la distribución binomial, pero cuando hay más de dos posibles resultados",
                 "Usado para datos de conteo en casos donde la varianza y la media son aproximadamente iguales" ),
  Sufijo_R = c( "norm()", "lnorm()", "unif()", "beta()", "binom()", "multinom()", "pois()" )
)

# Ver la tabla de distribuciones

View(tabla_distribuciones)

# En R, hay cuatro formas diferentes de utilizar cada una de estas funciones de distribución 
# (cada una tiene un prefijo diferente):
# 
# La función de densidad (o masa) de probabilidad (d-)
# La función de densidad acumulativa o función de distribución (p-)
# 

#****************************************************************************
# Suponga que X se distribuye Normal con media 500 y desviación estándar 50.

# parameters
mu = 500; sig = 50

?seq 

# a sequence of possible random variables
Sample = seq(200, 700, length = 100)

# use the two functions
densty = dnorm(x = Sample, mean = mu, sd = sig)  # takes specific lengths
cuprob = pnorm(q = Sample, mean = mu, sd = sig)  # takes specific lengths

# Create a dataframe 
data_sample <- data.frame( Sample = Sample, 
                           densty = densty, 
                           cuprob = cuprob )

# set up plotting region: see ?par for more details
# notice the tricks to clean up the plot
par(
  mfrow = c(1,2),    # set up 1x2 regions
  mar = c(3,3,3,1),  # set narrower margins
  xaxs = "i",        # remove "x-buffer"
  yaxs = "i",        # remove "y-buffer"
  mgp = c(2,0.4,0),  # bring in axis titles ([1]) and tick labels ([2])
  tcl = -0.25        # shorten tick marks
)

plot(densty ~ Sample, type = "l", lwd = 3, main = "dnorm()", #data = data_sample
     xlab = "X", ylab = "Density", las = 1,
     yaxt = "n") # turns off y-axis
axis(side = 2, at = c(0.002, 0.006), labels = c(0.002, 0.006), las = 2)
plot(cuprob ~ Sample, type = "l", lwd = 3, main = "pnorm()",
     xlab = "X", ylab = "Cumulative Probability", las = 1)

box() # add borders to the histogram

par(
  mfrow = c(1,1)  # 1 X 1 
)

#****************************************************************************
# Parámetros para la distribución binomial
n <- 100  # número de ensayos
p <- 0.5  # probabilidad de éxito en cada ensayo

# Crear una secuencia de posibles valores de variables aleatorias (número de éxitos)
Sample_B <- 0:n  # Desde 0 éxitos hasta n éxitos

# Utilizar las funciones de masa y distribución acumulativa
densidad <- dbinom(x = Sample_B, size = n, prob = p)  # Función de masa de probabilidad
prob_acum <- pbinom(q = Sample_B, size = n, prob = p)  # Función de distribución acumulativa

# Crear un dataframe para almacenar los resultados
data_Sample_B <- data.frame(
  Sample_B = Sample_B,
  densidad = densidad,
  prob_acum = prob_acum
)

# Configurar el área de trazado: consultar `?par` para más detalles
par(
  mfrow = c(1, 2),    # Configurar 1 fila x 2 columnas
  mar = c(3, 3, 3, 1), # Establecer márgenes más estrechos
  xaxs = "i",          # Eliminar el "buffer" en x
  yaxs = "i",          # Eliminar el "buffer" en y
  mgp = c(2, 0.4, 0),  # Ajustar la ubicación de los títulos y etiquetas del eje
  tcl = -0.25          # Acortar las marcas de graduación
)

# Graficar la densidad
plot(densidad ~ Sample_B, data = data_Sample_B, type = "h", lwd = 3, 
     main = "dbinom()", xlab = "Número de Éxitos", ylab = "Probabilidad", las = 1)

# Graficar la probabilidad acumulativa
plot(prob_acum ~ Sample_B, data = data_Sample_B, type = "s", lwd = 3, 
     main = "pbinom()", xlab = "Número de Éxitos", ylab = "Probabilidad Acumulativa", las = 1)

# Añadir bordes a los histogramas
box()

# Restaurar la configuración de 'par' para futuros gráficos
par(mfrow = c(1, 1))


