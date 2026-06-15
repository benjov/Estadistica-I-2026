# Estadistica I, Otoño de 2025
# Primeros Códigos. Introduccion

#****************************************************************************************
# Reglas importantes:
#  1. R es case sensitive, lo que significa que distingue entre minusculas y mayusculas
#  2. Todo lo que se escribe despues del simbolo de "#", R lo considera un comentario
#  3. Para ejecutar una linea de comando se utiliza "Control + Enter"
#  4. Para limpiar la Consola se utiliza "Control + L"
#  5. Utilizar ";" funciona para realizar más de una tarea en la misma línea de código
#  6. En R existen un conjunto de palabras reservadas, como lo son: break, for, function, 
#     if, in, next, repeat, return, while, True, False, y algunas otras
#  
#****************************************************************************************

## Clases de objetos

class("a")

class("R">"Python"); "R" > "Python"; 5 < 2

class(2); class(as.integer(2)); class("2")

#Asignacion e impresion de objetos 

ObjetoGuardado <- c(1,2,3,5,7,11,13,17,10)

print(ObjetoGuardado)

ObjetoGuardado

# Operaciones: +, -, *, /, ^, log, exp, ...

ObjetoGuardado + 5

ObjetoGuardado*5

ObjetoGuardado**(1/2)

log(ObjetoGuardado)

exp(ObjetoGuardado)

round(exp(ObjetoGuardado), 2)

## Clases de objetos (Vectores y listas)

# Para revisar cuantos elementos hemos escrito:

ls() # "ls" lista todos los elementos contenidos en Global Enviroment

# ## Clases de objetos (Vectores y listas)

class(ObjetoGuardado)

I <- c(1, 1, 1, 1, 1, 1, 1, 1, 1)

ObjetoGuardado %*% I # Producto punto entre vectores

ListaGuardada <- list(c("Omar", "Jose"), c(24,29), c(TRUE, FALSE))

ListaGuardada

class(ListaGuardada)

## Clases de objetos (Matrix y DataFrame)

M_object <-matrix(ObjetoGuardado, nrow = 3, ncol = 3) #Generamos una matriz a partir del vector

M_object

diagonal <- diag(3) #Generamos una matríz identidad de 3*3 

diagonal

M_object%*%diagonal #

t(M_object)%*%diagonal #

solve(M_object) #Matriz inversa

M_object%*%solve(M_object)

miData = data.frame(ListaGuardada) #Generamos un DataFrame a partir del objeto clase lista

miData #Imprimimos

names(miData) = c("Nombre", "Edad", "PEA")#Le damos nombre a las columnas 

miData #Imprimimos

### Seleccion de elementos en vectores y matrices

ObjetoGuardado[9]

ObjetoGuardado[c(8,9)]

ObjetoGuardado[4:9]

M_object[9]

M_object[3:5]

M_object[ , 3]

miData['Nombre']

miData[2]

miData[1:2]

miData[2, 'Nombre']

miData$Edad

## Clases de objetos (Funciones)

### Estadisticas, suma, media, desviacion estandar y muestreo:

length(ObjetoGuardado)

sum(ObjetoGuardado)

mean(ObjetoGuardado)

sd(ObjetoGuardado)

###Las funciones necesitan ARGUMENTOS, en el caos siguiente el argumento es el tamaño de la muestra
?sample

sample(ObjetoGuardado, 3, replace = FALSE) 

sample(ObjetoGuardado, 10, replace = TRUE) 

###Para conocer los argumentos de una funcion, se puede utilizar args()
args(sample)

###Funciones creadas
m_podada <- function(x, n){
  # Funcion de la media podada
  N = length(x)
  #x = sort(x)
  x = x[ (n+1) : (N-n) ]
  
  sum(x) / length(x)
}

###probar funcion
m_podada(sort(ObjetoGuardado),2)

