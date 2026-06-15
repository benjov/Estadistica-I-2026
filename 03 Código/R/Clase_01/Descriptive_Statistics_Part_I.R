# 
# Descriptive Statistics
#
#****************************************************************************
# Import necessary libraries
#install.packages("tidyverse")
#install.packages("hrbrthemes")
#install.packages("data.table")
#install.packages("readxl")

library(tidyverse) # The tidyverse is an collection of R packages. 
library(ggplot2) #
library(hrbrthemes)
library(data.table)
library(readxl)

#****************************************************************************

# Load the dataset
url <- "https://www.cre.gob.mx/da/Precios_promedio_diarios_y_mensuales_en_estaciones_de_servicio.xlsx"

nombre_archivo <- "Precios_PROMEDIO_CRE.xlsx"

# Descargar y guardar el archivo en el directorio de trabajo
download.file(url, destfile = nombre_archivo, mode = "wb")

# Leer la hoja "Cuadro 1.1", omitiendo las primeras 3 filas
data <- read_excel(nombre_archivo, sheet = "Cuadro 1.1", skip = 3)

# Show the first few rows of the dataframe
head(data)

#
tail(data)

# Display the structure of the dataframe
str(data)

# Get the dimensions of the dataframe
dim(data)

# Get the names of the dataframe
names(data)

# Select columns
data_monthly <- data[ , c( "Fecha", "Gasolina Regular", "Gasolina Premium", "Diésel" ) ]

# Assign new names
names(data_monthly) <- c( "Fecha", "Gasolina.Regular", "Gasolina.Premium", "Diesel" )

# Check for missing values
any(is.na(data_monthly))

sum(is.na(data_monthly))

colSums(is.na(data_monthly))

# Filter rows that have at least one NA value
data_monthly_na <- data_monthly[!complete.cases(data_monthly), ]

# Delete rows with NA values
data_monthly_clean <- na.omit(data_monthly)

# Get the statistical summary of the dataframe
summary(data_monthly_clean)

# Plot a histogram of Monthly Gas Price
ggplot( data_monthly_clean, aes(x = Gasolina.Regular) ) + 
    geom_histogram( fill = "darkblue", color = "black" ) + 
    labs( title = "Distribution of Monthly Regular Gas Price", 
          x = "Regular Gas Price", y = "Frequency")

# Visit: https://www.data-to-viz.com/
# Plot a scatter plot of Monthly Household Expense vs Income

#
# Convertir el dataframe a formato long
data_monthly_long <- melt( setDT(data_monthly_clean), 
                           id.vars = c( "Fecha" ), 
                           variable.name = "Gas.Type",
                           value.name = "Prices" )

# Plot a histogram
ggplot( data_monthly_long, aes(x=Prices, fill=Gas.Type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual( values=c("#69b3a2", "#404080", "#CC243C" )) +
  theme_ipsum() + 
  labs( title = "Distribution of Monthly Gas Prices", 
        x = "Gas Price", y = "Frequency")
#
ggsave("/Users/benjamin/Documents/Personal/Cursos_CIDE/Estadistica_I_2024/Estadistica_I_2024/03 Código/R/Clase_01/Gas_Prices.png",
       width = 20, height = 15, units = "cm")


# Save the modified dataframe
write.csv( data_monthly_clean, 
           "/Users/benjamin/Documents/Personal/Cursos_CIDE/Estadistica_I_2024/Estadistica_I_2024/03 Código/R/Clase_01/Gas_Prices.csv",
           row.names = FALSE)

# Tarea:
# 1. Seleccione 3 estados (uno del norte del país, otro del centro y uno más del sur)
# 2. Realice un histograma donde incluya los 3 estados para cada uno de los tipos de combustible
# 3. Guarde sus gráficos (3)
# 4. Enviar código y resultado por correo
# 5. Fecha de entrega: 24 de septiembre de 2025

# Hint:
# Select columns

# Load the dataset
url <- "https://www.cre.gob.mx/da/Precios_promedio_diarios_y_mensuales_en_estaciones_de_servicio.xlsx"

nombre_archivo <- "Precios_PROMEDIO_CRE.xlsx"

# Descargar y guardar el archivo en el directorio de trabajo
download.file(url, destfile = nombre_archivo, mode = "wb")

# Leer la hoja "Cuadro 1.1", omitiendo las primeras 3 filas
data_edo <- read_excel(nombre_archivo, sheet = "Cuadro 1.2", skip = 4)
# nota: Los Cuadro 1.2, Cuadro 1.3 y Cuadro 1.2 contienen los precios de los combustibles

# Show the first few rows of the dataframe
head(data_edo)

data_edo <- data_edo[1:33, ]

names(data_edo) <- c( "Estado" ) # Renombramos la primer columna

View( data_edo[c(2), ] )

