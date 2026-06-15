# 
# Descriptive Statistics
#
#****************************************************************************
# Import necessary libraries
#install.packages("tidyverse")
#install.packages("hrbrthemes")
#install.packages("readxl")

library(tidyverse) # The tidyverse is an collection of R packages. 
library(ggplot2) #
#library(hrbrthemes)
library(data.table)
library(readxl)

#****************************************************************************

getwd()
# Set Working Directory
setwd("/Users/benjamin/Documents/Personal/Cursos_CIDE/Estadistica_I_2025/Estadistica-I-2025/03 Código/R/Clase_02")

getwd()

# Load the dataset
data <- read_excel("Educacion.xlsx", range = "A5:M38")

# Mostrar las primeras filas del dataframe
head(data)

# Cambiemos nombres 

names(data)

names(data) <- c( "Entidad", "Total.2000", "Hombres.2000", "Mujeres.2000",
                  "Total.2010", "Hombres.2010", "Mujeres.2010", 
                  "Total.2015", "Hombres.2015", "Mujeres.2015",      
                  "Total.2020", "Hombres.2020", "Mujeres.2020")

# Summary
summary(data[, c("Total.2000", "Total.2010","Total.2015", "Total.2020")])

# Convertir el dataframe a formato long
data_long <- melt( setDT(data), 
             id.vars = c( "Entidad" ), 
             variable.name = "Variable.Year",
             value.name = "Escolaridad" )

# Set a different color for each group
data_long %>% filter( Variable.Year == 'Total.2000' | 
                      Variable.Year == 'Total.2010' |
                      Variable.Year == 'Total.2015' |
                      Variable.Year == 'Total.2020' ) %>% 
  ggplot( aes(x=Variable.Year, y=Escolaridad, fill=Variable.Year )) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") + 
  labs(title = "Educación promedio por estado según año censal")

ggsave("Total.png", width = 20, height = 15, units = "cm")

# Set a different color for each group
data_long %>% filter( Variable.Year == 'Hombres.2000' | 
                        Variable.Year == 'Hombres.2010' |
                        Variable.Year == 'Hombres.2015' |
                        Variable.Year == 'Hombres.2020' ) %>% 
  ggplot( aes(x=Variable.Year, y=Escolaridad, fill=Variable.Year )) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") + 
  labs(title = "Educación promedio de los hombres por estado según año censal")

ggsave("Hombres.png", width = 20, height = 15, units = "cm")

# Set a different color for each group
data_long %>% filter( Variable.Year == 'Mujeres.2000' | 
                        Variable.Year == 'Mujeres.2010' |
                        Variable.Year == 'Mujeres.2015' |
                        Variable.Year == 'Mujeres.2020' ) %>% 
  ggplot( aes(x=Variable.Year, y=Escolaridad, fill=Variable.Year )) + 
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") + 
  labs(title = "Educación promedio de las mujeres por estado según año censal")

ggsave("Mujeres.png", width = 20, height = 15, units = "cm")


