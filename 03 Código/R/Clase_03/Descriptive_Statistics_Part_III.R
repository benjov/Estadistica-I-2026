# 
# Descriptive Statistics
#
#****************************************************************************
# Import necessary libraries
#install.packages("dplyr")
#install.packages("tidyverse")
#install.packages("survey")
#install.packages("scales")
#install.packages("readr")

library(dplyr)
library(tidyverse) # 
library(survey)   # diseño muestral y estimación con pesos
library(ggplot2)  # gráficos
library(scales)   # etiquetas bonitas
library(readr)

#****************************************************************************

getwd()
# Set Working Directory
setwd("/Users/benjamin/Documents/Personal/Cursos_CIDE/Estadistica_I_2025/Estadistica-I-2025/03 Código/R/Clase_03")

getwd()

# Load the dataset
# Principales variables por hogar (concentradohogar.*)
# See: https://www.inegi.org.mx/programas/enigh/nc/2024/#microdatos

data <- read_csv(unz("enigh2024_ns_concentradohogar_csv.zip", "concentradohogar.csv"))

# Mostrar las primeras filas del dataframe
head(data)

# Ver la documentación:https://www.inegi.org.mx/contenidos/programas/enigh/nc/2024/microdatos/889463924494.pdf

# Seleccionamos columnas
names(data)

data_sel <- data[ , c( "folioviv", "foliohog", "ubica_geo", "tam_loc", "est_socio", 
                       "est_dis", "upm", "factor", "tot_integ", "hombres", "mujeres", 
                       "ing_cor", "ingtrab", "gasto_mon", "alimentos" ) ]

# 0) Preparación de variables -----------------------------------------------
df <- data_sel |>
  mutate(
    # ingreso per cápita (usa el denominador > 0)
    ing_pc       = if_else(tot_integ > 0, ing_cor  / tot_integ, NA_real_),
    gasto_pc     = if_else(tot_integ > 0, gasto_mon/ tot_integ, NA_real_),
    alimentos_pc = if_else(tot_integ > 0, alimentos/ tot_integ, NA_real_)
  )

# 1) Diseño muestral (ENIGH) ------------------------------------------------
# Suele ser: upm, est_dis, factor

options(survey.lonely.psu = "adjust")  # para estratos con 1 upm

dis <- svydesign(
  ids     = ~upm,
  strata  = ~est_dis,
  weights = ~`factor`,  # usa backticks porque "factor" también es una función en R
  data    = df,
  nest    = TRUE
)

# 2) Estadísticas descriptivas ponderadas -----------------------------------
# Medias (ponderadas) básicas

stats_basicas <- svymean(
  ~ tot_integ + hombres + mujeres + ing_cor + ing_pc + gasto_mon + gasto_pc + alimentos_pc,
  design = dis, na.rm = TRUE
)

stats_basicas

# medias por tamaño de localidad o urbano/rural si quieres cortes
svyby(~ing_pc, ~tam_loc, dis, svymean, na.rm = TRUE)

# 3) Deciles de ingreso per cápita (ponderados) -----------------------------
qs <- seq(0.1, 0.9, 0.1)  # 10%,20%,...,90%

q_obj <- svyquantile(
  ~ ing_cor,                 # o ~ ing_pc si quieres per cápita
  design = dis,
  quantiles = qs,
  na.rm = TRUE,
  ci = FALSE,
  method = "pi"              # "linear" también funciona
)

deciles_vec <- unname(coef(q_obj))  # <- aquí está el fix
deciles_vec  # puntos de corte

# 4) Clasificar cada hogar según los puntos de corte -------------------------
brks <- c(-Inf, deciles_vec, Inf)
dis2 <- update(
  dis,
  decil = cut(ing_pc, breaks = brks, labels = paste0("D", 1:10),
              include.lowest = TRUE, right = TRUE)
)

# --------------------------------------------------------------------
# Promedio de ingreso per cápita por decil (con error estándar)
prom_por_decil <- svyby(
  ~ ing_pc, ~ decil, dis2, svymean, na.rm = TRUE, vartype = "se"
) |>
  arrange(decil)

prom_por_decil

names(prom_por_decil)

# Ingresos promedio mensual por deciles de ingreso de los hogares:
cbind( prom_por_decil$decil, prom_por_decil$ing_pc/3)

# --------------------------------------------------------------------
# Tarea:

# 1. Usa los pasos 0) a 4) para el porcentaje de de gasto de los hogares en
# alimentos, en educación y en vivienda
# 2. Realiza un reporte breve (1 página aprox.) que incluya tu interpretación
# de los resultados

# Fecha de entrega: 29 de octubre de 2025

# --------------------------------------------------------------------
