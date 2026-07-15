# Código de clase — Estadística I

Todo el código del curso está en R (base R, sin tidyverse) y está
organizado por tema siguiendo el orden del temario oficial, no por semana
de clase.

## Estructura

| Carpeta | Temario | Contenido |
|---|---|---|
| `01_Probabilidad/` | Tema 1 | Espacios muestrales, cálculo combinatorio, definición de probabilidad, independencia, probabilidad condicional, teorema de Bayes. |
| `02_Variables_Aleatorias/` | Tema 2 | Funciones de distribución, VA discretas y continuas, distribuciones bivariadas/marginales/condicionales, independencia, muestras IID, distribuciones multivariadas, transformaciones (optativo). |
| `03_Esperanza/` | Tema 3 | Esperanza y sus propiedades, varianza y covarianza, esperanza/varianza de distribuciones comunes, esperanza condicional, funciones generadoras de momentos. |
| `04_Desigualdades_y_Convergencia/` | Temas 4-5 | Desigualdades de Markov, Chebyshev y Jensen; tipos de convergencia; Ley de los Grandes Números; Teorema del Límite Central. |
| `05_Estadistica_Descriptiva/` | Tema 6 | Conceptos elementales, descripciones numéricas, gráficas y para datos conjuntos, con datos reales (CRE, INEGI). |

Los temas 1 a 4 son scripts `.R` (se ejecutan de arriba a abajo en RStudio
o con `Rscript`). El tema 5 son notebooks R Markdown (`.Rmd`), pensados
como el cierre reproducible del primer tramo del curso: usan datos reales
en `05_Estadistica_Descriptiva/data/` en vez de datos simulados.

Cada script cierra con una sección **"Para practicar"** con ejercicios
sugeridos (sin resolver).

## Cómo correr el código

- Scripts `.R`: ábrelos en RStudio y corre línea por línea, o
  `Rscript nombre_del_script.R` desde terminal.
- Notebooks `.Rmd`: ábrelos en RStudio y usa "Knit", o
  `rmarkdown::render("nombre.Rmd")` desde la consola de R. Requieren el
  paquete `readxl` para leer los archivos `.xlsx`.

## Datos

Los datasets usados en `05_Estadistica_Descriptiva/` están incluidos en
`data/` para que los notebooks corran sin depender de descargas externas:

- `Gas_Prices.csv` — precios de combustibles (Comisión Reguladora de
  Energía).
- `Educacion.xlsx` — grado promedio de escolaridad por entidad (INEGI).
- `enigh2024_ns_concentradohogar_csv.zip` — microdatos de hogares, Encuesta
  Nacional de Ingresos y Gastos de los Hogares 2024 (INEGI).
