# Estadística I — Otoño 2026

**Centro de Investigación y Docencia Económicas, A.C. (CIDE)**
División de Economía · Licenciatura en Economía

**Profesor:** Benjamín Oliva-Vázquez —
[benjamin.oliva@cide.edu](mailto:benjamin.oliva@cide.edu) ·
[benjov@ciencias.unam.mx](mailto:benjov@ciencias.unam.mx)

**Laboratorista:** Giovanni Paulo Ordonez Corona —
[giovanni.ordonez@alumnos.cide.edu](mailto:giovanni.ordonez@alumnos.cide.edu)

### Horarios

| | Día y hora | Lugar |
|---|---|---|
| **Clase** | Viernes, 9:00–12:00 hrs | Aula de Videoconferencias, edificio Santa Fe |
| **Laboratorio** | Lunes, 9:40 hrs | Aula de Videoconferencias, edificio Santa Fe |
| **Horas de oficina** | Previa cita | En línea o presencial |

---

## Descripción

Curso de probabilidad y estadística descriptiva para la Licenciatura en
Economía. Este repositorio concentra el temario, las notas de clase, los
laboratorios y todo el código de R que se usa en clase.

**Objetivos**

1. Desarrollar las herramientas teóricas y prácticas de probabilidad e
   introducir el análisis de estadística descriptiva e inferencia
   estadística.
2. Establecer las bases para el uso de paquetes estadísticos y de
   procesamiento de datos en R.

## Bibliografía (Google Drive)

Los textos del curso están disponibles **sólo para las personas
integrantes del grupo** en la siguiente carpeta:

> **[📁 Bibliografía del curso en Google Drive](https://drive.google.com/drive/folders/1eQlLrz9h-DBz7NLsZazcqHM0bCM204w_?usp=sharing)**

Textos base del curso (\*):

- Larsen, R. J. y M. L. Marx (2018). *An Introduction to Mathematical
  Statistics and its Applications*, 6a ed., Pearson. (\*)
- Miller, I. y M. Miller (2014). *John E. Freund's Mathematical Statistics
  with Applications*, 8a ed., Pearson. (\*)
- Wackerly, D. D., W. Mendenhall III y R. L. Scheaffer (2008).
  *Mathematical Statistics with Applications*, 7a ed., Thomson. (\*)
- Agresti, A. y M. Kateri (2021). *Foundations of Statistics for Data
  Scientists: With R and Python*, Chapman and Hall/CRC.
- Wasserman, L. (2004). *All of Statistics. A Concise Course in
  Statistical Inference*, Springer.

## Estructura del repositorio

| Carpeta | Contenido |
|---|---|
| [`01 Temario y Notas/`](<01 Temario y Notas>) | Temario oficial (PDF), notas de clase y tablas de distribuciones. |
| [`02 Laboratorios/`](<02 Laboratorios>) | Enunciados de los 8 laboratorios del curso. |
| [`03 Código/R/`](<03 Código/R>) | Código de clase en R, organizado por tema. Ver su [README](<03 Código/R/README.md>). |
| `main.tex` | Fuente LaTeX del temario. |

## Temario

| # | Tema | Código |
|---|---|---|
| 1 | Probabilidad | [`01_Probabilidad/`](<03 Código/R/01_Probabilidad>) |
| 2 | Variables Aleatorias | [`02_Variables_Aleatorias/`](<03 Código/R/02_Variables_Aleatorias>) |
| 3 | Esperanza | [`03_Esperanza/`](<03 Código/R/03_Esperanza>) |
| 4 | Algunas Desigualdades Importantes | [`04_Desigualdades_y_Convergencia/`](<03 Código/R/04_Desigualdades_y_Convergencia>) |
| 5 | Convergencia de Variables Aleatorias | [`04_Desigualdades_y_Convergencia/`](<03 Código/R/04_Desigualdades_y_Convergencia>) |
| 6 | Estadística Descriptiva con R (transversal) | [`05_Estadistica_Descriptiva/`](<03 Código/R/05_Estadistica_Descriptiva>) |

El detalle por subtema está en el
[temario](<01 Temario y Notas/Temario_Estadistica_I_2026.pdf>).

## Cómo usar el código

Todo el código es **R base** (sin `tidyverse`), pensado para correrse en
RStudio línea por línea.

```r
# Scripts .R (temas 1 a 5)
source("03 Código/R/01_Probabilidad/01_Espacios_Muestrales_y_Eventos.R")

# Notebooks .Rmd (tema 6): abrir en RStudio y usar "Knit", o bien
install.packages("readxl")   # única dependencia externa
rmarkdown::render("03 Código/R/05_Estadistica_Descriptiva/01_Descripciones_Numericas.Rmd")
```

Requisitos: R (≥ 4.0) y, opcionalmente, RStudio. El paquete `readxl` sólo
hace falta para los notebooks del tema 6. Las simulaciones usan
`set.seed(2026)`, así que los resultados son reproducibles.

Cada script cierra con una sección **"Para practicar"** con ejercicios
sugeridos, sin resolver.

## Evaluación

| Rubro | Peso |
|---|---|
| Tareas, quizzes y otras asignaciones en R | 15% |
| Laboratorio | 15% |
| Examen Parcial | 35% |
| Examen Final | 35% |

## Calendario tentativo

Las fechas están sujetas a cambios por acuerdo del grupo.

| Laboratorio / Examen | Fecha (tentativa) | Día |
|---|---|---|
| Laboratorio 1 | 31 de agosto de 2026 | Lunes |
| Laboratorio 2 | 7 de septiembre de 2026 | Lunes |
| Laboratorio 3 | 14 de septiembre de 2026 | Lunes |
| Laboratorio 4 | 21 de septiembre de 2026 | Lunes |
| **Examen Parcial** (temas 1 y 2\*) | **2 de octubre de 2026** | Viernes |
| Laboratorio 5 | 19 de octubre de 2026 | Lunes |
| Laboratorio 6 | 26 de octubre de 2026 | Lunes |
| Laboratorio 7 | 2 de noviembre de 2026 | Lunes |
| Laboratorio 8 | 9 de noviembre de 2026 | Lunes |
| **Examen Final** (temas 2\*, 3, 4 y 5) | **11 de diciembre de 2026** | Viernes |

\* Este tema podría evaluarse de forma parcial dependiendo del avance del
curso.
