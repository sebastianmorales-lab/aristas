#Importo librerías
library(tidyverse)
library(ggplot2)
library(psych)

#Importo los datos

datos <- read.csv("Datos_Estudiantes_Socioemocional_6TO.csv", header = TRUE, sep = ";", dec = ".", stringsAsFactors = TRUE)

#Dimensiones y subdimensiones a ser evaluadas en sexto año de educación primaria
#Se analizo a nivel de dimensiones, no a nivel de subdimensiones, no a nivel de items.

#Genero una lista de los items a invertir
items_invertir <- c("ES4_1", "ES4_2", "ES4_3", "ES4_4", "ES4_5",
                    "ES8_1", "ES8_2", "ES8_3", "ES8_4", "ES8_5", "ES8_6")

# Cargo la función para invertir items
source("invertir_items.R")

#Genero la lista de los items invertidos usando psych::reverse.code
datos_invertidos <- invertir_items(datos, items_invertir)

# Une las columnas invertidas al dataframe original
datos <- cbind(datos, datos_invertidos)

# Compara los primeros 5 valores de cada ítem antes y después de invertir
for (item in items_invertir) {
  original <- datos[[item]][1:5]
  invertido <- datos[[paste0(item, "-R")]][1:5]
  mensaje <- sprintf(
    "Columna: %s\nOriginal: %s\nInvertido: %s\n",
    item,
    paste(original, collapse = ", "),
    paste(invertido, collapse = ", ")
  )
  cat(mensaje, "\n")
}

#----Genero una lista de los items a incluir en cada dimensión (3 dimensiones)----
#Motivación y autorregulación
Motivacion_y_autorregulacion <- list(
  "ES1_1", "ES1_2", "ES1_3", "ES1_4", "ES1_5", "ES1_6",

  "ES3_1", "ES3_2", "ES3_3", "ES3_4", "ES3_5",
  "ES4_1-R", "ES4_2-R", "ES4_3-R", "ES4_4-R", "ES4_5-R")

#Habilidades_interpersonales
Habilidades_interpersonales <- list(
  "ES5_1", "ES5_2", "ES5_3", "ES5_4", "ES5_5",
  "ES6_1", "ES6_2", "ES6_3", "ES6_4", "ES6_5", "ES6_6"
)

#Habilidades intrapersonales
Habilidades_intrapersonales <- list(
  "ES7_1", "ES7_2", "ES7_3", "ES7_4", "ES7_5",
  "ES8_1-R", "ES8_2-R", "ES8_3-R", "ES8_4-R", "ES8_5-R", "ES8_6-R"
)

#----Calculo las dimensiones----

# Calculo la suma de la dimensión Motivación y autorregulación
suma_Motivacion_y_autorregulacion <- rowSums(datos[, unlist(Motivacion_y_autorregulacion)], na.rm = TRUE)

# Calculo la suma de la dimensión Habilidades interpersonales
suma_Habilidades_interpersonales <- rowSums(datos[, unlist(Habilidades_interpersonales)], na.rm = TRUE)

# Calculo la suma de la dimensión Habilidades intrapersonales
suma_Habilidades_intrapersonales <- rowSums(datos[, unlist(Habilidades_intrapersonales)], na.rm = TRUE)

# Calculo la dimensión Motivación y autorregulación
dim_Motivacion_y_autorregulacion<- rowMeans(datos[, unlist(Motivacion_y_autorregulacion)], na.rm = TRUE)

# Calculo la dimensión Habilidades interpersonales
dim_Habilidades_interpersonales <- rowMeans(datos[, unlist(Habilidades_interpersonales)], na.rm = TRUE)

# Calculo la dimensión Habilidades intrapersonales
dim_Habilidades_intrapersonales <- rowMeans(datos[, unlist(Habilidades_intrapersonales)], na.rm = TRUE)

#----Agrego las dimensiones al dataframe original----
datos <- datos %>%
  mutate(
    Motivacion_y_autorregulacion = suma_Motivacion_y_autorregulacion,
    Habilidades_interpersonales = suma_Habilidades_interpersonales,
    Habilidades_intrapersonales = suma_Habilidades_intrapersonales
  )

#Imprimo las primeras filas del dataframe para verificar los resultados
print(head(datos))

# Filtrar los estudiantes SIN Necesidades Educativas Especiales (NEE == 0)
datos <- datos %>% filter(NEE == 0)

# Calculo el apha de Cronbach para cada dimensión
alpha_Motivacion_y_autorregulacion <- psych::alpha(datos[, unlist(Motivacion_y_autorregulacion)], check.keys = TRUE)
print(alpha_Motivacion_y_autorregulacion)

alpha_Habilidades_interpersonales <- psych::alpha(datos[, unlist(Habilidades_interpersonales)], check.keys = TRUE)
print(alpha_Habilidades_interpersonales)

alpha_Habilidades_intrapersonales <- psych::alpha(datos[, unlist(Habilidades_intrapersonales)], check.keys = TRUE)
print(alpha_Habilidades_intrapersonales)

# Análisis factorial exploratorio (si quieres validar estructura interna)
fa.parallel(datos[, unlist(Motivacion_y_autorregulacion)], fa="pc", n.iter=100)
fa(datos[, unlist(Motivacion_y_autorregulacion)], nfactors=1, rotate="none")

fa.parallel(datos[, unlist(Habilidades_interpersonales)], fa="pc", n.iter=100)
fa(datos[, unlist(Habilidades_interpersonales)], nfactors=1, rotate="none")

fa.parallel(datos[, unlist(Habilidades_intrapersonales)], fa="pc", n.iter=100)
fa(datos[, unlist(Habilidades_intrapersonales)], nfactors=1, rotate="none")