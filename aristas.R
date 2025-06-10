#Importo librerías
library(tidyverse)
library(psych)

#Importo los datos---------------

datos <- read.csv("Datos_Estudiantes_Socioemocional_6TO.csv", header = TRUE, 
                  sep = ";", dec = ".", stringsAsFactors = TRUE,
                  na.strings = c(""," ","99"))

#Dimensiones y subdimensiones a ser evaluadas en sexto año de educación primaria
#Se analizo a nivel de dimensiones, no a nivel de subdimensiones, no a nivel de items.

#Genero una lista de los items a invertir
items_invertir <- c("ES4_1", "ES4_2", "ES4_3", "ES4_4", "ES4_5",
                    "ES8_1", "ES8_2", "ES8_3", "ES8_4", "ES8_5", "ES8_6")

# Cargo laS funcines para invertir items
source("invertir_items.R")


# filtro: estudiantes que no fueron excluidos por necesidades educativas especiales
# filtro: estudiantes que respondieron el cuestionario socioemocional
# invierto todos los ítems de la lista de ítems a invertir
datos = datos |> 
  filter(NEE==0,IND_Estudiantes_Socioemocional_6TO==1) |> 
  mutate(across(all_of(items_invertir),invertir_puntaje))


#--  motivación y autorregulación -----

items_mya = c("ES1_1", "ES1_3", "ES1_4", "ES1_5", "ES1_6","ES3_1", "ES3_2", "ES3_3", "ES3_4", "ES3_5", "ES4_1", "ES4_2", "ES4_3", "ES4_4", "ES4_5")
datos$Motivación_y_autorregulación = rowSums(datos[,items_mya],na.rm = T)

#alpha de cronbach, revisa si es necesario invertir ítems
alpha(datos[,items_mya],check.keys = T)
# 
# 
# #Genero la lista de los items invertidos usando psych::reverse.code
# #datos_invertidos <- invertir_items(datos, items_invertir)
# 
# # Une las columnas invertidas al dataframe original
# datos <- cbind(datos, datos_invertidos)
# 
# # Compara los primeros 5 valores de cada ítem antes y después de invertir
# for (item in items_invertir) {
#   original <- datos[[item]][1:5]
#   invertido <- datos[[paste0(item, "-R")]][1:5]
#   mensaje <- sprintf(
#     "Columna: %s\nOriginal: %s\nInvertido: %s\n",
#     item,
#     paste(original, collapse = ", "),
#     paste(invertido, collapse = ", ")
#   )
#   cat(mensaje, "\n")
# }


#---Habilidades_interpersonales --------
items_hinter <- c("ES5_1", "ES5_2", "ES5_3", "ES5_4", "ES5_5",
  "ES6_1", "ES6_2", "ES6_3", "ES6_4", "ES6_5", "ES6_6")

datos$Habilidades_interpersonales = rowSums(datos[,items_hinter],na.rm = T)

#alpha de cronbach, revisa si es necesario invertir ítems
alpha(datos[,items_hinter],check.keys = T)

hist(datos$Habilidades_interpersonales)

#Habilidades intrapersonales----------------
items_hintra <- c("ES7_1", "ES7_2", "ES7_3", "ES7_4", "ES7_5",
  "ES8_1", "ES8_2", "ES8_3", "ES8_4", "ES8_5", "ES8_6"
)

datos$Habilidades_intrapersonales = rowSums(datos[,items_hintra],na.rm = T)

#alpha de cronbach, revisa si es necesario invertir ítems
alpha(datos[,items_hintra],check.keys = T)



#----Reemplazo los valores de 99 en los items de las dimensiones por NA----
# Reemplazo los valores de 99 en los items de las dimensiones por NA
# datos[, unlist(Motivacion_y_autorregulacion)][datos[, unlist(Motivacion_y_autorregulacion)] == 99] <- NA
# datos[, unlist(Habilidades_interpersonales)][datos[, unlist(Habilidades_interpersonales)] == 99] <- NA
# datos[, unlist(Habilidades_intrapersonales)][datos[, unlist(Habilidades_intrapersonales)] == 99] <- NA
# 


# #----Calculo las dimensiones----
# 
# # Calculo la suma de la dimensión Motivación y autorregulación
# suma_Motivacion_y_autorregulacion <- rowSums(datos[, unlist(Motivacion_y_autorregulacion)], na.rm = TRUE)
# suma_Motivacion_y_autorregulacion[rowSums(!is.na(datos[, unlist(Motivacion_y_autorregulacion)])) == 0] <- NA
# 
# # Calculo la suma de la dimensión Habilidades interpersonales
# suma_Habilidades_interpersonales <- rowSums(datos[, unlist(Habilidades_interpersonales)], na.rm = TRUE)
# suma_Habilidades_interpersonales[rowSums(!is.na(datos[, unlist(Habilidades_interpersonales)])) == 0] <- NA
# 
# # Calculo la suma de la dimensión Habilidades intrapersonales
# suma_Habilidades_intrapersonales <- rowSums(datos[, unlist(Habilidades_intrapersonales)], na.rm = TRUE)
# suma_Habilidades_intrapersonales[rowSums(!is.na(datos[, unlist(Habilidades_intrapersonales)])) == 0] <- NA
# 
# #----Calculo las dimensiones promediando los items----
# 
# # Calculo la dimensión Motivación y autorregulación (promediando los items)
# dim_Motivacion_y_autorregulacion<- rowMeans(datos[, unlist(Motivacion_y_autorregulacion)], na.rm = TRUE)
# 
# # Calculo la dimensión Habilidades interpersonales
# dim_Habilidades_interpersonales <- rowMeans(datos[, unlist(Habilidades_interpersonales)], na.rm = TRUE)
# 
# # Calculo la dimensión Habilidades intrapersonales
# dim_Habilidades_intrapersonales <- rowMeans(datos[, unlist(Habilidades_intrapersonales)], na.rm = TRUE)
# 
# #----Agrego las dimensiones al dataframe original----
# datos <- datos %>%
#   mutate(
#     Motivacion_y_autorregulacion = suma_Motivacion_y_autorregulacion,
#     Habilidades_interpersonales = suma_Habilidades_interpersonales,
#     Habilidades_intrapersonales = suma_Habilidades_intrapersonales
#   )

#Imprimo las primeras filas del dataframe para verificar los resultados
print(head(datos))

# Filtrar los estudiantes SIN Necesidades Educativas Especiales (NEE == 0)
# 
# # ver los valores únicos de cada dimensión
# print(unique(datos$Motivacion_y_autorregulacion))
# print(unique(datos$Habilidades_interpersonales))
# print(unique(datos$Habilidades_intrapersonales))
# # Ver valores únicos de cada ítem en la dimensión Motivación y autorregulación
# print(lapply(datos[, unlist(Motivacion_y_autorregulacion)], unique))
# # Ver valores únicos de cada ítem en la dimensión Habilidades interpersonales
# print(lapply(datos[, unlist(Habilidades_interpersonales)], unique))
# # Ver valores únicos de cada ítem en la dimensión Habilidades intrapersonales
# print(lapply(datos[, unlist(Habilidades_intrapersonales)], unique))

# Grafico la distribución de las dimensiones
ggplot(datos, aes(x = Motivación_y_autorregulación)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7, na.rm = TRUE) +
  geom_density(aes(y = ..count..), color = "red", size = 1, adjust = 1, na.rm = TRUE) +
  labs(title = "Distribucion de Motivacion y Autorregulacion",
       x = "Motivacion y Autorregulacion",
       y = "Frecuencia") +
  theme_minimal()

ggplot(datos, aes(x = Habilidades_interpersonales)) +
  geom_histogram(binwidth = 1, fill = "green", color = "black", alpha = 0.7, na.rm = TRUE) +
  geom_density(aes(y = ..count..), color = "red", size = 1, adjust = 1, na.rm = TRUE) +
  labs(title = "Distribucion de Habilidades Interpersonales",
       x = "Habilidades Interpersonales",
       y = "Frecuencia") +
  theme_minimal()

ggplot(datos, aes(x = Habilidades_intrapersonales)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black", alpha = 0.7, na.rm = TRUE) +
  geom_density(aes(y = ..count..), color = "red", size = 1, adjust = 1, na.rm = TRUE) +
  labs(title = "Distribucion de Habilidades Intrapersonales",
       x = "Habilidades Intrapersonales",
       y = "Frecuencia") +
  theme_minimal()


# Calculo el apha de Cronbach para cada dimensión
# alpha_Motivacion_y_autorregulacion <- psych::alpha(datos[, unlist(Motivacion_y_autorregulacion)], check.keys = TRUE)
# print(alpha_Motivacion_y_autorregulacion)
# 
# alpha_Habilidades_interpersonales <- psych::alpha(datos[, unlist(Habilidades_interpersonales)], check.keys = TRUE)
# print(alpha_Habilidades_interpersonales)
# 
# alpha_Habilidades_intrapersonales <- psych::alpha(datos[, unlist(Habilidades_intrapersonales)], check.keys = TRUE)
# print(alpha_Habilidades_intrapersonales)

fa.parallel(datos[, items_mya], fa="pc", n.iter=100)
fa(datos[, items_mya], nfactors=1, rotate="none")

fa.parallel(datos[, items_hinter], fa="pc", n.iter=100)
fa(datos[, items_hinter], nfactors=1, rotate="none")

fa.parallel(datos[, items_hintra], fa="pc", n.iter=100)
fa(datos[, items_hintra], nfactors=1, rotate="none")

# --- Selecciona las columnas de interés para el nuevo dataframe ---
# Excluye todas las columnas que empiezan con "ES", "BE" o "EST_W" seguidos de un número o cualquier carácter
datos_final <- datos %>%
  select(
    -matches("^ES[0-9]"),
    -matches("^BE[0-9]"),
    -matches("^EST_W")
  ) %>%
  select(MdeoInt, Categoria, SECTOR,
         ContextoANEP, ESCS_Centro_cat, Concurre, EDAD, AlumnoGenero, extraedad,
         ESCS_Alumno_cat, Niveles_LEN, Niveles_MAT,
         Motivación_y_autorregulación, Habilidades_interpersonales, Habilidades_intrapersonales)

datos_final$SECTOR <- factor(datos_final$SECTOR, levels = c(1, 2), labels = c("Público", "Privado"))

head(datos_final)



# Guarda datos_final como un RData que solo contiene datos_final
save(datos_final, file = "Aristas.RData")

cat("Archivo Aristas.RData generado correctamente.\n")