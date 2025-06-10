# Script para generar un diccionario de variables simplificado: columnas 'var' y 'etiqueta'

# Instalar y cargar paquetes necesarios
if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)
if (!requireNamespace("knitr", quietly = TRUE)) install.packages("knitr")
library(knitr)

# Cargar datos_final (si no está en el entorno)
if (!exists("datos_final")) {
  load("Aristas.RData")
}

# Leer el diccionario desde el archivo Excel
# Se asume que la primera hoja contiene la información y que la columna 1 es el nombre de la variable y la columna 4 la descripción

diccionario <- read_excel("Diccionario_Alumnos-6to-HSE.xlsx", col_names = TRUE)

# Ajusta estos nombres de columna si son diferentes en tu archivo
col_variable <- 1  # Número de columna con el nombre de la variable
col_descripcion <- 4  # Número de columna con la descripción

# Crear el data frame de salida con columnas 'var' y 'etiqueta'
diccionario_simple <- data.frame(
  var = names(datos_final),
  etiqueta = sapply(seq_along(names(datos_final)), function(i) {
    var <- names(datos_final)[i]
    # Para las últimas tres variables, colocar 'Dimension: <nombre_variable>'
    if (i > (length(names(datos_final)) - 3)) {
      paste("Dimension:", var)
    } else {
      idx <- which(diccionario[[col_variable]] == var)
      if (length(idx) > 0 && !is.na(idx[1])) {
        desc <- diccionario[[col_descripcion]][idx[1]]
        if (!is.na(desc)) as.character(desc) else NA
      } else {
        NA
      }
    }
  }),
  stringsAsFactors = FALSE
)

# Convertir a UTF-8 explícitamente para evitar problemas de tildes
diccionario_simple[] <- lapply(diccionario_simple, function(x) if(is.character(x)) enc2utf8(x) else x)


# Guardar el resultado como RData y CSV
save(diccionario_simple, file = "diccionario.RData")
write.csv(diccionario_simple, "diccionario.csv", row.names = FALSE, fileEncoding = "UTF-8", na = "")

cat("Archivo diccionario_simple.RData y diccionario.csv generados correctamente.\n")

# Imprimir como tabla en consola
kable(diccionario_simple, caption = "Diccionario simple de variables")
