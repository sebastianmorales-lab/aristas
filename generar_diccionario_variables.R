# Script para generar un diccionario de variables a partir de datos_final y Diccionario_Alumnos-6to-HSE.xlsx

# Instalar y cargar paquetes necesarios

if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
library(readxl)

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

# Crear el data frame de salida
variables_info <- data.frame(
  Variable = names(datos_final),
  Descripcion = sapply(seq_along(names(datos_final)), function(i) {
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
  Tipo.de.variable = sapply(datos_final, function(x) class(x)[1]),
  Valores = sapply(datos_final, function(x) {
    if (is.numeric(x)) {
      paste0(min(x, na.rm=TRUE), "---", max(x, na.rm=TRUE))
    } else {
      vals <- unique(x)
      vals <- vals[!is.na(vals)]
      # Reemplazar los ; por , en la concatenación
      paste(gsub(";", ",", head(vals, 10)), collapse = ", ")
    }
  }),
  stringsAsFactors = FALSE
)

# Reemplazar los tipos de variable por los nombres solicitados
tipo_map <- function(tipo) {
  if (tipo %in% c("integer", "numeric")) {
    return("numérica")
  } else if (tipo == "factor") {
    return("categórica")
  } else {
    return(tipo)
  }
}
variables_info$Tipo.de.variable <- sapply(variables_info$Tipo.de.variable, tipo_map)

# Convertir a UTF-8 explícitamente para evitar problemas de tildes
variables_info[] <- lapply(variables_info, function(x) if(is.character(x)) enc2utf8(x) else x)

# Guardar el resultado como CSV con codificación UTF-8 y separador coma
write.csv(variables_info, "diccionario_variables.csv", row.names = FALSE, fileEncoding = "UTF-8", na = "")

cat("Archivo diccionario_variables.csv generado correctamente.\n")
