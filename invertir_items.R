# ...existing code...

# Supone que 'datos' es tu dataframe y 'items_invertidos' es un vector con los nombres de las columnas a invertir

library(psych)

invertir_items <- function(datos, items_invertidos, min_val = 1, max_val = 4) {
  # Solo selecciona las columnas a invertir
  datos_a_invertir <- datos[, items_invertidos, drop = FALSE]
  
  # Invierte los ítems seleccionados
  invertidos <- reverse.code(keys = rep(-1, length(items_invertidos)),
                             items = datos_a_invertir,
                             mini = min_val, maxi = max_val)
  
  # Renombra las columnas agregando "R"
  colnames(invertidos) <- paste0(colnames(invertidos), "R")
  
  # Devuelve solo las columnas invertidas
  return(invertidos)
}

# Ejemplo de uso:
# data_invertida <- invertir_items(datos, items_invertidos)
# ...existing code...
