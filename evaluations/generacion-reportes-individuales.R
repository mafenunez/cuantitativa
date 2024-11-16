# Paquetes necesarios
library(googlesheets4)
library(dplyr)

# Autenticación para acceder a Google Sheets
gs4_auth()

# Leer el archivo de Google Sheets sin asignar la primera fila como encabezado
data <- read_sheet("https://docs.google.com/spreadsheets/d/1s70sLhCXoioqHDoJBTYmJMQX7npCXAQmc3YE_ByWgIs/edit?gid=1556635076#gid=1556635076", sheet = "resultados_all", col_names = FALSE)

# Define los nombres de las columnas usando la primera fila y elimina esa fila
col_names <- as.character(unlist(data[1, ]))
data <- data[-1, ]
colnames(data) <- col_names

# Determina el número de bloques de 3 filas redondeado hacia abajo a un entero
num_cases <- floor(nrow(data) / 3)

# Crea y guarda archivos CSV para cada grupo de 3 filas
lapply(seq_len(num_cases), function(i) {
  # Selecciona las filas correspondientes al grupo actual (3 filas por caso)
  start_row <- (i - 1) * 3 + 1
  end_row <- start_row + 2
  case_data <- data[start_row:end_row, ]
  
  # Extrae el nombre del archivo desde la primera columna del primer caso
  case_id <- case_data[1, 1]
  file_name <- paste0(case_id, ".csv")
  
  # Convierte columnas de tipo 'list' a 'character' para evitar errores
  case_data <- as.data.frame(lapply(case_data, function(col) {
    if (is.list(col)) {
      sapply(col, function(x) paste(x, collapse = ", "))
    } else {
      col
    }
  }))
  
  # Agrega los títulos de las columnas al dataset antes de guardar
  #case_data <- rbind(col_names, case_data)
  #colnames(case_data) <- col_names
  
  # Guarda el archivo CSV
  write.csv(case_data, file_name, row.names = FALSE)
})
