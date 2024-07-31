# Función para capturar la hora y los minutos actuales y almacenarlos en el data frame
capturar_hora <- function(df) {
  # Obtener la hora actual
  hora_actual <- Sys.time()
  
  # Extraer la hora y los minutos
  hora <- as.integer(format(hora_actual, "%H"))
  minutos <- as.integer(format(hora_actual, "%M"))
  
  # Crear un nuevo registro con la hora y los minutos actuales
  nuevo_registro <- data.frame(hora = hora, minutos = minutos, stringsAsFactors = FALSE)
  
  # Agregar el nuevo registro al data frame existente
  df <- cbind.data.frame(df, nuevo_registro)
  
  return(df)
}
