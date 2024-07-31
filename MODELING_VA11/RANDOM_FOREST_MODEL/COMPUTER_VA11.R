
rm(list = ls()) 
library(randomForest)

source('FUNCION_PRECIO_FINAL_VA11.R')
source('CONTROL_ERRORES_VA11.R')



# Definición del data frame 'producto' con los nombres de los parámetros
producto = data.frame(
  Espesor_mm = 0,
  Ancho_mm = 0,
  parametroA = 0,
  parametroB = 0,
  parametroC = 0,
  parametroD = 0,
  parametroE = 0,
  parametroF = 0,
  parametroG = 0,
  parametroHMax = 0,
  parametroHMin = 0,
  parametroI = 0,
  Longitud_m_unidad  = 0, 
  Peso_t_unidad  = 0,
  Numero_producto = 0
)


# Iterar a lo largo de los nombres de las columnas de 'producto' para pedir al usuario valores numéricos
for (i in names(producto)) {
  repeat {
    # Leer la entrada del usuario como cadena
    valor <- readline(paste0("Valor de ", i, ": "))
    # Validar el valor
    if (validar_valor(i, valor)) {
      # Convertir el valor a numérico y asignarlo al data frame 'producto' en la columna correspondiente
      producto[[i]] <- as.numeric(valor)
      break
    }
  }
}




# Mostrar el data frame resultante

producto$Tiempo_produccion_unidad = producto$Longitud_m_unidad / 739.4691
print(producto)

precio_orden = precio_final(producto)
precio_orden

(precio_unitario= precio_orden/producto$Numero_producto)


