
source('ENGY_PREDICT.R')
source('PRICE_FUNCTION.R')
source('TIME_FUNCTION.R')

precio_final <- function(datos){
  components <- c("Componente1kW", "Componente2kW", "Componente3kW", "Componente4kW", "Componente5kW", "Componente6kW","Componente7kW")
  
  predictions <- predict_with_models(datos, "modelos_rf_VA09", components)
  
  predictions = as.data.frame(predictions)
  
  datos = cbind.data.frame(datos,predictions)
  
  datos$Consumo_total = sum(datos[,17:23])
  
  print(paste(datos$Consumo_total, 'kW consumidos'))
  
  datos = capturar_hora(datos)
  
  precio_producto <- precio(datos)
  
  numero_productos_orden = datos$Numero_producto
  
  result = precio_producto * numero_productos_orden
  
  return(result)
  
}
