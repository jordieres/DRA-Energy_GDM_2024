
library(readxl)


price = read_excel('PRECIO_ENERGIA.xlsx')


precio <- function(datos){
  
  preciohora = 0
  
  if(datos$Longitud_m_unidad ==0 | datos$Peso_t_unidad ==0 | datos$Espesor_mm ==0 | datos$Ancho_mm ==0){
    
    print('El producto no es válido.')
    preciohora = 0
    
  }else{
    
    columna1 <- datos$hora
    columna2 <- datos$hora + 1
    
    
    
    
    
    if(datos$minutos + datos$Tiempo_produccion_unidad > 60){
      
      
      price1 <- price[, columna1]
      price2 <- price[, columna2]
      
      
      preciohora <- ((60 - datos$minutos) * price1 + 
                       (datos$Tiempo_produccion_unidad + datos$minutos - 60) * price2) / 
        datos$Tiempo_produccion_unidad
      
    } else {
      preciohora <- price[, columna1]
    }
  }

  

  resultado <- (datos$Consumo_total*datos$Tiempo_produccion_unidad/60) * preciohora
  return(resultado)
}


