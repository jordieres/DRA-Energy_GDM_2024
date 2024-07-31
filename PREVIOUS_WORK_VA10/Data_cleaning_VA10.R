library(readxl)
library(dplyr)

rm(list = ls())

# Leer la base de datos desde el archivo Excel
datos = read_excel("linea_VA10.guay.xlsx")

# Sumar los valores de la columna "Numero" agrupados por los valores de la columna "Valor"
resultado <- datos %>%
  group_by(`order no.`) %>%
  summarise(Tiempo_produccion_a = `(VA) actual Processing time (min)`,
            Tiempo_produccion = sum(`(VA) actual Processing time (min)`, na.rm = TRUE),
            Longitud_m = sum(`(VA) length`, na.rm = TRUE),
            Peso = sum(`(VA) weight`, na.rm = TRUE),
            Espesor = `(VA) thickness`,
            Ancho = `(VA) width`,
            Longitud = `(VA) length`,
            Forma = `Coil/Sheet/Strip/Scoll`,
            Recocido = `Batch Annealing/Continuous Annealing/Not Annealing`,
            Reduccion = `Single Reduction/Double Reduction/No Reduction`,
            Recubrimiento = `Chromium-coated/Tin-coated/Others`,
            Num.productos = n(),
            Velocidad = Longitud/Tiempo_produccion_a
  )

res = as.data.frame(resultado)
View(res)

datos2 = read_excel("VA10.xlsx")
datos2 = na.omit(datos2)

# Función para contar las repeticiones no seguidas
contar_repeticiones <- function(columna_valores) {
  repeticiones <- integer(length(columna_valores))
  for (i in 1:(length(columna_valores) - 1)) {
    if (columna_valores[i] != columna_valores[i + 1]) {
      repeticiones[i] <- 1
    }
  }
  # El último valor siempre se cuenta como repetición
  repeticiones[length(columna_valores)] <- 1
  return(repeticiones)
}

secuencias = contar_repeticiones(datos2$`Production-Order-NR`)
datos2$secuencias = secuencias

resultado2 <- datos2 %>%
  group_by(`Production-Order-NR`) %>%
  summarise(parametroA = `Production-Order Parameter A`,
            parametroB = `Production-Order Parameter B`,
            parametroC = `Production-Order Parameter C`,
            parametroD = `Production-Order Parameter D`,
            parametroE = `Production-Order Parameter E`,
            parametroF = `Production-Order Parameter F`,
            parametroG = `Production-Order Parameter G`,
            parametroHMin = `Production-Order Parameter H-Min`,
            parametroHMax = `Production-Order Parameter H-Max`,
            parametroI = `Production-Order Parameter I`,
            Componente1kW= sum(Component1, na.rm = TRUE),
            Componente2kW= sum(Component2, na.rm = TRUE),
            Componente3kW= sum(Component3, na.rm = TRUE),
            Componente4kW= sum(Component4, na.rm = TRUE),
            Fecha_inicio = first(Time),
            Fecha_fin = last(Time),
            Secuencias = sum(secuencias),
            TinLayerup = `Tin-Layer-up`,
            TinLayerdown = `Tin-Layer-down`,
            ton_hora = mean(ToneladasHora, na.rm = TRUE),
            ton_hora_metro2 = mean(ToneladasMetroCuadradoHora, na.rm = TRUE),
  )
res2 = as.data.frame(resultado2)
View(res2)

# Realizar una unión interna basada en las columnas coincidentes
resultado3 <- merge(res, res2, by.x = "order no.", by.y = "Production-Order-NR")

# Mostrar el resultado
res3 = as.data.frame(resultado3)
View(res3)


resultado4 <- resultado3 %>%
  group_by(`order no.`) %>%
  summarise(
    Fecha_primer_dato = Fecha_inicio,
    Fecha_último_dato = Fecha_fin,
    Tiempo_produccion = Tiempo_produccion,
    Secuencias = Secuencias,
    Forma = Forma,
    Num_productos = Num.productos,
    Espesor_mm = Espesor,
    Ancho_mm = Ancho,
    Longitud_m = Longitud_m,
    Peso_t = Peso,
    ton_hora_metro2 = ton_hora_metro2,
    ton_hora = ton_hora,
    Velocidad = mean(Velocidad, na.rm = TRUE),
    TinLayerup = TinLayerup,
    TinLayerdown = TinLayerdown,
    parametroA = parametroA,
    parametroB = parametroB,
    parametroC = parametroC,
    parametroD = parametroD,
    parametroE = parametroE,
    parametroF = parametroF,
    parametroG = parametroG,
    parametroHMin = parametroHMin,
    parametroHMax = parametroHMax,
    parametroI = parametroI,
    Componente1kW= Componente1kW,
    Componente2kW= Componente2kW,
    Componente3kW= Componente3kW,
    Componente4kW= Componente4kW,
    Recocido = Recocido,
    Reduccion = Reduccion,
    Recubrimiento = Recubrimiento,
    
  )

# Mostrar el resultado
res4 = as.data.frame(resultado4)
View(res4)

# Eliminar filas duplicadas basadas en la columna order no.
res_unique <- distinct(res4, `order no.`, .keep_all = TRUE)


# Mostrar el resultado
View(res_unique)

library(openxlsx)

VA10_final = "VA10_final.xlsx"

write.xlsx(res_unique, file = VA10_final, rowNames = FALSE)
