
# Función para validar los valores
validar_valor <- function(parametro, valor) {
  # Condición 1: Coma en vez de punto
  if (grepl(",", valor)) {
    cat("Error: Utiliza un punto en lugar de una coma para los decimales.\n")
    return(FALSE)
  }
  # Condición 2: Espacios
  if (grepl("\\s", valor)) {
    cat("Error: No introduzcas espacios.\n")
    return(FALSE)
  }
  # Condición 3: Signos especiales
  if (grepl("[^0-9.]", valor)) {
    cat("Error: No introduzcas signos especiales o letras.\n")
    return(FALSE)
  }
  # Convertir el valor a numérico después de pasar las validaciones iniciales
  valor_numerico <- as.numeric(valor)
  if (parametro == "Espesor_mm" && (valor_numerico < 0.1 || valor_numerico > 0.7)) {
    cat("Error: El valor de Espesor_mm debe estar entre 0.1 y 0.7\n")
    return(FALSE)
  }
  if (parametro == "Ancho_mm" && (valor_numerico < 680  || valor_numerico > 1200)) {
    cat("Error: El valor de Ancho_mm debe estar entre 680 y 1200\n")
    return(FALSE)
  }
  if (parametro == "Longitud_m_unidad" && (valor_numerico < 0)) {
    cat("Error: El valor de Longitud_m_unidad debe ser mayor que 0.\n")
    return(FALSE)
  }
  if (parametro == "Peso_t " && (valor_numerico < 0)) {
    cat("Error: El valor de Peso_t  debe ser mayor que 0.\n")
    return(FALSE)
  }
  if (parametro == "parametroA" && (valor_numerico < 100 || valor_numerico >430)) {
    cat("Error: El valor de parametroA debe debe estar entre 100 y 430\n")
    return(FALSE)
  }
  if (parametro == "parametroB" && (valor_numerico < 1 || valor_numerico > 52)) {
    cat("Error: El valor de parametroB debe estar entre 1 y 52.\n")
    return(FALSE)
  }
  if (parametro == "parametroC" && (valor_numerico < 1 || valor_numerico > 68)) {
    cat("Error: El valor de parametroC debe estar entre 1 y 68\n")
    return(FALSE)
  }
  if (parametro == "parametroD" && (valor_numerico < 0 || valor_numerico > 8)) {
    cat("Error: El valor de parametroD debe estar entre 0 y 8.\n")
    return(FALSE)
  }
  if (parametro == "parametroE" && (valor_numerico < 100 || valor_numerico > 100000)) {
    cat("Error: El valor de parametroE debe estar entre 100 y 100000.\n")
    return(FALSE)
  }

  if (parametro == "parametroF" && (valor_numerico < 20 || valor_numerico > 57)) {
    cat("Error: El valor de parametroF debe estar entre 20 y 57.\n")
    return(FALSE)
  }
  if (parametro == "parametroHMin" && (valor_numerico < 0 || valor_numerico > 1)) {
    cat("Error: El valor de parametroHMin debe estar entre 0 y 1.\n")
    return(FALSE)
  }
  if (parametro == "parametroHMax" && (valor_numerico < 0 || valor_numerico > 1.3)) {
    cat("Error: El valor de parametroHMax debe estar entre 0 y 1.3.\n")
    return(FALSE)
  }
  if (parametro == "parametroI" && (valor_numerico < 102 || valor_numerico > 381)) {
    cat("Error: El valor de parametroI debe estar entre 102 y 362.\n")
    return(FALSE)
  }
  if (parametro == "Numero_producto" && (valor_numerico < 0)) {
    cat("Error: El valor de Numero_producto debe ser mayor que 0.\n")
    return(FALSE)
  }
  return(TRUE)
}

