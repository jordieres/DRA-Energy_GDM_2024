# Limpiar el entorno
rm(list = ls())

# Cargar las librerías necesarias
library(readxl)
library(mice)
library(randomForest)
library(caret)

# Cargar los datos
datos = read_excel('VA11_final.xlsx')
datos$Tiempo_produccion_unidad = datos$Tiempo_produccion / datos$Num_productos
datos$Longitud_m_unidad = datos$Longitud_m / datos$Num_productos
datos$Peso_t_unidad = datos$Peso_t / datos$Num_productos
datos$Componente1kW_unidad = datos$Componente1kW / datos$Num_productos
datos$Componente2kW_unidad = datos$Componente2kW / datos$Num_productos
datos$Componente3kW_unidad = datos$Componente3kW / datos$Num_productos
datos$Componente4kW_unidad = datos$Componente4kW / datos$Num_productos
datos$Componente5kW_unidad = datos$Componente5kW / datos$Num_productos
datos$Componente6kW_unidad = datos$Componente6kW / datos$Num_productos

# Replicar cada fila según el valor de Num_productos
datos <- datos[rep(row.names(datos), datos$Num_productos), ]
rownames(datos) <- NULL

# Seleccionar los datos relevantes
nuevos_datos = datos[, c(8, 9, 17:26, 36:44)]
nuevos_datos <- subset(nuevos_datos, Componente2kW_unidad < 4000 & Componente4kW_unidad < 1000 & Componente5kW_unidad < 2000)

# Crear un directorio para guardar los modelos
dir.create("modelos_rf_VA11", showWarnings = FALSE)

# Definir la función para entrenar, evaluar y guardar el modelo random forest usando k-fold cross-validation
train_and_evaluate_kfold <- function(data, target_column, k) {
  
  # Preparar los datos
  target_name <- paste0(target_column, "_unidad")
  data <- data[, c(1:15, match(target_name, names(data)))]
  
  # Imputar los valores faltantes en los datos
  imputed_data <- mice(data, m = 1, maxit = 50, method = 'pmm', seed = 500)
  data_imputed <- complete(imputed_data)
  
  # Configurar k-fold cross-validation
  train_control <- trainControl(method = "cv", number = k)
  
  # Entrenar el modelo random forest usando k-fold cross-validation
  formula <- as.formula(paste(target_name, "~ ."))
  rf_model <- train(formula, data = data_imputed, method = "rf", trControl = train_control, ntree = 200)
  
  # Plot variable importance
  varImpPlot(rf_model$finalModel)
  
  # Calcular el MAPE para cada fold
  mape <- function(actual, predicted) {
    mean(abs((actual - predicted) / actual) * 100, na.rm = TRUE)
  }
  
  # Obtener predicciones y calcular MAPE
  predictions <- predict(rf_model, newdata = data_imputed)
  mape_value <- mape(data_imputed[[target_name]], predictions)
  
  # Guardar el modelo
  model_path <- paste0("modelos_rf_VA11/", target_column, "_rf_model.rds")
  saveRDS(rf_model, file = model_path)
  message("Model saved to: ", model_path)
  
  return(list(model = rf_model, mape = mape_value))
}

# Entrenar y evaluar modelos para cada componente usando k-fold cross-validation
components <- c("Componente1kW", "Componente2kW", "Componente3kW", "Componente4kW", "Componente5kW", "Componente6kW")
results <- lapply(components, function(comp) {
  train_and_evaluate_kfold(nuevos_datos, comp, k = 5)
})

# Mostrar los resultados
names(results) <- components
results

# Crear un data frame para los MAPE de prueba
mape_data <- data.frame(
  Componente = components,
  MAPE = sapply(results, function(res) res$mape)
)

# Graficar los MAPE de prueba por componente
library(ggplot2)
ggplot(mape_data, aes(x = Componente, y = MAPE)) +
  geom_line() +
  geom_point() +
  labs(title = "MAPE del conjunto de prueba por componente",
       x = "Componente", y = "MAPE") +
  theme_minimal()

# Listar los archivos en el directorio de modelos
list.files("modelos_rf_VA11")
