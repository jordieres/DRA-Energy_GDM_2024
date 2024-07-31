# Limpiar el entorno
rm(list = ls())

# Cargar las librerías necesarias
library(readxl)
library(mice)
library(randomForest)
library(ggplot2)
library(caret)

# Cargar los datos
datos = read_excel('VA10_final.xlsx')
datos$Tiempo_produccion_unidad = datos$Tiempo_produccion / datos$Num_productos
datos$Longitud_m_unidad = datos$Longitud_m / datos$Num_productos
datos$Peso_t_unidad = datos$Peso_t / datos$Num_productos
datos$Componente1kW_unidad = datos$Componente1kW / datos$Num_productos
datos$Componente2kW_unidad = datos$Componente2kW / datos$Num_productos
datos$Componente3kW_unidad = datos$Componente3kW / datos$Num_productos
datos$Componente4kW_unidad = datos$Componente4kW / datos$Num_productos

# Replicar cada fila según el valor de Num_productos
datos <- datos[rep(row.names(datos), datos$Num_productos), ]
rownames(datos) <- NULL

# Seleccionar los datos relevantes
nuevos_datos = datos[, c(8, 9, 17:26, 34:40)]
nuevos_datos <- subset(nuevos_datos, Componente4kW_unidad < 2500)

# Crear un directorio para guardar los modelos
dir.create("modelos_rf", showWarnings = FALSE)

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
  model_path <- paste0("modelos_rf/", target_column, "_rf_model.rds")
  saveRDS(rf_model, file = model_path)
  message("Model saved to: ", model_path)
  
  return(list(model = rf_model, mape = mape_value))
}

# Entrenar y evaluar modelos para cada componente usando k-fold cross-validation
components <- c("Componente1kW", "Componente2kW", "Componente3kW", "Componente4kW")
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
ggplot(mape_data, aes(x = Componente, y = MAPE)) +
  geom_line() +
  geom_point() +
  labs(title = "MAPE del conjunto de prueba por componente",
       x = "Componente", y = "MAPE") +
  theme_minimal()

# Definir la función para entrenar y evaluar el modelo de bosque aleatorio
train_and_evaluate <- function(train_data, test_data, target_column) {
  
  # Preparar los datos
  target_name <- paste0(target_column, "_unidad")
  c_train <- train_data[, c(1:11, match(target_name, names(train_data)))]
  c_test <- test_data[, c(1:11, match(target_name, names(test_data)))]
  
  # Imputar valores faltantes en los datos de entrenamiento
  imputed_train <- mice(c_train, m = 1, maxit = 50, method = 'pmm', seed = 500)
  c_train_imputed <- complete(imputed_train)
  
  # Entrenar el modelo de bosque aleatorio
  formula <- as.formula(paste(target_name, "~ ."))
  r_model <- randomForest(formula, data = c_train_imputed)
  
  # Imputar valores faltantes en los datos de prueba
  c_test_imputed <- complete(mice(c_test, m = 1, maxit = 50, method = 'pmm', seed = 500))
  test_predictions <- predict(r_model, newdata = c_test_imputed)
  
  # Calcular errores para cada observación
  test_errors <- c_test[[target_name]] - test_predictions
  
  # Calcular MAPE
  mape_train <- mean(abs((c_train[[target_name]] - predict(r_model)) / c_train[[target_name]]) * 100, na.rm = TRUE)
  mape_test <- mean(abs((c_test[[target_name]] - test_predictions) / c_test[[target_name]]) * 100, na.rm = TRUE)
  
  return(list(model = r_model, mape_train = mape_train, mape_test = mape_test, test_errors = test_errors))
}

# Evaluar modelos para cada componente usando k-fold cross-validation
results <- lapply(components, function(comp) {
  train_and_evaluate(train, test, comp)
})

# Mostrar los resultados
names(results) <- components

# Crear un data frame para los errores de prueba
error_data_long <- data.frame(
  Componente = rep(components, each = length(results[[1]]$test_errors)),
  Error = unlist(lapply(results, function(res) res$test_errors))
)

# Agregar un índice para representar las observaciones
error_data_long$Observacion <- 1:nrow(error_data_long)

# Graficar los errores de prueba
ggplot(error_data_long, aes(x = Observacion, y = Error, group = Componente, color = Componente)) +
  geom_line() +
  labs(title = "Errores del conjunto de prueba por observación y componente",
       x = "Observación", y = "Error de Predicción (kW)") +
  theme_minimal()

