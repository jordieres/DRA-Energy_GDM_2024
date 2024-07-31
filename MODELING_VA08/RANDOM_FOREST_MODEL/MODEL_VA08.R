library(readxl)
library(mice)
library(randomForest)
library(caret)

rm(list = ls())
# Load the data
datos = read_excel('VA08_final_final_final.xlsx')
datos$Tiempo_produccion_unidad = datos$Tiempo_produccion / datos$Num_productos
datos$Longitud_m_unidad = datos$Longitud_m / datos$Num_productos
datos$Peso_t_unidad = datos$Peso_t / datos$Num_productos
datos$Componente1kW_unidad = datos$Componente1kW / datos$Num_productos
datos$Componente2kW_unidad = datos$Componente2kW / datos$Num_productos
datos$Componente3kW_unidad = datos$Componente3kW / datos$Num_productos
datos$Componente4kW_unidad = datos$Componente4kW / datos$Num_productos
datos$Componente5kW_unidad = datos$Componente5kW / datos$Num_productos
datos$Componente6kW_unidad = datos$Componente6kW / datos$Num_productos

# Replicate each row according to the value of Num_productos
datos <- datos[rep(row.names(datos), datos$Num_productos), ]
rownames(datos) <- NULL

# Subset the data
nuevos_datos = datos[, c(8, 9, 15:17, 19, 20, 24, 34:42)]
nuevos_datos <- subset(nuevos_datos, Componente2kW_unidad < 500 & Componente3kW_unidad < 500 & Componente4kW_unidad < 500)

# Create a directory for saving models
dir.create("modelos_rf", showWarnings = FALSE)

# Define the function to train, evaluate, and save the random forest model using k-fold cross-validation
train_and_evaluate_kfold <- function(data, target_column, k) {
  
  # Prepare the data
  target_name <- paste0(target_column, "_unidad")
  data <- data[, c(1:11, match(target_name, names(data)))]
  
  # Impute missing values in the data
  imputed_data <- mice(data, m = 1, maxit = 50, method = 'pmm', seed = 500)
  data_imputed <- complete(imputed_data)
  
  # Set up k-fold cross-validation
  train_control <- trainControl(method = "cv", number = k)
  
  # Train the random forest model using k-fold cross-validation
  formula <- as.formula(paste(target_name, "~ ."))
  rf_model <- train(formula, data = data_imputed, method = "rf", trControl = train_control, ntree = 110)
  
  # Plot variable importance
  varImpPlot(rf_model$finalModel)
  
  # Calculate MAPE for each fold
  mape <- function(actual, predicted) {
    mean(abs((actual - predicted) / actual) * 100, na.rm = TRUE)
  }
  
  # Get predictions and calculate MAPE
  predictions <- predict(rf_model, newdata = data_imputed)
  mape_value <- mape(data_imputed[[target_name]], predictions)
  
  # Save the model
  model_path <- paste0("modelos_rf/", target_column, "_rf_model.rds")
  saveRDS(rf_model, file = model_path)
  message("Model saved to: ", model_path)
  
  return(list(model = rf_model, mape = mape_value))
}

# Train and evaluate models for each component using k-fold cross-validation
components <- c("Componente1kW", "Componente2kW", "Componente3kW", "Componente4kW", "Componente5kW", "Componente6kW")
results <- lapply(components, function(comp) {
  result <- train_and_evaluate_kfold(nuevos_datos, comp, k = 5)
  message("Trained model for: ", comp)
  return(result)
})

# Display the results
names(results) <- components
results

list.files("modelos_rf")

# Crear un data frame para los MAPE de prueba
mape_data <- data.frame(
  Componente = components,
  MAPE = sapply(results, function(res) res$mape_test)
)

# Graficar los MAPE de prueba por componente
ggplot(mape_data, aes(x = Componente, y = MAPE)) +
  geom_line() +
  geom_point() +
  labs(title = "MAPE del conjunto de prueba por componente",
       x = "Componente", y = "Error absoluto porcentual (%)") +
  theme_minimal()

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
