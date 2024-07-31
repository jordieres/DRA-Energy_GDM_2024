
library(randomForest)

# Function to load models and make predictions
predict_with_models <- function(new_data, model_dir, components) {

  
  # Initialize a list to store predictions
  predictions_list <- list()
  
  # Loop through each component and make predictions
  for (component in components) {
    model_path <- file.path(model_dir, paste0(component, "_rf_model.rds"))
    
    # Load the model
    model <- readRDS(model_path)
    
    # Make predictions
    component_predictions <- predict(model, newdata = new_data)
    
    # Store the predictions in the list
    predictions_list[[component]] <- component_predictions
  }
  
  return(predictions_list)
}


