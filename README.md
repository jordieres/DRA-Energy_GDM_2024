# DRA-Energy_GDM_2024
Industrial Energy forecasting in HDGL use case.

This repository contains scripts and data analysis documents for Miguel Garcia's final grade thesis (TFG). It includes tools for understanding data frames from a European steel corporation, featuring data analysis and modeling code from five of its downstream production lines: 8, 9, 10, 11, and 12. Each script includes in its name the number of the production line it belongs to.
 

## Repository-Contents
In the **PREVIOUS_WORK_VAXX** folder:

  1. **Data_cleaning_VAXX**: script containing code to clean and prepare data to accomplish the analysis of the production line.
  
  2. **Data_breakdown_VAXX**: script containing code for data analysis of the production line and its results. These results are used to understand how the data are structured and how each variable is correlated with others.

In the **MODELING_VAXX**, the are two more folders:

 **RANDOM_FOREST_MODEL**:

  1. *MODEL_VAXX*: this script includes the code for building and validating the random forest model using k-fold cross-validation, as well as calculating error metrics such as mean error and mean absolute percentage error.

  2. *TIME_FUNCTION*: ontains the function capturar_hora to record the current time. This function ensures that the calculated energy consumption is matched with the appropriate energy price, which varies depending on the time of day.

  3. *PRICE_FUNCTION*: the function precio returns the calculated price value for each product.

  4. *ENERGY_PREDICT*: a function called predict_with_models is built, which aims to predict electric consumption using the random forest models.

  5. *COMPUTER_VAXX*:  includes the code to input the parameters of the product for which the energy cost needs to be calculated.

  6. *FINAL_COST_VAXX*: the script includes a final function, called precio_final , derived from the functions in the TIME_FUNCTION, PRICE_FUNCTION, and ENERGY_PREDICT scripts, which calculates the energy cost attributable to each product.

  7. *ERROR_VAXX*: code that aims to prevent spelling errors from affecting the prediction.

 **OTHER_MODELING_TECHNICS**:

  1. *LINEAL_REGRESSION_VAXX*: Script where a linear regression model is built under the same conditions as the random forest model to compare the results.

  2. *XGBOOST_VAXX*:Script where a Extreme Gradient Boost model is built under the same conditions as the random forest model to compare the results.

## Requirements
To run the scripts and reproduce the analyses, you need to have the following R packages installed: 'readxl', 'dplyr', 'caret', 'randomForest', 'openxlsx', 'mice', 'tidyverse', and 'xgboost'.

You can install these packages using the following command in R:

     install.packages(c('readxl', 'dplyr','caret', 'randomForest', 'openxlsx', 'mice', 'tidyverse', 'xgboost' ))


  

