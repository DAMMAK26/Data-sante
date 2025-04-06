# Charger les bibliothèques nécessaires
library(tidymodels)
library(data.table)
library(readxl)
library(keras)
install_tensorflow() 


library(reticulate)
reticulate::py_config()


install_tensorflow()
reticulate::py_install("tensorflow")
library(tensorflow)

# Charger les données
data <- read_excel('dataset_complet2.xlsx')
df <- as.data.table(data)
df <- df[complete.cases(df), ]

# Liste des variables d'intérêt
columns_of_interest <- c(
  'sm2a', 'sm2b', 'sm2c', 'sm1', 'sm3', 'sm6', 
  'situation_fin', 'ecole_love','result5ts_s3ol', 'sante', 'etat_corps',
  'vf2_1', 'vf2_2', 'vf2_3', 'vf2_4', 'vf2_5', 'vf2_6', 'vf2_7', 'vf4a', 'vf4b', 
  'secu_scol', 'absence_scol', 'violence_scol', 
  'al4_1', 'al4_3', 'heure_sport_extra', 'sport_amusement', 'sport_sante', 'jour_sport',
  'tb1', 'tb2', 'ao1', 'cn1', 'sd1', 'vi1','vi5','tb8'
)

# Filtrer le dataframe pour ne garder que les colonnes d'intérêt
df_filtered <- df[, ..columns_of_interest]

# Split dataset (Train/Test)
df_split <- initial_split(df_filtered, prop = 0.8)
train <- training(df_split)
test <- testing(df_split)

# Liste des variables cibles
target_column <- 'sm3'

# Liste des variables explicatives
predictor_columns <- c(
  'vf2_1', 'vf2_2', 'vf2_3', 'vf2_4', 'vf2_5', 'vf2_6', 'vf2_7', 'vf4a', 'vf4b', 
  'situation_fin', 'ecole_love','result5ts_s3ol', 'sante', 'etat_corps',
  'secu_scol', 'absence_scol', 'violence_scol', 
  'al4_1', 'al4_3', 'heure_sport_extra', 'sport_amusement', 'sport_sante', 'jour_sport',
  'tb1', 'tb2', 'ao1', 'cn1', 'sd1','vi1','vi5','tb8'
)

# Convertir les variables explicatives en facteurs si nécessaire
train[, (predictor_columns) := lapply(.SD, as.factor), .SDcols = predictor_columns]
test[, (predictor_columns) := lapply(.SD, as.factor), .SDcols = predictor_columns]

train[, (target_column) := as.factor(sm3)]
test[, (target_column) := as.factor(sm3)]

# Définir le préprocesseur
preprocessor <- recipe(sm3 ~ ., data = train) %>%
  step_dummy(all_nominal(), -all_outcomes())

# Définir le modèle MLP (Multilayer Perceptron)
mlp_model <- mlp(
  mode = "classification",
  hidden_units = 10, # Vous pouvez ajuster ce paramètre
  penalty = 0.01 # Ajoutez une régularisation pour éviter le sur-apprentissage
  ) %>%
  set_engine("keras")%>%
  set_mode("classification")

# Définir le flux de travail
workflow_mlp <- workflow() %>%
  add_recipe(preprocessor) %>%
  add_model(mlp_model)

# Entraîner le modèle
mlp_fit <- fit(workflow_mlp, data = train)

# Faire des prédictions sur l'ensemble de test
predictions <- predict(mlp_fit, new_data = test)



# Convert predictions to a numeric vector (1, 2, or 3)
predictions_vector <- as.integer(predictions$.pred_class)

table(df_filtered)
# Check the result
#predictions_vector

# Convert predictions to a factor (1, 2, or 3)
predictions_factor <- factor(predictions$.pred_class, levels = c(1, 2, 3))

# Check the result
predictions_factor


# Ensure that sm3 (truth) and predictions are factors
sm3_factor <- factor(test$sm3, levels = c(1, 2, 3))  # Make sure the levels are consistent with your predictions
predictions_factor <- factor(predictions$.pred_class, levels = c(1, 2, 3))  # Similarly for predictions

# Use the metrics function from yardstick to compute accuracy, recall, precision, and F1 score
# Metrics: accuracy, precision, recall, F1
accuracy_result <- accuracy(data.frame(truth = sm3_factor, prediction = predictions_factor), truth = "truth", estimate = "prediction")
precision_result <- precision(data.frame(truth = sm3_factor, prediction = predictions_factor), truth = "truth", estimate = "prediction")
recall_result <- recall(data.frame(truth = sm3_factor, prediction = predictions_factor), truth = "truth", estimate = "prediction")
f1_result <- f_meas(data.frame(truth = sm3_factor, prediction = predictions_factor), truth = "truth", estimate = "prediction")

# Print the results
accuracy_result
precision_result
recall_result
f1_result






















sm3_factor <- factor(test$sm3)

# Evaluate the model using metrics
metrics_result <- metrics(data.frame(truth = sm3_factor, prediction = predictions_factor), truth = "truth", estimate = "prediction")
metrics_result


# Évaluer le modèle
metrics <- metrics(predictions_factor, truth =sm3)
print(metrics)

