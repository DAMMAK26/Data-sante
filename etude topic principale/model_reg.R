# Charger les bibliothèques nécessaires
library(tidymodels)
library(data.table)
library(readxl)

# Charger les données
data <- read_excel("dataset_complet1.xlsx")
df <- as.data.table(data)
#df <- df[complete.cases(df), ]

# Liste des variables d'intérêt
columns_of_interest <- c(
  'sm2a', 'sm2b', 'sm2c', 'sm1', 'sm3', 'sm6', 
  'vf2_1', 'vf2_2', 'vf2_3', 'vf2_4', 'vf2_5', 'vf2_6', 'vf2_7', 'vf4a', 'vf4b', 
  'secu_scol', 'absence_scol', 'violence_scol', 
  'al4_1', 'al4_3', 'heure_sport_extra', 'sport_amusement', 'sport_sante', 
  'tb1', 'tb2', 'ao1', 'ao2a', 'cn1', 'sd1'
)

# Filtrer le dataframe pour ne garder que les colonnes d'intérêt
df_filtered <- df[, ..columns_of_interest]

# Split dataset (Train/Test)
df_split <- initial_split(df_filtered, prop = 0.8)
train <- training(df_split)
test <- testing(df_split)
# Liste des variables cibles
target_columns <- c('sm2a', 'sm2b', 'sm2c', 'sm1', 'sm3', 'sm6')

# Liste des variables explicatives
predictor_columns <- c('vf2_1', 'vf2_2', 'vf2_3', 'vf2_4', 'vf2_5', 'vf2_6', 'vf2_7', 'vf4a', 
                       'vf4b', 'secu_scol', 'absence_scol', 'violence_scol', 'al4_1', 'al4_3', 
                       'heure_sport_extra', 'sport_amusement', 'sport_sante', 'tb1', 'tb2', 
                       'ao1', 'ao2a', 'cn1', 'sd1')

# Fonction d'entraînement du modèle
train_reg_model <- function(target_column) {
  # Convertir la variable cible en facteur
  train[[target_column]] <- as.factor(train[[target_column]])
  
  # Créer la formule du modèle
  formula <- as.formula(paste(target_column, "~", paste(predictor_columns, collapse = " + ")))
  
  # Créer le modèle de régression logistique multinomiale
  log_reg <- multinom_reg() %>%
    set_engine("nnet") %>%
    set_mode("classification")
  
  # Créer le workflow
  log_reg_wf <- workflow() %>%
    add_model(log_reg) %>%
    add_formula(formula)
  
  # Entraîner le modèle
  trained_log_reg <- fit(log_reg_wf, data = train)
  
  return(trained_log_reg)
}

# Fonction de test du modèle et calcul des métriques
test_model <- function(trained_log_reg, target_column) {
  # Prédictions sur l'ensemble de test
  log_reg_preds <- predict(trained_log_reg, new_data = test)$.pred_class
  
  # Convertir la variable cible en facteur et récupérer les prédictions
  test[[target_column]] <- factor(test[[target_column]], levels = levels(log_reg_preds))
  
  # Calculer les métriques de performance
  metrics <- metric_set(accuracy, precision, recall, f_meas)
  
  # Créer un data frame avec les véritables valeurs et les prédictions
  results_df <- tibble(
    truth = test[[target_column]],
    estimate = log_reg_preds
  )
  
  # Calculer les métriques
  log_reg_metrics <- metrics(results_df, truth = truth, estimate = estimate)
  
  return(log_reg_metrics)
}

test_model_on_colums <- function(target_columns){
  
  # Appliquer les fonctions à toutes les variables cibles
  results <- list()
  
  for (target_column in target_columns) {
    # Entraîner le modèle pour chaque variable cible
    trained_log_reg <- train_reg_model(target_column)
    
    # Tester le modèle et récupérer les métriques
    metrics <- test_model(trained_log_reg, target_column)
    
    # Ajouter les résultats au tableau
    results[[target_column]] <- metrics
  }
  
  # Afficher les résultats
  return (results) 
  
}
results <- test_model_on_colums( target_columns)
results

