# Charger les bibliothèques nécessaires
library(tidymodels)
library(data.table)
library(readxl)

# Charger les données
data <- read_excel('dataset_complet2.xlsx')
df <- as.data.table(data)
df <- df[complete.cases(df), ]



# Liste des variables d'intérêt
columns_of_interest <- c(
  'sm2a', 'sm2b', 'sm2c', 'sm1', 'sm3', 'sm6', 
  'situation_fin', 'ecole_love','result5ts_s3ol',	'sante',	'etat_corps',
  'vf2_1', 'vf2_2', 'vf2_3', 'vf2_4', 'vf2_5', 'vf2_6', 'vf2_7', 'vf4a', 'vf4b', 
  'secu_scol', 'absence_scol', 'violence_scol', 
  'al4_1', 'al4_3', 'heure_sport_extra', 'sport_amusement', 'sport_sante', 'jour_sport',
  'tb1', 'tb2', 'ao1', 'cn1', 'sd1', 'vi1','vi5','tb8'
)

# Filtrer le dataframe pour ne garder que les colonnes d'intérêt
df_filtered <- df[, ..columns_of_interest]
table(df_filtered$sm3)




# Split dataset (Train/Test)
df_split <- initial_split(df_filtered, prop = 0.8)
train <- training(df_split)
test <- testing(df_split)

# Liste des variables cibles

target_columns <- c( 'sm2a', 'sm2b', 'sm2c', 'sm1', 'sm3', 'sm6' )
train[, (target_columns) := lapply(.SD, as.factor), .SDcols = target_columns]
test[, (target_columns) := lapply(.SD, as.factor), .SDcols = target_columns]
# Liste des variables explicatives
predictor_columns <- c(
  'vf2_1', 'vf2_2', 'vf2_3', 'vf2_4', 'vf2_5', 'vf2_6', 'vf2_7', 'vf4a', 'vf4b', 
  'situation_fin', 'ecole_love','result5ts_s3ol',	'sante',	'etat_corps',
  'secu_scol', 'absence_scol', 'violence_scol', 
  'al4_1', 'al4_3', 'heure_sport_extra', 'sport_amusement', 'sport_sante', 'jour_sport',
  'tb1', 'tb2', 'ao1', 'cn1', 'sd1','vi1','vi5','tb8'
)
# Convertir les variables explicatives en numériques si nécessaire
train[, (predictor_columns) := lapply(.SD, as.factor), .SDcols = predictor_columns]
test[, (predictor_columns) := lapply(.SD, as.factor), .SDcols = predictor_columns]






# Fonction d'entraînement du modèle
train_rf_model <- function(target_column, train ,test) {
  # Convertir les variables explicatives en numériques si nécessaire
  #train[, (predictor_columns) := lapply(.SD, as.numeric), .SDcols = predictor_columns]
  #test[, (predictor_columns) := lapply(.SD, as.numeric), .SDcols = predictor_columns]
  
  # Convertir la variable cible en facteur dans l'ensemble d'entraînement et de test
  train[[target_column]] <- as.factor(train[[target_column]])
  test[[target_column]] <- as.factor(test[[target_column]])
  
  # Assurez-vous que les niveaux dans l'ensemble de test sont les mêmes que ceux dans l'ensemble d'entraînement
  test[[target_column]] <- factor(test[[target_column]], levels = levels(train[[target_column]]))
  test[[target_column]] <- factor(test[[target_column]], levels = levels(train[[target_column]]))
  
  # Convertir la variable cible en facteur (car c'est une classification multiclasse)
  train[[target_column]] <- as.factor(train[[target_column]])
  # Créer la formule du modèle
  formula <- as.formula(paste(target_column, "~", paste(predictor_columns, collapse = " + ")))
  
  
  
  
  
  rf <- rand_forest(mtry = tune(), trees = 500) %>% set_engine("ranger") %>% set_mode("classification")
  rf_wf <- workflow() %>% add_model(rf) %>% add_formula(formula)
  cv_folds <- vfold_cv(train, v = 5, strata = target_column)
  rf_results <- tune_grid(rf_wf, resamples = cv_folds, grid = 10, control = control_grid())
  best_rf <- select_best(rf_results, metric = "accuracy")
  final_rf <- finalize_workflow(rf_wf, best_rf)
  trained_rf <- fit(final_rf, data = train)
  
  
  
  return(trained_rf)
} 

# Fonction d'entraînement du modèle
train_xgb_model <- function(target_column, train ,test) {
  # Convertir les variables explicatives en numériques si nécessaire
  #train[, (predictor_columns) := lapply(.SD, as.numeric), .SDcols = predictor_columns]
  #test[, (predictor_columns) := lapply(.SD, as.numeric), .SDcols = predictor_columns]
  
  # Convertir la variable cible en facteur dans l'ensemble d'entraînement et de test
  
  
  # Assurez-vous que les niveaux dans l'ensemble de test sont les mêmes que ceux dans l'ensemble d'entraînement
  test[[target_column]] <- factor(test[[target_column]], levels = levels(train[[target_column]]))
  test[[target_column]] <- factor(test[[target_column]], levels = levels(train[[target_column]]))
  
  # Convertir la variable cible en facteur (car c'est une classification multiclasse)
  train[[target_column]] <- as.factor(train[[target_column]])
  # Créer la formule du modèle
  formula <- as.formula(paste(target_column, "~", paste(predictor_columns, collapse = " + ")))
  
  xgb <- boost_tree(mtry = tune(), trees = 500, learn_rate = tune()) %>% set_engine("xgboost") %>%    set_mode("classification")
  xgb_wf <- workflow() %>% add_model(xgb) %>% add_formula(formula)
  cv_folds <- vfold_cv(train, v = 5, strata = target_column)
  xgb_results <- tune_grid(xgb_wf, resamples = cv_folds, grid = 10, control = control_grid())
  best_xgb <- select_best(xgb_results, metric = "accuracy")
  final_xgb <- finalize_workflow(xgb_wf, best_xgb)
  trained_xgb <- fit(final_xgb, data = train)
  
  return(trained_xgb)
}



# Fonction d'entraînement du modèle
train_reg_model <- function(target_column, train ,test) {
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

# Fonction d'entraînement du modèle
train_svm_model <- function(target_column, train ,test) {
  # Convertir les variables explicatives en numériques si nécessaire
  #train[, (predictor_columns) := lapply(.SD, as.numeric), .SDcols = predictor_columns]
  #test[, (predictor_columns) := lapply(.SD, as.numeric), .SDcols = predictor_columns]
  
  # Convertir la variable cible en facteur dans l'ensemble d'entraînement et de test
  train[[target_column]] <- as.factor(train[[target_column]])
  test[[target_column]] <- as.factor(test[[target_column]])
  
  # Assurez-vous que les niveaux dans l'ensemble de test sont les mêmes que ceux dans l'ensemble d'entraînement
  test[[target_column]] <- factor(test[[target_column]], levels = levels(train[[target_column]]))
  test[[target_column]] <- factor(test[[target_column]], levels = levels(train[[target_column]]))
  
  # Convertir la variable cible en facteur (car c'est une classification multiclasse)
  train[[target_column]] <- as.factor(train[[target_column]])
  # Créer la formule du modèle
  formula <- as.formula(paste(target_column, "~", paste(predictor_columns, collapse = " + ")))
  # Support Vector Machine pour classification multiclasse avec un noyau RBF
  svm_model <- svm_rbf(cost = tune()) %>%
    set_engine("kernlab") %>%
    set_mode("classification")
  # Créer le workflow avec le modèle et la formule
  svm_wf <- workflow() %>%
    add_model(svm_model) %>%
    add_formula(formula)
  # Créer les folds pour la validation croisée
  cv_folds <- vfold_cv(train, v = 5, strata = target_column)
  
  # Appliquer la grille de recherche avec le contrôle correct
  svm_results <- tune_grid(svm_wf, resamples = cv_folds, grid = 10, control = control_grid(verbose = TRUE))
  best_svm <- select_best(svm_results, metric = "accuracy")
  final_svm <- finalize_workflow(svm_wf, best_svm)
  trained_svm <- fit(final_svm, data = train) 
  
  return(trained_svm)
}



get_preds_model <- function (model, target_column){
  the_preds <-predict(model, new_data = test, type = "class")$.pred_class
  return( the_preds)
}


# Fonction de test du modèle et calcul des métriques
test_model <- function(model, target_column) {
  # Prédictions sur l'ensemble de test
  the_preds <- predict(model, new_data = test)$.pred_class
  
  # Convertir la variable cible en facteur et récupérer les prédictions
  test[[target_column]] <- factor(test[[target_column]], levels = levels(the_preds))
  
  # Calculer les métriques de performance
  metrics <- metric_set(accuracy, precision, recall, f_meas)
  
  # Créer un data frame avec les véritables valeurs et les prédictions
  results_df <- tibble(
    truth = test[[target_column]],
    estimate = the_preds
  )
  
  # Calculer les métriques
  the_metrics <- metrics(results_df, truth = truth, estimate = estimate)
  
  return(the_metrics)
}



test_model_on_columns <- function(target_columns, used_model_funtcion){
  
  # Appliquer les fonctions à toutes les variables cibles
  results <- list()
  preds <- list()
  for (target_column in target_columns) {
    # Entraîner le modèle pour chaque variable cible
    trained_model <- used_model_funtcion(target_column)
    preds[[target_column]] <- get_preds_model(trained_model ,target_column )
    # Tester le modèle et récupérer les métriques
    metrics <- test_model(trained_model, target_column)
    
    # Ajouter les résultats au tableau
    results[[target_column]] <- metrics
  }
  
  # Afficher les résultats
  return(list(results = results, preds = preds))
  
}


get_all_models <- function(target_column, train, test) {
  
  # Entraîner chaque modèle
  trained_rf <- train_rf_model(target_column, train, test)
  trained_xgb <- train_xgb_model(target_column, train, test)
  trained_log_reg <- train_reg_model(target_column, train, test)
  trained_svm <- train_svm_model(target_column, train, test)
  
  # Récupérer les prédictions pour chaque modèle
  rf_preds <- get_preds_model(trained_rf, target_column)
  xgb_preds <- get_preds_model(trained_xgb, target_column)
  log_reg_preds <- get_preds_model(trained_log_reg, target_column)
  svm_preds <- get_preds_model(trained_svm, target_column)
  
  # Calculer les métriques de performance pour chaque modèle
  rf_metrics <- test_model(trained_rf, target_column)
  xgb_metrics <- test_model(trained_xgb, target_column)
  log_reg_metrics <- test_model(trained_log_reg, target_column)
  svm_metrics <- test_model(trained_svm, target_column)
  
  # Organiser les résultats dans une liste
  results <- list(
    rf = list(
      model = trained_rf,
      predictions = rf_preds,
      metrics = rf_metrics
    ),
    xgb = list(
      model = trained_xgb,
      predictions = xgb_preds,
      metrics = xgb_metrics
    ),
    log_reg = list(
      model = trained_log_reg,
      predictions = log_reg_preds,
      metrics = log_reg_metrics
    ),
    svm = list(
      model = trained_svm,
      predictions = svm_preds,
      metrics = svm_metrics
    )
  )
  
  return(results)
}



extract_metrics <- function(result) {
  # Extracting the metrics for each model from the result
  metrics_only <- lapply(result, function(model_result) model_result$metrics)
  
  return(metrics_only)
}
extract_preds <- function(result) {
  # Extracting the metrics for each model from the result
  metrics_only <- lapply(result, function(model_result) model_result$predictions)
  
  return(metrics_only)
}

















results_sm6 <- get_all_models('sm6', train, test)
metrics_result_sm6 <- extract_metrics(results_sm6)
metrics_result_sm6

results_sm1 <- get_all_models('sm1', train, test)
metrics_result_sm1 <- extract_metrics(results_sm1)
metrics_result_sm1

results_sm3 <- get_all_models('sm3', train, test)
metrics_result_sm3 <- extract_metrics(results_sm3)
metrics_result_sm3

results_sm2a <- get_all_models('sm2a', train, test)
metrics_result_sm2a <- extract_metrics(results_sm2a)
metrics_result_sm2a

results_sm2b <- get_all_models('sm2b', train, test)
metrics_result_sm2b <- extract_metrics(results_sm2b)
metrics_result_sm2b

results_sm2c <- get_all_models('sm2c', train, test)
metrics_result_sm2c <- extract_metrics(results_sm2c)
metrics_result_sm2c




















# Utiliser la fonction avec un vecteur de colonnes cibles
resultats_et_predictions <- test_model_on_columns(target_columns,train_svm_model )

# Extraire les résultats et les prédictions
results <- resultats_et_predictions$results
preds <- resultats_et_predictions$preds

# Afficher les résultats et les prédictions
results
preds








