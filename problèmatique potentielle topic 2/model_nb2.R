# Charger les bibliothèques nécessaires
library(tidymodels)
library(data.table)
library(readxl)
library(discrim)

# Charger les données
data <- read_excel('dataset_complet2.xlsx')
df <- as.data.table(data)
df <- df[complete.cases(df), ]



# Liste des variables d'intérêt
# Liste des variables d'intérêt
columns_of_interest <- c(
  'sm1', 'sm2a','sm2b','sm2c', 'sm3',  # Santé mentale
  'ecole_love', 'resultats_scol', 'absence_scol',  # Performance scolaire
  'vf2_1','vf2_2'	,'vf2_3'	,'vf2_4'	,'vf2_5'	,'vf2_6'	,'vf2_7', 'vf4a', 'vf4b', 'situation_fin',  # Soutien familial
  'secu_scol',  # Climat scolaire
  'violence_scol', 'al4_1', 'al4_3','jour_sport', 'sport_extra',  # Comportements
  'tb1', 'tb2', 'ao1', 'ao2a', 'cn1'  # Comportements de santé
)

# Filtrer le dataframe pour ne garder que les colonnes d'intérêt
df_filtered <- df[, ..columns_of_interest]
#df_filtered <- df_filtered[1:500,]
# Split dataset (Train/Test)
df_split <- initial_split(df_filtered, prop = 0.8)
train <- training(df_split)
test <- testing(df_split)

# Liste des variables cibles

# Liste des variables cibles # Performance scolaire
target_columns <- c( 'sm1', 'sm2a','sm2b','sm2c', 'sm3', 'ecole_love', 'resultats_scol', 'absence_scol' )  

# Liste des variables explicatives
predictor_columns <- c(  'vf2_1','vf2_2'	,'vf2_3'	,'vf2_4'	,'vf2_5'	,'vf2_6'	,'vf2_7', 'vf4a', 'vf4b', 'situation_fin',  # Soutien familial
                         'secu_scol',  # Climat scolaire
                         'violence_scol', 'al4_1', 'al4_3','jour_sport', 'sport_extra',  # Comportements
                         'tb1', 'tb2', 'ao1', 'ao2a', 'cn1'  # Comportements de santé
                         )





# Fonction d'entraînement du modèle
train_nb_model <- function(target_column) {
  # Convertir les variables explicatives en numériques si nécessaire
  train[, (predictor_columns) := lapply(.SD, as.numeric), .SDcols = predictor_columns]
  test[, (predictor_columns) := lapply(.SD, as.numeric), .SDcols = predictor_columns]
  
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
  
  # Naive Bayes
  nb <- naive_Bayes() %>% set_engine("klaR") %>% set_mode("classification")
  nb_wf <- workflow() %>% add_model(nb) %>% add_formula(formula)
  cv_folds <- vfold_cv(train, v = 5, strata = target_column)
  nb_results <- tune_grid(  nb_wf,   resamples = cv_folds, grid = 2,   control = control_grid(verbose = TRUE))
  best_nb <- select_best(nb_results, metric = "accuracy")
  final_nb <- finalize_workflow(nb_wf, best_nb)
  trained_nb <- fit(final_nb, data = train)
  
  
  return(trained_nb)
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


# Utiliser la fonction avec un vecteur de colonnes cibles
resultats_et_predictions <- test_model_on_columns(target_columns,train_nb_model )

# Extraire les résultats et les prédictions
results <- resultats_et_predictions$results
preds <- resultats_et_predictions$preds

# Afficher les résultats et les prédictions
results
preds








