# Charger les bibliothèques nécessaires
library(tidymodels)
library(data.table)
library(readxl)

# Charger les données
data <- read_excel("dataset_complet2.xlsx")
df <- as.data.table(data)
df <- df[complete.cases(df), ]

# Liste des variables d'intérêt
columns_of_interest <- c(
  'ss2',  # Comportements préventifs
  'sante','tb1',   # Anxiété de santé
  'pr1a'	,'pr1b'	,'pr1c',	'pr1d','pr1e','pr1f','pr1g',	'pr1h', 'pr2a', 	'pr2b'	,'pr2c'	,'pr2d'	, 'pr2e'	,'pr2f'	,'pr2g',	'pr2h',  # Perception des risques environnementaux
  'alimentation_saine','ptit_dej_semaine','ptit_dej_weekend','al4_1', 'al4_3', 'jour_sport','sport_extra', 'sd1',  # Comportements de santé
  'ao1', 'ao2a', 'cn1',  # Consommation de substances
  'niveau_scol', 'type_ecole', 'vf2_1',	'vf2_2'	,'vf2_3',	'vf2_4',	'vf2_5',	'vf2_6'	,'vf2_7',
  'sexe', 'age'  # Caractéristiques sociodémographiques
)


# Filtrer le dataframe pour ne garder que les colonnes d'intérêt
df_filtered <- df[, ..columns_of_interest]

# Split dataset (Train/Test)
df_split <- initial_split(df_filtered, prop = 0.8)
train <- training(df_split)
test <- testing(df_split)
# Liste des variables cibles # Performance scolaire
target_columns <- c('ss2',  # Comportements préventifs
                     'sante','tb1'   # Anxiété de santé
                     )  

# Liste des variables explicatives
predictor_columns <- c(    'pr1a'	,'pr1b'	,'pr1c',	'pr1d','pr1e','pr1f','pr1g',	'pr1h', 'pr2a', 	'pr2b'	,'pr2c'	,'pr2d'	, 'pr2e'	,'pr2f'	,'pr2g',	'pr2h',  # Perception des risques environnementaux
                           'alimentation_saine','ptit_dej_semaine','ptit_dej_weekend','al4_1', 'al4_3', 'jour_sport','sport_extra', 'sd1',  # Comportements de santé
                            'ao1', 'ao2a', 'cn1',  # Consommation de substances
                           'niveau_scol', 'type_ecole', 'vf2_1',	'vf2_2'	,'vf2_3',	'vf2_4',	'vf2_5',	'vf2_6'	,'vf2_7',
                           'sexe', 'age'  # Caractéristiques sociodémographiques
                         )


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
resultats_et_predictions <- test_model_on_columns(target_columns,train_reg_model )

# Extraire les résultats et les prédictions
results <- resultats_et_predictions$results
preds <- resultats_et_predictions$preds

# Afficher les résultats et les prédictions
results
preds
