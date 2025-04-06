# Charger les bibliothèques nécessaires
install.packages("tidymodels")
install.packages("nnet")

library(tidymodels)
library(data.table)
library(readxl)
library(nnet)

# Charger les données
data <- read_excel("dataset_complet2_ss4.xlsx")
df <- as.data.table(data)
df <- df[complete.cases(df), ]

# Liste des variables d'intérêt
columns_of_interest <- c(
  'ss4', 'ss2', 'sante', 'tb1', 'pr1a', 'pr1b', 'pr1c', 'pr1d', 'pr1e', 'pr1f', 'pr1g', 'pr1h',
  'pr2a', 'pr2b', 'pr2c', 'pr2d', 'pr2e', 'pr2f', 'pr2g', 'pr2h', 'alimentation_saine', 'ptit_dej_semaine',
  'ptit_dej_weekend', 'al4_1', 'al4_3', 'jour_sport', 'sport_extra', 'sd1', 'ao1', 'ao2a', 'cn1',
  'niveau_scol', 'type_ecole', 'vf2_1', 'vf2_2', 'vf2_3', 'vf2_4', 'vf2_5', 'vf2_6', 'vf2_7',
  'sexe', 'age'
)

# Filtrer le dataframe pour ne garder que les colonnes d'intérêt
df_filtered <- df[, ..columns_of_interest]

# Split dataset (Train/Test)
df_split <- initial_split(df_filtered, prop = 0.8)
train <- training(df_split)
test <- testing(df_split)

# Liste des variables cibles
target_columns <- c('ss4')

# Liste des variables explicatives
predictor_columns <- c('ss2', 'sante', 'tb1', 'pr1a', 'pr1b', 'pr1c', 'pr1d', 'pr1e', 'pr1f', 'pr1g', 'pr1h',
                       'pr2a', 'pr2b', 'pr2c', 'pr2d', 'pr2e', 'pr2f', 'pr2g', 'pr2h', 'alimentation_saine',
                       'ptit_dej_semaine', 'ptit_dej_weekend', 'al4_1', 'al4_3', 'jour_sport', 'sport_extra', 'sd1',
                       'ao1', 'ao2a', 'cn1', 'niveau_scol', 'type_ecole', 'vf2_1', 'vf2_2', 'vf2_3', 'vf2_4', 'vf2_5',
                       'vf2_6', 'vf2_7', 'sexe', 'age')

# Convertir les variables explicatives en matrices numériques
train_matrix <- as.matrix(train[, ..predictor_columns])
test_matrix <- as.matrix(test[, ..predictor_columns])

# Convertir la variable cible en un vecteur numérique
train_labels <- as.numeric(train[[target_columns]]) - 1  # Ajuster à 0-based pour nnet
test_labels <- as.numeric(test[[target_columns]]) - 1



# Définir le modèle MLP avec nnet (MLP avec une couche cachée)
mlp_spec <- mlp(hidden_units = 128, penalty = 0.01) %>%
  set_engine("nnet") %>%
  set_mode("classification")

# Créer un workflow avec `tidymodels`
mlp_workflow <- workflow() %>%
  add_model(mlp_spec) %>%
  add_formula(ss4 ~ .)

# Visualiser le workflow
mlp_workflow


# Entraîner le modèle avec le workflow
mlp_fit <- mlp_workflow %>%
  fit(data = train)

# Afficher le modèle entraîné
mlp_fit


