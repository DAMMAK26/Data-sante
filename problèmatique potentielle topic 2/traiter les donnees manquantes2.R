
# Charger les bibliothèques nécessaires
library(tidymodels)
library(data.table)
library(readxl)

# Charger les données
data <- read_excel("dataset_complet1.xlsx")
df <- as.data.table(data)

# Liste des variables d'intérêt pour l'impact de la vie familiale et scolaire sur le bien-être
columns_of_interest <- c(
  'sm1', 'sm2a','sm2b','sm2c', 'sm3',  # Santé mentale
  'ecole_love', 'resultats_scol', 'absence_scol',  # Performance scolaire
  'vf2_1','vf2_2'	,'vf2_3'	,'vf2_4'	,'vf2_5'	,'vf2_6'	,'vf2_7', 'vf4a', 'vf4b', 'situation_fin',  # Soutien familial
    'secu_scol',  # Climat scolaire
  'violence_scol', 'al4_1', 'al4_3','jour_sport', 'sport_extra',  # Comportements
  'tb1', 'tb2', 'ao1', 'ao2a', 'cn1'  # Comportements de santé
)

columns_to_filter <- c(
  'vf2_1','vf2_2'	,'vf2_3'	,'vf2_4'	,'vf2_5'	,'vf2_6'	,'vf2_7', 'vf4a', 'vf4b', 'situation_fin',  # Soutien familial
  'secu_scol',  # Climat scolaire
  'violence_scol', 'al4_1', 'al4_3','jour_sport', 'sport_extra',  # Comportements
  'tb1', 'tb2', 'ao1', 'ao2a', 'cn1'  # Comportements de santé
)

# Filtrer le dataframe pour ne garder que les colonnes d'intérêt
df_filtered <- df[, ..columns_of_interest]
#
## Voir les valeurs dans chacune des colonnes

# Afficher les valeurs uniques pour chaque colonne dans columns_of_interest
unique_values <- lapply(columns_of_interest, function(col) unique(df_filtered[[col]]))

# Afficher les résultats
names(unique_values) <- columns_of_interest
unique_values

# Remplacer les valeurs manquantes (NA) par 0 dans toutes les colonnes de columns_of_interest
df_filtered[, (columns_to_filter) := lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = columns_to_filter]
df_filtered <- df_filtered[complete.cases(df_filtered), ]
# Vérifier que les valeurs manquantes ont été remplacées
sapply(df_filtered[, ..columns_of_interest], function(x) sum(is.na(x)))  # Compter les valeurs NA restantes
library(writexl)
write_xlsx(df_filtered, "dataset_complet2.xlsx")

