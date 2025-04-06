# Charger les bibliothèques nécessaires
library(tidymodels)
library(data.table)
library(readxl)

# Charger les données
data <- read_excel("dataset_complet1.xlsx")
df <- as.data.table(data)

# Liste des variables d'intérêt pour l'impact de la perception des risques et comportements de santé
columns_of_interest <- c(
    'ss2',  # Comportements préventifs
  'sante','tb1',   # Anxiété de santé
  'pr1a'	,'pr1b'	,'pr1c',	'pr1d','pr1e','pr1f','pr1g',	'pr1h', 'pr2a', 	'pr2b'	,'pr2c'	,'pr2d'	, 'pr2e'	,'pr2f'	,'pr2g',	'pr2h',  # Perception des risques environnementaux
  'alimentation_saine','ptit_dej_semaine','ptit_dej_weekend','al4_1', 'al4_3', 'jour_sport','sport_extra', 'sd1',  # Comportements de santé
   'ao1', 'ao2a', 'cn1',  # Consommation de substances
  'niveau_scol', 'type_ecole', 'vf2_1',	'vf2_2'	,'vf2_3',	'vf2_4',	'vf2_5',	'vf2_6'	,'vf2_7',
  'sexe', 'age'  # Caractéristiques sociodémographiques
)

columns_to_filter <- c(
  'pr1a'	,'pr1b'	,'pr1c',	'pr1d','pr1e','pr1f','pr1g',	'pr1h', 'pr2a', 	'pr2b'	,'pr2c'	,'pr2d'	, 'pr2e'	,'pr2f'	,'pr2g',	'pr2h',  # Perception des risques environnementaux
  'alimentation_saine','ptit_dej_semaine','ptit_dej_weekend','al4_1', 'al4_3', 'jour_sport','sport_extra', 'sd1',  # Comportements de santé
 'ao1', 'ao2a', 'cn1',  # Consommation de substances
  'niveau_scol', 'type_ecole', 'vf2_1',	'vf2_2'	,'vf2_3',	'vf2_4',	'vf2_5',	'vf2_6'	,'vf2_7',
  'sexe', 'age'  # Caractéristiques sociodémographiques
)

# Filtrer le dataframe pour ne garder que les colonnes d'intérêt
df_filtered <- df[, ..columns_of_interest]

## Voir les valeurs dans chacune des colonnes
# Afficher les valeurs uniques pour chaque colonne dans columns_of_interest
unique_values <- lapply(columns_of_interest, function(col) unique(df_filtered[[col]]))

# Afficher les résultats
names(unique_values) <- columns_of_interest
unique_values




# Remplacer les valeurs manquantes (NA) par 0 dans toutes les colonnes de columns_of_interest
df_filtered[, (columns_to_filter) := lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = columns_to_filter]

# Supprimer les lignes avec des valeurs manquantes dans les colonnes d'intérêt restantes
#df_filtered <- df_filtered[complete.cases(df_filtered), ]

# Vérifier que les valeurs manquantes ont été remplacées
sapply(df_filtered[, ..columns_of_interest], function(x) sum(is.na(x)))  # Compter les valeurs NA restantes

# Sauvegarder les données filtrées dans un nouveau fichier Excel
library(writexl)
write_xlsx(df_filtered, "dataset_complet2.xlsx")
