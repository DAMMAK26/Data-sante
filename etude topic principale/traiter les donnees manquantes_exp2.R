# Charger les bibliothèques nécessaires
library(tidymodels)
library(data.table)
library(readxl)

# Charger les données
data <- read_excel("dataset_complet1.xlsx")
df <- as.data.table(data)
# Sélectionner les 10 premières colonnes
cols_interest <- colnames(df)[250:317]

# Calculer les occurrences (y compris NA) sur les 10 premières colonnes
unique_values <- lapply(cols_interest, function(col) {
  as.data.frame(table(df[[col]], useNA = "ifany"))
})

# Donner les noms des colonnes à la liste
names(unique_values) <- cols_interest

# Afficher les résultats
unique_values



# Liste de tes colonnes d'intérêt
cols_interest <- c(
  "communaute_1", "communaute_2", "communaute_3", "communaute_4", "communaute_5",
  "communaute_6", "communaute_7", "communaute_8", "communaute_9", "result5ts_s3ol",
  "accord_poids", "es8", "es9", "es9b", "as3_1", "as3_2", "as3_3", "as3_4", "as3_5",
  "as3_6", "as3_7", "as3_8", "as3_9", "as3_10", "al4_1", "al4_2", "al4_3", "al4_4",
  "al4_5", "al5_6", "al5_7", "heure_sport_extra", "sport_amusement", "sport_detente",
  "sport_amitie", "sport_talent", "sport_competition", "sport_occupation",
  "sport_obligation", "sport_beaute", "sport_perte_poids", "sport_sante",
  "sport_autre", "no_sport_1", "no_sport_2", "no_sport_3", "no_sport_4",
  "no_sport_5", "no_sport_6", "no_sport_7", "no_sport_8", "no_sport_9",
  "no_sport_10", "no_sport_autre", "sm5", "sm6", "sm7", "cv4a", "cv4b",
  "tb2", "tb3", "tb4", "tb5", "tb7", "tb8", "ao2a", "ao2b", "ao3a", "ao3b",
  "ao3c", "ao3d", "ao4", "ao5", "ao6", "ao7", "ao9", "ao10", "ao11",
  "cn2", "cn3", "cn4", "cn6", "cn7", "ad1", "ad2",
  "ss1", "ss2", "ss3", "ss4", "ss5", "ss8", "ss10", "ss11",
  "pr1a", "pr1b", "pr1c", "pr1d", "pr1e", "pr1f", "pr1g", "pr1h",
  "pr2a", "pr2b", "pr2c", "pr2d", "pr2e", "pr2f", "pr2g", "pr2h"
)
# Sélectionner les 30 premières colonnes d'intérêt
cols_30 <- cols_interest[1:30]

# Calculer les fréquences (y compris les NA) sur ces 30 colonnes
frequencies_30 <- lapply(cols_30, function(col) {
  as.data.frame(table(df[[col]], useNA = "ifany"))
})

# Associer les noms des colonnes aux résultats
names(frequencies_30) <- cols_30

# Afficher le résultat
frequencies_30

# Étape 1 : Calculer la fréquence (avec NA) sur toutes les colonnes d'intérêt
frequencies <- lapply(cols_interest, function(col) {
  as.data.frame(table(df[[col]], useNA = "ifany"))
})
names(frequencies) <- cols_interest

# Étape 2 : Sélectionner les colonnes où le nombre de NA est < 300
cols_na_inf_300 <- names(frequencies)[sapply(frequencies, function(freq_table) {
  na_count <- freq_table$Freq[is.na(freq_table$Var1)]
  if (length(na_count) == 0) na_count <- 0
  na_count < 404
})]

# Résultat : noms des colonnes respectant la condition
print(cols_na_inf_300)



# Tes listes
colonnes_avec_na <- c("communaute_1", "communaute_2", "communaute_3", "communaute_4", "communaute_5",
                      "communaute_6", "communaute_7", "communaute_8", "communaute_9", "result5ts_s3ol",
                      "accord_poids", "es8", "es9", "es9b", "as3_1", "as3_2", "as3_3", "as3_4", "as3_5",
                      "as3_6", "as3_7", "as3_8", "as3_9", "as3_10", "al4_1", "al4_2", "al4_3", "al4_4",
                      "al4_5", "al5_6", "al5_7", "heure_sport_extra", "sport_amusement", "sport_detente",
                      "sport_amitie", "sport_talent", "sport_competition", "sport_occupation",
                      "sport_obligation", "sport_beaute", "sport_perte_poids", "sport_sante",
                      "sport_autre", "no_sport_1", "no_sport_2", "no_sport_3", "no_sport_4",
                      "no_sport_5", "no_sport_6", "no_sport_7", "no_sport_8", "no_sport_9",
                      "no_sport_10", "no_sport_autre", "sm5", "sm6", "sm7", "cv4a", "cv4b",
                      "tb2", "tb3", "tb4", "tb5", "tb7", "tb8", "ao2a", "ao2b", "ao3a", "ao3b",
                      "ao3c", "ao3d", "ao4", "ao5", "ao6", "ao7", "ao9", "ao10", "ao11",
                      "cn2", "cn3", "cn4", "cn6", "cn7", "ad1", "ad2",
                      "ss1", "ss2", "ss3", "ss4", "ss5", "ss8", "ss10", "ss11",
                      "pr1a", "pr1b", "pr1c", "pr1d", "pr1e", "pr1f", "pr1g", "pr1h",
                      "pr2a", "pr2b", "pr2c", "pr2d", "pr2e", "pr2f", "pr2g", "pr2h")

moins_de_400_na <- c("communaute_1", "communaute_2", "communaute_3", "communaute_4", "communaute_5",
                     "communaute_6", "communaute_7", "communaute_8", "communaute_9", "result5ts_s3ol",
                     "accord_poids", "sm6", "sm7", "tb8", "ad1", "ad2", "ss2", "ss11",
                     "pr1a", "pr1b", "pr1c", "pr1d", "pr1e", "pr1f", "pr1g", "pr1h")

# Calcul des colonnes avec plus de 400 NA
plus_de_400_na <- setdiff(colonnes_avec_na, moins_de_400_na)

# Résultat
print(plus_de_400_na)

frequencies_30 <- lapply(plus_de_400_na, function(col) {
  as.data.frame(table(df[[col]], useNA = "ifany"))
})
# Associer les noms des colonnes aux résultats
names(frequencies_30) <- plus_de_400_na

# Afficher le résultat
frequencies_30


# Supposons que ton dataframe s'appelle df

# Liste des colonnes avec plus de 400 NA (calculée précédemment)
plus_de_400_na <- setdiff(colonnes_avec_na, moins_de_400_na)

# Exclure ces colonnes de df
colonnes_a_garder <- setdiff(colnames(df), plus_de_400_na)

# Résultat : les noms des colonnes filtrées
print(colonnes_a_garder)



frequencies_30 <- lapply(colonnes_a_garder, function(col) {
  as.data.frame(table(df[[col]], useNA = "ifany"))
})
# Associer les noms des colonnes aux résultats
names(colonnes_a_garder) <- colonnes_a_garder

# Afficher le résultat
frequencies_30


df_filtre <- df[, ..colonnes_a_garder]

df_filtre

# 4. (Facultatif) Garder uniquement les lignes complètes (sans NA)
df_filtre <- df_filtre[complete.cases(df_filtre), ]

# 5. Vérification
print(dim(df_filtre))         # Voir la taille
print(colnames(df_filtre))    # Voir les colonnes restantes

# 6. Recalculer les fréquences sur ce dataframe filtré
frequences_finales <- lapply(colnames(df_filtre), function(col) {
  as.data.frame(table(df_filtre[[col]], useNA = "ifany"))
})
names(frequences_finales) <- colnames(df_filtre)

# 7. Exemple d'affichage d'une colonne
print(frequences_finales)
write_xlsx(df_filtre, "dataset_complet3.xlsx")






#df <- df[1:1000, ]

# Liste des variables d'intérêt
columns_of_interest <- c(
  'sm2a', 'sm2b', 'sm2c', 'sm1', 'sm3', 'sm6', 
  'situation_fin', 'ecole_love','result5ts_s3ol',	'sante',	'etat_corps',
  'vf2_1', 'vf2_2', 'vf2_3', 'vf2_4', 'vf2_5', 'vf2_6', 'vf2_7', 'vf4a', 'vf4b', 
  'secu_scol', 'absence_scol', 'violence_scol', 
  'al4_1', 'al4_3', 'heure_sport_extra', 'sport_amusement', 'sport_sante', 'jour_sport',
  'tb1', 'tb2', 'ao1', 'cn1', 'sd1', 'vi1','vi5','tb8'
)
columns_to_filter <- c(
  'vf2_1', 'vf2_2', 'vf2_3', 'vf2_4', 'vf2_5', 'vf2_6', 'vf2_7', 'vf4a', 'vf4b', 
  'situation_fin', 'ecole_love','result5ts_s3ol',	'sante',	'etat_corps',
  'secu_scol', 'absence_scol', 'violence_scol', 
  'al4_1', 'al4_3', 'heure_sport_extra', 'sport_amusement', 'sport_sante', 'jour_sport',
  'tb1', 'tb2', 'ao1', 'cn1', 'sd1','vi1','vi5','tb8'
)
columns_to_change <- c( 
  'sd1',  'sport_sante', 'sport_amusement', 'heure_sport_extra',  'al4_3','al4_1')
df[, (columns_to_change) := lapply(.SD, function(x) as.integer(as.factor(x)) + 1), .SDcols = columns_to_change]



# Filtrer le dataframe pour ne garder que les colonnes d'intérêt
df_filtered <- df[, ..columns_of_interest]


## Voir les valuers dans chacune des colonnes 

# Afficher les valeurs uniques pour chaque colonne dans columns_of_interest
unique_values <- lapply(columns_of_interest, function(col) unique(df_filtered[[col]]))

# Afficher les résultats
names(unique_values) <- columns_of_interest
unique_values
# Remplacer les valeurs manquante# Remplacer les valeurs manquante# Remplacer les valeurs manquantes (NA) par 0 dans toutes les colonnes de columns_of_interest
df_filtered[, (columns_to_filter) := lapply(.SD, function(x) replace(x, is.na(x), 0)), .SDcols = columns_to_filter]

# Vérifier que les valeurs manquantes ont été remplacées
sapply(df_filtered[, ..columns_of_interest], function(x) sum(is.na(x)))  # Compter les valeurs NA restantes
lapply(df_filtered, table, useNA = "ifany")
library(writexl)
write_xlsx(df_filtered, "dataset_complet2.xlsx")

