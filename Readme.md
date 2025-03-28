<!-- Si vous souhaitez appliquer un style personnalisé en local, vous pouvez ajouter ce bloc HTML -->
<style>
  body {
    font-size: 16px;
    line-height: 1.6;
  }
  h1 { font-size: 2.5em; }
  h2 { font-size: 2em; }
  h3 { font-size: 1.75em; }
  ul {
    list-style-type: disc;
    margin-left: 1.5em;
  }
  code {
    background-color: #f4f4f4;
    padding: 2px 4px;
    border-radius: 4px;
    font-family: Consolas, Monaco, 'Andale Mono', 'Ubuntu Mono', monospace;
  }
  pre {
    background-color: #f4f4f4;
    padding: 10px;
    border-radius: 4px;
    overflow-x: auto;
  }
</style>

# Application Shiny d'Analyse de la Santé Mentale des Jeunes

## Contexte et Statistiques Clés

La santé mentale des jeunes est un enjeu crucial aujourd'hui. Des études récentes montrent que :

- **20%** des jeunes français (15-24 ans) se déclarent régulièrement anxieux.
- **1 étudiant sur 4** vit un stress important quotidiennement.
- Les jeunes issus de milieux défavorisés présentent un risque accru de **40%** de troubles psychologiques.
- **70%** des victimes de harcèlement souffrent de troubles mentaux significatifs.

Ces chiffres démontrent l'urgence d'intervenir et la nécessité de développer des outils permettant d'identifier rapidement les signes de détresse. C'est dans ce contexte que s'inscrit notre projet.

## Présentation du Projet

L'objectif de cette application Shiny est de fournir aux enseignants un outil d'aide à la décision permettant de détecter, via un questionnaire de santé, plusieurs dimensions clés de la santé mentale chez les jeunes :

- **sm1** — Bien-être mental général  
- **sm2a** — Sentiments de solitude, anxiété et troubles du sommeil  
- **sm3** — Mal-être psychologique prolongé  
- **sm6** — Pensées suicidaires  

Grâce à l'analyse des réponses et à des modèles de machine learning (régression logistique et SVM), l'application prédit le niveau de bien-être ou de détresse psychologique, facilitant ainsi une intervention rapide en milieu scolaire.

## Rubriques de l'Application Shiny

L'application est organisée en plusieurs onglets, chacun dédié à une partie spécifique de l'analyse :

- **Accueil**  
  Présentation du projet, des statistiques clés et de l'importance du thème, avec des visuels percutants et des liens vers des sources fiables (Santé Publique France, Ministère de la Santé, etc.).

- **Objectif du Projet**  
  Description détaillée du thème, des hypothèses à tester, et des variables d’intérêt et explicatives.

- **Dataset**  
  Présentation du dataset utilisé (Baromètre Santé Jeunes 2019 en Nouvelle-Calédonie), avec un tableau interactif pour explorer les données brutes.

- **Analyse Univariée**  
  Visualisation des distributions de chaque variable via des graphiques générés avec `ggplot2`, permettant une analyse rapide des tendances.

- **Résultats**  
  Interface de saisie du questionnaire et affichage en temps réel des prédictions issues des modèles (sm1, sm2a, sm3, sm6).

- **Explications**  
  Détail des modèles prédictifs et interprétation des coefficients (transformés en odds ratios), avec des recommandations pour l'intervention.

## Partie 2 : Détection et Analyse

Dans cette section, l'application met en œuvre des techniques de modélisation prédictive pour détecter automatiquement plusieurs dimensions clés de la santé mentale :

- **Détection SM1 (Bien-être mental général)**  
  Utilisation d'une régression logistique pour estimer le niveau de bien-être global des étudiants.
  
- **Détection SM3 (Mal-être psychologique prolongé)**  
  Application d'une régression logistique pour repérer les signes de mal-être prolongé.
  
- **Détection SM2a et SM6 (Solitude, anxiété, troubles du sommeil et pensées suicidaires)**  
  Emploi de modèles SVM pour identifier les signes liés à la solitude, à l'anxiété et aux pensées suicidaires.

Ces approches permettent d'exploiter efficacement les données issues du questionnaire pour fournir des prédictions précises et faciliter la détection précoce des troubles.

## Partie 3 : Interprétation et Recommandations

Les résultats obtenus par les modèles sont présentés de manière claire et interprétable :

- **Interprétation des Coefficients**  
  Les coefficients des modèles sont transformés en odds ratios pour mesurer l’impact de chaque variable. Par exemple, un coefficient positif indique une augmentation de la probabilité d’un état de bien-être ou, inversement, d’un état de mal-être.
  
- **Recommandations**  
  - En cas de détection de pensées suicidaires (sm6), il est conseillé d’envisager un soutien psychologique immédiat.  
  - Une probabilité élevée de solitude ou d’anxiété (sm2a) suggère de prendre contact avec un professionnel pour évaluer la situation.
  - Les résultats pour le bien-être général (sm1) et le mal-être prolongé (sm3) offrent des indications claires pour orienter les interventions adaptées.

Cette analyse détaillée aide les enseignants à mieux comprendre les signaux d’alerte et à agir en conséquence pour soutenir leurs élèves.

## Présentation de l'Équipe

Ce projet a été réalisé par deux membres passionnés par la data science et la santé publique :

- **Yiré Asma SORO**  
  Responsable de l'analyse de données et de la modélisation prédictive.  
  Profil LinkedIn : [Yiré Asma SORO](https://www.linkedin.com)

- **Iyed DAMMAK**  
  En charge du développement de l'application Shiny et de l'interface utilisateur.  
  Profil LinkedIn : [Iyed DAMMAK](https://www.linkedin.com)

## Technologies et Outils Utilisés

- **Langage R & Shiny** :  
  - `shiny` et `shinydashboard` pour la création de l'application web interactive.  
  - `ggplot2` pour la visualisation des données.  
  - `DT` pour la gestion des tableaux interactifs.  
  - `readxl` pour l'import des données.

- **Machine Learning** :  
  - Modèles SVM et régressions logistiques via `parsnip` et `workflows` pour la prédiction des indicateurs de santé mentale.




## Configuration du Projet

- **Clonage ou téléchargement du dépôt du projet.**
- **Placer le dataset `dataset_complet1.xlsx` ainsi que les fichiers modèles**  
  `(svm_model_sm6.rds, svm_model_sm2a.rds, log_reg_model_sm1.rds, log_reg_model_sm3.rds)`  
  **dans le même répertoire que le fichier `app.R`.**

## Lancement de l'Application
1. **Prérequis** :  
   Installer R, RStudio et les packages nécessaires. Vous pouvez installer les packages via :
   ```r
   install.packages(c("shiny", "shinydashboard", "DT", "readxl", "ggplot2", "parsnip", "workflows", "data.table"))
   shiny::runApp("app.R")



## Impact et Potentiel

**Aide à la Décision**  
Permet aux enseignants d'identifier rapidement les signaux de détresse et d'intervenir de manière appropriée.

**Exploitation de Données Réelles**  
Basée sur le Baromètre Santé Jeunes 2019 en Nouvelle-Calédonie, garantissant ainsi la robustesse des analyses.

**Démonstration de Compétences Techniques**  
Intégration d'une interface web interactive, de visualisations avancées et de modèles de machine learning, illustrant une expertise recherchée en data science et en santé publique.

## Conclusion

Ce projet offre une solution innovante pour la détection précoce des troubles de la santé mentale chez les jeunes. En combinant une interface utilisateur moderne, des analyses statistiques poussées et des modèles de machine learning, il constitue un outil précieux pour le secteur éducatif et la santé publique, tout en démontrant une expertise technique et une compréhension approfondie des enjeux liés à la santé mentale.
