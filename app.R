library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(ggplot2)

# Chargement des données
data <- read_excel("dataset_complet1.xlsx")
df <- data

# Liste des variables à explorer 
var_choices <- c("sexe", "niveau_scol", "situation_fin", "VF2", "VF4",
                 "sd1", "jour_sport", "alimentation_saine", "sante",
                 "result5ts_s3ol", "violence_scol", "vi5", "VI7",
                 "tb1", "ao1", "cn1",
                 "sm1", "sm3", "sm6")

# Factoring de variables simples
df$sexe <- factor(df$sexe, levels = c(1, 2), labels = c("Homme", "Femme"))
df$niveau_scol <- factor(df$niveau_scol, 
                         levels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15), 
                         labels = c("6ème", "5ème", "4ème", "3ème", "2nde Générale",
                                    "2nde Technologique", "1ère Générale", "1ère Technologique",
                                    "Terminale Générale", "Terminale Technologique", "2nde Pro",
                                    "1ère Pro", "Terminale Pro", "1ère année de CAP", "2ème année de CAP"))
df$situation_fin <- factor(df$situation_fin, 
                           levels = c(0,1,2,3,4,5), 
                           labels = c("Je ne sais pas", "Pas à l’aise du tout financièrement",
                                      "Très peu à l’aise financièrement", "Moyennement à l’aise financièrement",
                                      "Plutôt à l’aise financièrement", "Très à l'aise financièrement"))
df$sd1 <- factor(df$sd1, 
                 levels = c(0,1,2,3,4,5,6,7), 
                 labels = c("Moins d’une heure par jour", "1 heure par jour", "2 heures par jour",
                            "3 heures par jour", "4 heures par jour", "5 heures par jour",
                            "6 heures par jour", "7 heures par jour ou plus"))
df$jour_sport <- factor(df$jour_sport, 
                        levels = c(0,1,2,3,4,5,6,7), 
                        labels = c("0 jour", "1 jour", "2 jours", "3 jours", "4 jours", "5 jours", "6 jours", "7 jours"))
df$alimentation_saine <- factor(df$alimentation_saine, 
                                levels = c(0,1,2), 
                                labels = c("Je ne sais pas", "oui", "non"))
df$sante <- factor(df$sante, 
                   levels = c(1,2,3,4,5), 
                   labels = c("Très mauvaise", "mauvaise", "Moyenne", "Bonne", "Très bonne"))
df$result5ts_s3ol <- factor(df$result5ts_s3ol, 
                            levels = c(1,2,3,4,5), 
                            labels = c("Je suis parmi les moins bon(ne)s", "Je suis plus faible que la moyenne",
                                       "Je suis dans la moyenne", "Je suis plus fort(e) que la moyenne",
                                       "Je suis parmi les meilleurs"))
df$violence_scol <- factor(df$violence_scol, levels = c(0,1), labels = c("Non", "Oui"))
df$vi5 <- factor(df$vi5, levels = c(1,2), labels = c("Non", "Oui"))
df$tb1 <- factor(df$tb1, levels = c(1,2), labels = c("Non", "Oui"))
df$ao1 <- factor(df$ao1, 
                 levels = c(1,2,3), 
                 labels = c("Non", "Oui, juste une ou deux gorgees pour gouter/tester ou sans faire expres", "Oui, plus que quelques gorgees"))
df$cn1 <- factor(df$cn1, levels = c(1,2), labels = c("Non", "Oui"))
df$sm1 <- factor(df$sm1, 
                 levels = c(0,1,2,3,4), 
                 labels = c("Je ne sais pas", "Très malheureux(se)", "Malheureux(se)", "Heureux(se)", "Très heureux(se)"))
df$sm3 <- factor(df$sm3, 
                 levels = c(1,2,3), 
                 labels = c("Je ne comprends pas", "non", "oui"))
df$sm6 <- factor(df$sm6, levels = c(0,1), labels = c("Non", "Oui"))

# Interface utilisateur (UI)
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "DATA SANTE"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Objectif du Projet", tabName = "objectif", icon = icon("bullseye")),
      menuItem("Dataset", tabName = "dataset", icon = icon("table")),
      menuItem("Analyse univariées", tabName = "analyse", icon = icon("users")),
      menuItem("Résultats", tabName = "Résultats", icon = icon("chart-bar"))
      
    )
  ),
  dashboardBody(
    tags$head(tags$meta(charset = "UTF-8")),
    tabItems(
      # Onglet Accueil
      tabItem(
        tabName = "accueil", 
        fluidRow(
          box(
            title = "Bienvenue",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            "Bienvenue sur notre projet d'analyse de données en santé. Ce projet a été réalisé dans le cadre du cours d'analyse de données de santé afin de dérouler une analyse exploratoire d'un jeu de données dans le domaine de la santé avec les méthodes vues en cours.
            Nous nous sommes intérèssés à la santé mentale des jeunes."
          ),
          box(
            title = "Statistiques Clés sur la santé mentale des jeune",
            width = 12,
            solidHeader = TRUE,
            status = "success",
            column(
              6,
              align = "center",
              div(
                style = "font-size: 30px; font-weight: bold; color: green; text-align: center;",
                p("20%", style = "font-size: 50px;"),
                p("des jeunes français (15-24 ans) se déclarent régulièrement anxieux", 
                  style = "color: black;font-size: 15px; text-align: center")
              ),
              div(
                tags$a("Source : Santé publique France", 
                       href = "https://www.santepubliquefrance.fr", 
                       target = "_blank", 
                       style = "color: green; font-size: 8px; text-decoration: none;")
              ),
              div(
                style = "font-size: 30px; font-weight: bold; color: green; text-align: center;",
                p("1 sur 4", style = "font-size: 50px;"),
                p("étudiants vit un stress important au quotidien", 
                  style = "color: black;font-size: 15px;")
              ),
              div(
                tags$a("Source : Observatoire de la Vie Étudiante", 
                       href = "https://www.ove-national.education.fr", 
                       target = "_blank", 
                       style = "color: green; font-size: 8px; text-decoration: none;")
              ),
              div(
                style = "font-size: 30px; font-weight: bold; color: green; text-align: center;",
                p("40%", style = "font-size: 50px;"),
                p("de risque en plus pour les jeunes issus de milieux défavorisés", 
                  style = "color: black;font-size: 15px;")
              ),
              div(
                tags$a("Source : Ministère de la Santé", 
                       href = "https://solidarites-sante.gouv.fr", 
                       target = "_blank", 
                       style = "color: green; font-size: 8px; text-decoration: none;")
              ),
              div(
                style = "font-size: 30px; font-weight: bold; color: green; text-align: center;",
                p("70%", style = "font-size: 50px;"),
                p("des victimes de harcèlement souffrent de troubles mentaux", 
                  style = "color: black;font-size: 15px;")
              ),
              div(
                tags$a("Source : Santé publique France", 
                       href = "https://www.santepubliquefrance.fr", 
                       target = "_blank", 
                       style = "color: green; font-size: 8px; text-decoration: none;")
              )
            ),
            column(
              6,
              align = "center",
              img(src = "image1.jpg", height = "500px", style = "max-width: 100%;")
            )
          ),
          box(
            title = "Equipe du Projet",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            tags$p("Ce projet est réalisé par:"),
            fluidRow(
              # Profil 1
              column(
                width = 6,
                box(
                  width = 12,
                  title = NULL,
                  solidHeader = FALSE,
                  status = "primary",
                  style = "
                    background-image: url('background.jpg');
                    background-size: cover;
                    background-position: center;
                    height: 200px;
                    position: relative;
                    color: white;
                    margin-bottom: 0px;
                  ",
                  tags$h2(
                    "Yire Asma SORO",
                    style = "position: absolute; bottom: 10px; left: 20px; margin: 0;"
                  )
                ),
                box(
                  width = 12,
                  div(
                    style = "text-align: center; margin-top: -70px;",
                    tags$img(
                      src = "profil_asma.jpg",
                      style = "border-radius: 50%; width: 120px; height: 120px; border: 3px solid white; object-fit: cover;"
                    )
                  ),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$p("LinkedIn : Yire Asma SORO", style = "font-size: 16px; font-weight: bold;")
                  )
                )
              ),
              # Profil 2
              column(
                width = 6,
                box(
                  width = 12,
                  title = NULL,
                  solidHeader = FALSE,
                  status = "primary",
                  style = "
                    background-image: url('background.jpg');
                    background-size: cover;
                    background-position: center;
                    height: 200px;
                    position: relative;
                    color: white;
                    margin-bottom: 0px;
                  ",
                  tags$h2(
                    "Iyed DAMMAK",
                    style = "position: absolute; bottom: 10px; left: 20px; margin: 0;"
                  )
                ),
                box(
                  width = 12,
                  div(
                    style = "text-align: center; margin-top: -70px;",
                    tags$img(
                      src = "profil_iyed.jpg",
                      style = "border-radius: 50%; width: 120px; height: 120px; border: 3px solid white; object-fit: cover;"
                    )
                  ),
                  div(
                    style = "text-align: center; margin-top: 10px;",
                    tags$p("LinkedIn : Iyed Dammak", style = "font-size: 16px; font-weight: bold;")
                  )
                )
              )
            )
          )
        )
      ),
      # Onglet Objectif
      tabItem(
        tabName = "objectif",
        fluidRow(
          box(
            title = "Objectif du Projet",
            width = 12,
            status = "info",
            solidHeader = TRUE,
            HTML("
              <h3>Thème : Santé mentale et facteurs associés</h3>
              <p><strong>Objectif :</strong> Construire un modèle prédictif permettant d’évaluer le risque de troubles mentaux (stress, anxiété, solitude, idées suicidaires) chez les jeunes en fonction de leurs conditions de vie, habitudes et environnement social.</p>
              <h4>Hypothèses à tester</h4>
              <ol>
                <li>Les conditions familiales et économiques influencent significativement la santé mentale des étudiants.
                  <ul>
                    <li>Les jeunes vivant dans des familles en difficulté financière ont plus de risques de se sentir seuls ou anxieux.</li>
                    <li>Une faible implication des parents (suivi scolaire, discussions) est associée à une détresse psychologique plus élevée.</li>
                  </ul>
                </li>
                <li>Le mode de vie et l'hygiène de vie ont un impact direct sur le bien-être mental.
                  <ul>
                    <li>Un manque de sommeil et une forte sédentarité augmentent le risque d’anxiété et de dépression.</li>
                    <li>Une alimentation déséquilibrée et la consommation de substances (tabac, alcool, cannabis) sont corrélées à un mal-être psychologique.</li>
                  </ul>
                </li>
                <li>Les relations sociales et le harcèlement sont des facteurs déterminants de la santé mentale.
                  <ul>
                    <li>Les jeunes victimes de harcèlement (en ligne ou en présentiel) ont plus de risques de développer des troubles mentaux.</li>
                    <li>Un bon réseau social (amis, famille) réduit le risque de solitude et d’idées suicidaires.</li>
                  </ul>
                </li>
              </ol>
              <h4>Variable d’intérêt (cible à prédire)</h4>
              <ul>
                <li>SM1 : Sentiment général de bien-être (très heureux → très malheureux) (Variable Ordinale)</li>
                <li>SM2 : Fréquence des sentiments de solitude, d’anxiété ou de troubles du sommeil (Variable Ordinale)</li>
                <li>SM3 : Épisodes prolongés de tristesse ou de désespoir (Binaire : Oui/Non)</li>
                <li>SM6 : Pensées suicidaires (Binaire : Oui/Non)</li>
              </ul>
              <h4>Variables explicatives (facteurs influents)</h4>
              <ul>
                <li><strong>Variables socio-démographiques :</strong> CS1, CS3, VF1</li>
                <li><strong>Vie familiale :</strong> VF2, VF4</li>
                <li><strong>Mode de vie :</strong> SD1, AP1, AL1, ES1</li>
                <li><strong>Facteurs de stress social et scolaire :</strong> VS2, VS5, VI5, VI7</li>
                <li><strong>Consommation de substances :</strong> TB1, AO1, CN1</li>
              </ul>
              <p>Cliquez sur l'onglet <strong>Analyse univariées</strong> pour explorer les distributions de chaque variable.</p>
            ")
          )
        )
      ),
      # Onglet Analyse univariées
      tabItem(
        tabName = "analyse",
        fluidRow(
          box(
            title = "Analyse Univariée",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 4,
                selectInput("variable", "Choisir une variable :", 
                            choices = var_choices, selected = "sexe")
              ),
              column(
                width = 8,
                plotOutput("univPlot")
              )
            )
          ),
          box(
            title = "Correspondance des variables",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            fluidRow(
              column(
                width = 6,
                HTML("
                  <ul>
                    <li><strong>niveaau_scol :</strong> Niveau scolaire des étudiants</li>
                    <li><strong>Situation_fin :</strong> Ressenti des étudiants sur la situation financière de leur famille</li>
                    <li><strong>VF2 :</strong> Avec quel(s) adulte(s) l'étudiant vit la plupart du temps</li>
                    <li><strong>VF4 :</strong> Conscience et connaissance des parents de l'état de bien-être mental des étudiants</li>
                    <li><strong>sd1 :</strong> Temps quotidien passé en position assise ou allongée (hors sommeil) devant un écran</li>
                    <li><strong>jour_sport :</strong> Nombre de jours de pratique d'au moins 1 heure d'activités sportives</li>
                    <li><strong>alimentation saine :</strong> Ressenti des étudiants sur la qualité de leur alimentation</li>
                    <li><strong>sante :</strong> Perception des étudiants de leur état de santé</li>
                    <li><strong>result5ts_s3ol :</strong> Ressenti des étudiants par rapport aux autres élèves de leur classe sur leur niveau scolaire</li>
                  </ul>
                ")
              ),
              column(
                width = 6,
                HTML("
                  <ul>
                    <li><strong>violence_scol :</strong> TB1, AO1, CN1</li>
                    <li><strong>vi5 :</strong> CS1, CS3, VF1</li>
                    <li><strong>VI7 :</strong> VF2, VF4</li>
                    <li><strong>tb1 :</strong> SD1, AP1, AL1, ES1</li>
                    <li><strong>ao1 :</strong> VS2, VS5, VI5, VI7</li>
                    <li><strong>cn1 :</strong> TB1, AO1, CN1</li>
                    <li><strong>sm1 :</strong> SD1, AP1, AL1, ES1</li>
                    <li><strong>sm3 :</strong> VS2, VS5, VI5, VI7</li>
                    <li><strong>sm6 :</strong> TB1, AO1, CN1</li>
                  </ul>
                ")
              )
            )
          )
        )
      ),
      # Onglet Dataset
      tabItem(
        tabName = "dataset",
        fluidRow(
          HTML(" 
            <h2><strong>Source:</strong></h2> 
            Le <strong>Baromètre Santé Jeune (BSJ)</strong> est une enquête dont l'objectif est d'améliorer nos connaissances sur la santé et les comportements en santé des jeunes scolarisés en collèges et lycées en Nouvelle-Calédonie.
            <br><br>
            Ce jeu de données présente les résultats de la deuxième édition qui a été réalisée, en <strong>2019</strong>, par l'<strong>Agence sanitaire et sociale de Nouvelle-Calédonie (ASS-NC).</strong>
          "),
          fluidRow(
            column(
              width = 4,
              div(
                style = "margin: 10px; padding: 10px;",
                infoBox(
                  title = "Nombre d'observations",
                  value = "3404 Individus",
                  icon = icon("database"),
                  color = "purple",
                  width = NULL,
                  fill = TRUE
                )
              )
            ),
            column(
              width = 4,
              div(
                style = "margin: 10px; padding: 10px;",
                infoBox(
                  title = "Nombre de variables explicatives",
                  value = "16 variables",
                  icon = icon("cogs"),
                  color = "yellow",
                  width = NULL,
                  fill = TRUE
                )
              )
            ),
            column(
              width = 4,
              div(
                style = "margin: 10px; padding: 10px;",
                infoBox(
                  title = "Variables d'intérêt",
                  value = "4 variables liées à la santé mentale",
                  icon = icon("bullseye"),
                  color = "green",
                  width = NULL,
                  fill = TRUE
                )
              )
            )
          ),
          box(
            title = "Aperçu du Dataset",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            DTOutput("data_table")
          )
        )
      ),
      # Onglet Participants
      tabItem(
        tabName = "Résultats",
        fluidRow(
          box(
            title = "Résultats",
            width = 12,
            status = "success",
            solidHeader = TRUE,
            "Les différentes prédictions......."
          )
        )
      )
    )  # Fin tabItems
  )  # Fin dashboardBody
)  # Fin dashboardPage

# Serveur
server <- function(input, output, session) {
  
  # Afficher le dataset avec DT, avec scroll horizontal activé
  output$data_table <- DT::renderDataTable({
    DT::datatable(data, options = list(scrollX = TRUE))
  })
  
  # Analyse univariée pour la variable sélectionnée
  output$univPlot <- renderPlot({
    var <- input$variable
    # Conversion en facteur pour variables catégorielles
    x_data <- factor(df[[var]])
    
    ggplot(df, aes(x = x_data)) +
      geom_bar(fill = "lightgreen", color = "white") +
      labs(title = paste("Distribution de", var), x = var, y = "Fréquence") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
