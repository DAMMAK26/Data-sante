library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(ggplot2)

data <- read_excel("dataset_complet1.xlsx")

# Liste des variables à explorer 
var_choices <- c("sexe", "niveau_scol", "situation_fin", "VF2", "VF4",
                 "sd1", "jour_sport", "alimentation_saine", "sante",
                 "result5ts_s3ol", "violence_scol", "vi5", "VI7",
                 "tb1", "ao1", "cn1",
                 "sm1", "sm3", "sm6")

data$sexe <- factor(data$sexe, levels = c(1, 2), labels = c("Homme", "Femme"))
data$niveau_scol <- factor(data$niveau_scol, levels = c(1, 2,3,4,5,6,7,8,9,10,11,12,13,14,15), labels = c("6ème","5ème",
"4ème",
"3ème",
"2nde Générale",
"2nde Technologique",
"1ère Générale",
"1ère Technologique",
"Terminale Générale",
"Terminale Technologique",
"2nde Pro",
"1ère Pro",
"Terminale Pro",
"1ère année de CAP",
"2ème année de CAP"))

data$situation_fin <- factor(data$situation_fin, levels=c(0,1, 2, 3, 4, 5), labels=c("Je ne sais pas","Pas à l’aise du tout financièrement","Très peu à l’aise financièrement","Moyennement à l’aise financièrement","Plutôt à l’aise financièrement","Très à l'aise financièrement"))
data$sd1 <- factor(data$sd1, levels=c(0,1, 2, 3, 4, 5,6,7), labels=c("Moins d’une heure par jour","1 heure par jour","2 heures par jour","3 heures par jour","4 heures par jour","5 heures par jour","6 heures par jour","7 heures par jour ou plus"))



# Interface utilisateur (UI)
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Présentation du Projet"),
  dashboardSidebar(sidebarMenu(
    menuItem("Accueil", tabName = "accueil", icon = icon("home")),
    menuItem("Objectif du Projet",tabName = "objectif",icon = icon("bullseye")),
    menuItem("Dataset", tabName = "dataset", icon = icon("table")),
    menuItem("Analyse univariées", tabName = "analyse", icon = icon("users"))
  )),
  dashboardBody(
    tags$head(tags$meta(charset = "UTF-8")),
    tabItems(
      # Onglet Accueil
      tabItem(tabName = "accueil", fluidRow(
        box(
          title = "Bienvenue",
          width = 12,
          status = "success",
          solidHeader = TRUE,
          "Bienvenue sur notre projet d'analyse de données en santé. Ce projet a été réalisé dans le cadre du cours d'analyse de données de santé afin de dérouler une analyse exploratoire d'un jeu de données dans le domaine de la santé avec les méthodes vues en cours."
        ),
        
        box(
          title = "Equipe du Projet",
          width = 12,
          status = "success",
          solidHeader = TRUE,
          tags$p("Ce projet est réalisé par:"),
          # Conteneur principal
          fluidRow(# ---- PROFIL 1 ----
                   column(
                     width = 6,
                     box(
                       width = 12,
                       title = NULL,
                       solidHeader = FALSE,
                       status = "primary",
                       # Bannière avec image de fond
                       style = "
            background-image: url('background.jpg');
            background-size: cover;
            background-position: center;
            height: 200px;
            position: relative;
            color: white;
            margin-bottom: 0px;
          ",
                       # Nom au bas de la banniC(re
                       tags$h2(
                         "Yire Asma SORO",
                         style = "
              position: absolute;
              bottom: 10px;
              left: 20px;
              margin: 0;
            "
                       )
                     ),
                     # Photo de profil circulaire + lien
                     box(
                       width = 12,
                       div(
                         style = "
              text-align: center;
              margin-top: -70px; 
            ",
                         tags$img(
                           src = "profil_asma.jpg",
                           style = "
                border-radius: 50%;
                width: 120px;
                height: 120px;
                border: 3px solid white;
                object-fit: cover;
              "
                         )
                       ),
                       div(
                         style = "text-align: center; margin-top: 10px;",
                         tags$p("LinkedIn : Yire Asma SORO", style = "font-size: 16px; font-weight: bold;")
                       )
                     )
                   ), # ---- PROFIL 2 ----
                   column(
                     width = 6,
                     box(
                       width = 12,
                       title = NULL,
                       solidHeader = FALSE,
                       status = "primary",
                       # BanniC(re avec une autre image de fond
                       style = "
            background-image: url('background.jpg');
            background-size: cover;
            background-position: center;
            height: 200px;
            position: relative;
            color: white;
            margin-bottom: 0px;
          ",
                       # Nom au bas de la banniC(re
                       tags$h2(
                         "Iyed DAMMAK",
                         style = "
              position: absolute;
              bottom: 10px;
              left: 20px;
              margin: 0;
            "
                       )
                     ),
                     # Photo de profil circulaire + lien
                     box(
                       width = 12,
                       div(
                         style = "
              text-align: center;
              margin-top: -70px;
            ",
                         tags$img(
                           src = "profil_iyed.jpg",
                           style = "
                border-radius: 50%;
                width: 120px;
                height: 120px;
                border: 3px solid white;
                object-fit: cover;
              "
                         )
                       ),
                       div(
                         style = "text-align: center; margin-top: 10px;",
                         tags$p("LinkedIn : Iyed Dammak", style = "font-size: 16px; font-weight: bold;")
                       )
                     )
                   ))
        )
      )),
      # ------------------ Objectif ------------------
      tabItem(tabName = "objectif",
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
                    <p>Cliquez sur l'onglet <strong>Analyse Univariée</strong> pour explorer les distributions de chaque variable.</p>
                  ")
                )
              )
      ),
      
      # ------------------ Analyse Univariée ------------------
      tabItem(tabName = "analyse",
              fluidRow(
                box(
                  title = "Analyse Univariée",
                  width = 12,
                  status = "success",
                  solidHeader = TRUE,
                  fluidRow(
                    column(
                      width = 4,
                      selectInput("variable", "Choisir une variable :", choices = var_choices, selected = "CS1")
                    ),
                    column(
                      width = 8,
                      plotOutput("univPlot")
                    )
                  )
                )
              )
      ),
      

      # Onglet Dataset
      tabItem(tabName = "dataset", fluidRow(
        fluidRow(
          column(width = 4, div(
            style = "margin: 10px; padding: 10px;",
            infoBox(
              title = "Nombre d'observations",
              value = "3404 Individus",
              icon = icon("database"),
              color = "purple",
              width = NULL,
              # Laissez width à NULL pour éviter les conflits
              fill = TRUE
            )
          )),
          column(width = 4, div(
            style = "margin: 10px; padding: 10px;",
            infoBox(
              title = "Nombre de variables explicatives",
              value = "16 variables",
              icon = icon("cogs"),
              color = "yellow",
              width = NULL,
              fill = TRUE
            )
          )),
          column(width = 4, div(
            style = "margin: 10px; padding: 10px;",
            infoBox(
              title = "Variables d'intérêt",
              value = "4 variables liées à la santé mentale",
              icon = icon("bullseye"),
              color = "green",
              width = NULL,
              fill = TRUE
            )
          ))
        )
        ,
        #aperçu du dataset
        box(
          title = "Aperçu du Dataset",
          width = 12,
          status = "success",
          solidHeader = TRUE,
          DTOutput("data_table")
        )
      ))
    )
  ),
  # Onglet Participants
  tabItem(tabName = "participants", fluidRow(
   
  ))
  
)

# Serveur
server <- function(input, output, session) {
  
  # Afficher le dataset avec DT
  output$data_table <- DT::renderDataTable({
    DT::datatable(data)
  })
  
  # Analyse univariée pour la variable sélectionnée
  output$univPlot <- renderPlot({
    var <- input$variable
    # Vérifier si la variable est numérique ou catégorielle
    if(is.numeric(data[[var]])) {
      ggplot(data, aes_string(x = var)) +
        geom_histogram(fill = "skyblue", color = "white", bins = 30) +
        labs(title = paste("Distribution de", var), x = var, y = "valeur") +
        theme_minimal()
    } else {
      ggplot(data, aes_string(x = var)) +
        geom_bar(fill = "lightgreen", color = "white") +
        labs(title = paste("Distribution de", var), x = var, y = "Fréquence") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
