library(shiny)
library(shinydashboard)
library(DT)
library(readxl)

data <- read_excel("dataset_complet.xlsx")

# Interface utilisateur (UI)
ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = "Présentation du Projet"),
  dashboardSidebar(sidebarMenu(
    menuItem("Accueil", tabName = "accueil", icon = icon("home")),
    menuItem(
      "Objectif du Projet",
      tabName = "objectif",
      icon = icon("bullseye")
    ),
    menuItem("Participants", tabName = "participants", icon = icon("users")),
    menuItem("Dataset", tabName = "dataset", icon = icon("table"))
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
          "Bienvenue sur notre projet d'analyse de données en santé. Ce projet vise à explorer et prédire l'état mental des étudiants en analysant leurs conditions de vie, leurs habitudes et leur environnement."
        )
      )),
      # Onglet Objectif du Projet
      tabItem(tabName = "objectif", fluidRow(
        box(
          title = "Objectif du Projet",
          width = 12,
          status = "success",
          solidHeader = TRUE,
          tags$p(
            "L'objectif principal du projet est d'explorer et de prédire l'état mental des étudiants en fonction de divers facteurs :"
          ),
          tags$ul(
            tags$li(
              "Analyser l'influence des conditions familiales et socio-démographiques sur la santé mentale."
            ),
            tags$li(
              "Etudier l'impact des habitudes de vie, comme le temps passé devant les écrans et l'activité physique."
            ),
            tags$li(
              "Utiliser des modèles prédictifs pour identifier les facteurs clés influençant le bien-être mental."
            )
          )
        )
      )),
      # Onglet Participants
      tabItem(tabName = "participants", fluidRow(
        box(
          title = "équipe du Projet",
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
                       # Nom au bas de la bannière
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
              margin-top: -70px; /* Faire remonter la photo sur la bannière */
            ",
                         tags$img(
                           src = "background.jpg",
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
                       # Bannière avec une autre image de fond
                       style = "
            background-image: url('background.jpg');
            background-size: cover;
            background-position: center;
            height: 200px;
            position: relative;
            color: white;
            margin-bottom: 0px;
          ",
                       # Nom au bas de la bannière
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
                           src = "background.jpg",
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
      # Onglet Dataset
      tabItem(tabName = "dataset", fluidRow(
        box(
          title = "Aperçu du Dataset",
          width = 12,
          status = "warning",
          solidHeader = TRUE,
          DTOutput("data_table")
        )
      ))
    )
  )
)

# Serveur
server <- function(input, output, session) {
  output$data_table <- renderDT({
    datatable(data, options = list(pageLength = 10))
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
