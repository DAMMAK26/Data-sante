library(shiny)
library(shinydashboard)
library(DT)
library(readxl)

data <- read_excel("dataset_complet.xlsx")

# Interface utilisateur (UI)
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = "Présentation du Projet"),
  dashboardSidebar(sidebarMenu(
    menuItem("Accueil", tabName = "accueil", icon = icon("home")),
    menuItem("Objectif du Projet",tabName = "objectif",icon = icon("bullseye")),
    menuItem("Dataset", tabName = "dataset", icon = icon("table")),
    menuItem("Participants", tabName = "participants", icon = icon("users"))
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
      # Onglet Objectif du Projet
      tabItem(tabName = "objectif", fluidRow(
        box(
          title = "Objectifs du Projet",
          width = 12,
          status = "success",
          solidHeader = TRUE,
          tags$p(
            "L'objectif principal du projet est d'explorer et de prC)dire l'C)tat mental des C)tudiants en fonction de divers facteurs :"
          ),
          tags$ul(
            tags$li(
              "Analyser l'influence des conditions familiales et socio-dC)mographiques sur la santC) mentale."
            ),
            tags$li(
              "Etudier l'impact des habitudes de vie, comme le temps passC) devant les C)crans et l'activitC) physique."
            ),
            tags$li(
              "Utiliser des modC(les prC)dictifs pour identifier les facteurs clC)s influenC'ant le bien-C*tre mental."
            )
          )
        )
      )),

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
          status = "warning",
          solidHeader = TRUE,
          color = "green",
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
  output$data_table <- renderDT({
    datatable(data, options = list(pageLength = 10))
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
