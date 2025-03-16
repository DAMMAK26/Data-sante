library(shiny)
library(shinydashboard)
library(DT)
library(readxl)

data <- read_excel("dataset_complet.xlsx")

# Interface utilisateur (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Présentation du Projet"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Accueil", tabName = "accueil", icon = icon("home")),
      menuItem("Objectif du Projet", tabName = "objectif", icon = icon("bullseye")),
      menuItem("Participants", tabName = "participants", icon = icon("users")),
      menuItem("Dataset", tabName = "dataset", icon = icon("table"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$meta(charset = "UTF-8")
    ),
    tabItems(
      # Onglet Accueil
      tabItem(tabName = "accueil",
              fluidRow(
                box(title = "Bienvenue", width = 12, status = "primary", solidHeader = TRUE,
                    "Bienvenue sur notre projet d'analyse de données en santé. Ce projet vise à explorer et prédire l'état mental des étudiants en analysant leurs conditions de vie, leurs habitudes et leur environnement.")
              )
      ),
      # Onglet Objectif du Projet
      tabItem(tabName = "objectif",
              fluidRow(
                box(title = "Objectif du Projet", width = 12, status = "info", solidHeader = TRUE,
                    tags$p("L'objectif principal du projet est d'explorer et de prédire l'état mental des étudiants en fonction de divers facteurs :"),
                    tags$ul(
                      tags$li("Analyser l'influence des conditions familiales et socio-démographiques sur la santé mentale."),
                      tags$li("??tudier l'impact des habitudes de vie, comme le temps passé devant les écrans et l'activité physique."),
                      tags$li("Utiliser des modèles prédictifs pour identifier les facteurs clés influençant le bien-être mental.")
                    )
                )
              )
      ),
      # Onglet Participants
      tabItem(tabName = "participants",
              fluidRow(
                box(title = "équipe du Projet", width = 12, status = "success", solidHeader = TRUE,
                    tags$p("Ce projet est réalisé par l'équipe suivante :"),
                    tags$ul(
                      tags$li("Yiré Asma SORO - 2ème Année ingénieure en TAF DCL"),
                      tags$li("Iyed DAMMAK - 2ème Année ingénieure en TAF MCE"),
                     
                    )
                )
              )
      ),
      # Onglet Dataset
      tabItem(tabName = "dataset",
              fluidRow(
                box(title = "Aper??u du Dataset", width = 12, status = "warning", solidHeader = TRUE,
                    DTOutput("data_table")
                )
              )
      )
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
