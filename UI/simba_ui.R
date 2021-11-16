
# -------- Interface (UI) -------- #


# --- Header --- #

header <- dashboardHeader(title = "SIMBA App")


# --- Sidebar --- #

sidebar <- dashboardSidebar(sidebarMenu(
  
    # L'id permet à input$tabs de donner le nom de l'onglet sélectionné
    id = "tabs",
  
    # - Onglet chargement - #
    menuItem(
        "Chargement des fichiers",
        tabName = "chargement",
        icon = icon("upload")
    ),
  
    # - Onglet Ontologie - #
    menuItem(
        "Ontologie", 
         tabName = "ontologie", 
         icon = icon("hubspot")
    ),
  
    # - Onglet filtrage - #
    menuItem(
        "Filtrer les données",
        tabName = "filtrage",
        icon = icon("filter")
    ),
  
    # - Onglet Statistiques - #
    menuItem(
        "Statistiques Globales",
        icon = icon("bar-chart-o"),
        menuSubItem("Séquences", tabName = "sequences"),
        menuSubItem("Individus", tabName = "individus")
    ),
  
     # - Onglet Clustering - #
    menuItem("Clustering", icon = icon("sitemap"), tabName = "clustering")
),

textOutput("res"))

# - Onglet Chargement des fichiers - #
source("simba_ui_load.R")

# - Onglet Filtrage - #
source("simba_ui_filter.R")

# - Onglet Statistiques - #
source("simba_ui_stats.R")

# - Onglet Ontologie - #
source("simba_ui_ontology.R")

# - Onglet Clustering - #
source("simba_ui_cluster.R")

# --- Body --- #

body <- dashboardBody(
  
    #Rendre la couleur des erreurs en noir (fait moins peur)
    tags$head(tags$style(".shiny-output-error{color: black;}")),
  
  
    # -- Onglets -- #
    tabItems(
        LoadItem,
        OntologyItem,
        FilterItem,
        StatsSeqItem,
        StatsIndItem,
        ClusterItem
    )
)

# --- UI : Combine header, sidebar et body --- #

ui <- dashboardPage(header, sidebar, body)