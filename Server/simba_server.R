# -------- Serveur --------

server <- function(input, output, session) {
  
  # Packages
  
  library(stats)
  library(hash)
  library(circlize)
  library(readr)
  library(comprehenr)
  library(ggplot2)
  library(clValid)
  library(raster)
  library(reticulate)
  library(igraph)
  library(stringr)
  library(lubridate)
  library(networkD3)
  library(openxlsx)
  library(plyr)
  library(dplyr)
  library(DataCombine)
  library(data.table)
  library(purrr)
  library(plotly)
  library(arules)
  library(arulesViz)
  library(tidyverse)
  library(RColorBrewer)
  library(OpenImageR)
  library(ggpubr)
  library(gplots)
  library(vcd)
  library(wesanderson)
  library(gmodels)
  library(descr)
  library(plotly)
  library(SciViews)
  library(wordcloud)
  library(extrafont)
  library(gridExtra)
  library(magrittr)
  library(cowplot)
  library(plotfunctions)
  library(draw)
  library(fpc)
  library(sf)
  library(dbscan)
  library(factoextra)
  library(spdep)
  library(ape)
  library(optrees)
  library(dendextend)
  library(FactoMineR)
  library(shinyWidgets)
  library(DT)
  library(networkD3)
  library(htmlwidgets)
  library(shinyalert)
  library(shinyjs)
  library(grDevices)
  library(ROCR)
  
  source("http://addictedtor.free.fr/packages/A2R/lastVersion/R/code.R")
  
  ##  -- Variables globales et événements -- ##
  
  val <- reactiveValues(
      
      # Fichier de séquences
      file_sequences = NULL,
      
      # Fichier filtré
      file_filtre = NULL,
      
      # Fichier matrice de distances
      file_distance = NULL,
      
      # Ontologies à partir des  fichiers
      ontologies = NULL,
      
      # Fichier issu du clustering
      clust_mk = NULL,
      
      # Matrice de distances
      ced = NULL,
      
      # Matrice filtrée
      ced_filtre = NULL,
      
      # Groupes dans le clustering
      groups = NULL,     
      
      #Clustering hiérarchique
      hc = NULL,
      
      # Diagramme de flux
      flow = NULL,
      
      # Données pour le ciel de mots
      data_ciel = NULL
    )
  
  # Filtres colonnes données
  
  myInputs <- reactiveValues(rendered = c(1))
  
  # Données filtrées en temps réel
  
  myData <- reactive({
                inFile_seq <- input$file_sequences
                inFile_ind <- input$file_individus
                if (!(is.null(inFile_seq) | is.null(inFile_ind))) {
          
                    file_seq <- read.csv(
                        inFile_seq$datapath,
                        sep = input$sep_seq,
                        stringsAsFactors = FALSE,
                        encoding = "UTF-8-BOM"
                    )
          
                    file_ind <- read.csv(
                        inFile_ind$datapath,
                        sep = input$sep_ind,
                        stringsAsFactors = FALSE,
                        encoding = "UTF-8-BOM"
                    )
                    
                    if (ncol(file_seq) > 1 && ncol(file_ind) > 1) {
                        tryCatch({
                            val$file_filtre <- merge(file_seq, file_ind, by = "pers_id", all.x = TRUE)
                        },
                    
                    # -- Notification des erreurs -- #
                    error = function(e) {
                        message <- "Fichier(s) incorrect(s).\nAssurez-vous d'avoir \"pers_id\" dans les deux fichiers"
                        shinyalert("Oops!", message, ype = "error")
                        showModal(modalDialog(div(message), easyClose = TRUE))
                        showNotification(paste0(message), type = 'err') 
                        })
                    }
                }
            })
  
  
  ###############
  # Event boutons
  ###############
  source("./Server/simba_server_button.R")
  
  # Bouton de défilement dans l'onglet filtrage -
  
  filterButton(myInputs, myData, input, session, output, val)
  
  # Boutons Onglet filtrage #
  
  output$buttons <- filterUI(myInputs, myData, input, session, output, val)
  
  # Event bouton d'ajout
  
  filterButtonAdd(myInputs, myData, input, session, output, val)
  
  # Event bouton de suppression
  
  filterButtonDel(myInputs, myData, input, session, output, val)
  
  # Event bouton refresh données filtrées
  
  filterButtonRefresh(myInputs, myData, input, session, output, val)
  
  # Ajout automatique de nouveaux filtres
  
  filterButtonAutoAdd(myInputs, myData, input, session, output, val)
  
  # Bouton exporter les données filtrées
  
  output$save_donnees_filtrees <- exportFilteredDate(myInputs, myData, input, session, output, val)
  
  # Bouton exporter les cluster
  
  output$save_cluster <- exportDataCluster(myInputs, myData, input, session, output, val)
  
  
  # - Observe si les fichiers ont été importés - #
  
  observeEvent({input$file_sequences
    input$file_individus}, {
      if(!is.null(input$file_individus)){}
    })
  
  # - Observe et mise à jour selon filtrage - #
  
  observeEvent(c(val$file_filtre, val$ced_filtre), {})
  
  # Fichier matrice de distances
  
  distMatrixObs(myInputs, myData, input, session, output, val)
  
  # Fichier ontologies : recherche les mots clés "stop","move","lieu","accomp"
  
  ontologyObs(myInputs, myData, input, session, output, val)
 
  # - Observer mise à jour ciel de mots - #
  
  observeEvent(val$clust_mk, {
    # print(nrow(val$clust_mk))
    updatePickerInput(
      session,
      "dim_ciel",
      choices = colnames(val$clusterMK),
      options = list(`actions-box` = TRUE)
    )
  })
  
  
  # Event ouverture de l'onglet clustering -> Passage automatique au premier onglet
  
  clusterButton(myInputs, myData, input, session, output, val)
  
  
  ##############
  # Ontologie
  ##############
  
  source("./Server/simba_server_ontology.R")  
  
  
  output$onto <- ontologyRender(myInputs, myData, input, session, output, val)
  
  
  #########################
  # Statistiques séquences
  #########################
  
  source("./Server/simba_server_statsSeq.R")  
  
  # Affichage barplot activités
  output$barplot <- barplotAct(myInputs, myData, input, session, output, val)
  
  # Affichage Zipf law
  output$zipf <- zipfAct(myInputs, myData, input, session, output, val)
  
  # Affichage histogramme des longueurs
  
  output$longueur <- longueurSeq(myInputs, myData, input, session, output, val)
     
  # Affichage chord diagram 
  
   output$flow <- chord(myInputs, myData, input, session, output, val)
   
  # Daily patterns  
   
  #########
  
  #########################
  # Statistiques individus
  #########################
    
  
  source("./Server/simba_server_statsInd.R")  
    
  # Filtrage et Affichage des mosaiques
  
  output$choix_dim_mosaique <- mosaicInd(myInputs, myData, input, session, output, val)
    
   
  #########################
  # Clustering
  #########################
  
  source("./Server/simba_server_cluster.R")  
 
  
  # Mosaiques par cluster
   
  output$mosaiques_clusters <- mosaicCluster(myInputs, myData, input, session, output, val)
  
  # Clustering
  
  output$clusters <- clustHC(myInputs, myData, input, session, output, val)
  
  # Quality summary
  
  output$quality <- qualityTable(myInputs, myData, input, session, output, val)
  
  # Affichage des longueurs de séquences par clusters 
  
  #output$boxplot_length <- renderPlot({
  #  val$clust_mk
  #})
  
  # Affichage du saut d'inertie et de silhouette
  
  output$inertia <- silhouetteInertie(myInputs, myData, input, session, output, val)
  
  # Barre empilées
  
  output$stack <- stackplot(myInputs, myData, input, session, output, val)  
  
  
  #==============#
  # Ciel de mots #
  #==============#
  
  source("./Server/simba_server_ciel.R")  
  
  # Met à jour la table filtrée pour le ciel de mots
  
  observeEvent(input$dim_ciel, {
    dataCiel <- NULL
    if (!is.null(input$dim_ciel)) {
      if (is.null(dataCiel)) {
        dataCiel <-
          val$clust_mk[, colnames(val$clust_mk) %in% input$dim_ciel]
      }
      else {
        dataCiel <- dataCiel[, colnames(dataCiel) %in% input$dim_ciel]
      }
    }
    if (is.null(dataCiel)) {
      dataCiel <- val$clust_mk
    }
    renderUI({
      plotOutput("ciel")
    })
    
    val$data_ciel <- dataCiel
    
  })
  
  output$choix_dim_ciel <- renderUI({
    
    pickerInput(
      "dim_ciel",
      label = "",
      choices = colnames(val$clust_mk),
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "Déselect. tout",
        `select-all-text` = "Sélect. tout",
        `none-selected-text` = "Tout"
      )
    )
  })
  
  output$ciel <- ciel_de_mots(myInputs, myData, input, session, output, val)  
   
}


