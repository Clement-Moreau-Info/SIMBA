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
  
  # - Variables globales et événements - #
  
  val <-
    reactiveValues(
      
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
      
      file_seq <-        read.csv(
        inFile_seq$datapath,
        sep = input$sep_seq,
        stringsAsFactors = FALSE,
        encoding = "UTF-8-BOM"
      )
      file_ind <-        read.csv(
        inFile_ind$datapath,
        sep = input$sep_ind,
        stringsAsFactors = FALSE,
        encoding = "UTF-8-BOM"
      )
      if (ncol(file_seq) > 1 &&
          ncol(file_ind) > 1) {
        tryCatch({
          val$file_filtre <-
            merge(file_seq, file_ind, by = "pers_id", all.x = TRUE)
          
          
        },
        error = function(e) {
          message <-
            "Fichier(s) incorrect(s).
          \nAssurez-vous d'avoir \"pers_id\" dans les deux fichiers"
          
          shinyalert("Oops!",
                     message,
                     type = "error")
          showModal(modalDialog(div(message)
                                ,easyClose = TRUE))
          showNotification(paste0(message), type = 'err') #Notif en bas à droite
          
        })
      }
    }
  })
  
  # Event bouton de défilement dans l'onglet filtrage
  
  observeEvent(lapply(paste0("drop", myInputs$rendered), function(x)
    input[[x]]), {
      for (i in myInputs$rendered) {
        updatePickerInput(
          session,
          paste0('select', i),
          choices = unique(myData()[input[[paste0('drop', i)]]]),
          selected = input[[paste0("select", i)]],
          options = list(`actions-box` = TRUE)
        )
      }
    })
  
  # Event ouverture de l'onglet clustering -> Passage automatique au premier onglet
  
  observeEvent(input$tabs,{
    
    if(input$tabs == "Clustering"){
      updateTabsetPanel(session, "clustering_tabs",
                        selected = NULL)
    }
    
  })
  
  # Boutons Onglet filtrage #
  
  output$buttons <- renderUI({
    useShinyjs()
    tags$hr()
    tags$div(
      class = "row",
      div(
        class = "col-lg-4",
        
        actionButton(
          inputId = "add",
          label = "Ajouter filtre",
          icon = icon(name = "plus")
        )
      ),
      tags$div(
        class = "col-lg-2",
        actionButton(
          inputId = "delete",
          label = "",
          icon = icon(name = "trash")
        )
      ),
      tags$div(
        class = "col-lg-6",
        actionButton(
          inputId = "calc",
          label = "Afficher/Mettre à jour",
          icon = icon(name = "refresh"),
          class = "btn btn-success",
          style = "color:#fff"
        )
      )
    )
  })
  
  # Event bouton d'ajout
  
  observeEvent(input$add, {
    myInputs$rendered <- c(myInputs$rendered, max(myInputs$rendered) + 1)
  })
  
  # Event bouton de suppression
  
  observeEvent(input$delete, {
    if (length(myInputs$rendered) > 1) {
      removeUI(selector = paste("div:has(> #drop", max(myInputs$rendered), ")", sep =
                                  ""))
      removeUI(selector = paste("div:has(> #select", max(myInputs$rendered), ")", sep =
                                  ""))
      myInputs$rendered <-
        myInputs$rendered[-length(myInputs$rendered)]
    }
  })
  
  # Event bouton refresh données filtrées
  
  observeEvent(input$calc, {
    showData <- NULL
    for (i in 1:length(myInputs$rendered)) {
      if (!is.null(input[[paste0("select", i)]])) {
        if (is.null(showData)) {
          showData <-
            filter(myData(), myData()[, input[[paste0("drop", i)]]] %in% input[[paste0("select", i)]])
        }
        else {
          showData <-
            filter(showData, showData[, input[[paste0("drop", i)]]] %in% input[[paste0("select", i)]])
        }
      }
    }
    if (is.null(showData)) {
      showData <- myData()
    }
    val$file_filtre <- showData
    output$table_filtree <- renderDataTable({
      showData
    })
    
    updateTabsetPanel(session, "clustering",
                      selected = NULL)
    
  })
  
  # Ajout automatique de nouveaux filtres
  
  observe({
    output$inputs <- renderUI({
      rows <- lapply(myInputs$rendered, function(i) {
        fluidRow(
          tags$div(
            class = "col-lg-5",
            selectInput(
              paste0('drop', i),
              label = "",
              choices = colnames(myData()),
              selected = input[[paste0("drop", i)]]
            )
          ),
          tags$div(
            class = "col-lg-7",
            pickerInput(
              paste0('select', i),
              label = "",
              choices = unique(myData()[1]),
              multiple = TRUE,
              options = list(
                `actions-box` = TRUE,
                `deselect-all-text` = "Déselect. tout",
                `select-all-text` = "Sélect. tout",
                `none-selected-text` = "Tout"
              )
            )
          )
        )
        
      })
      do.call(shiny::tagList, rows)
    })
    
    if (!is.null(myData())) {
      output$table_filtree <- renderDataTable({
        myData()
      })
    }
    
  })
  
  # Bouton exporter les données filtrées
  
  output$save_donnees_filtrees <- downloadHandler(
    filename = function() {
      paste(substr(
        input$file_sequences$name,
        1,
        nchar(input$file_sequences$name) - 4
      ),
      "_filtree",
      ".csv",
      sep = "")
    },
    
    content = function(file) {
      
      write.csv2(val$file_filtre,
                 file,
                 sep = input$sep_seq)
    }
  )
  
  output$save_cluster <- downloadHandler(
    filename = function() {
      paste("cluster_", input$cluster_nb,".csv",
      sep = "")
    },
    
    content = function(file) {
      
      write.csv2(val$clust_mk,
                 file,
                 sep = input$sep_seq)
    }
  )
  
  observeEvent({input$file_sequences
    input$file_individus}, {
      if(!is.null(input$file_individus)){}
    })
  
  observeEvent(c(val$file_filtre, val$ced_filtre), {
    
  })
  
  # Fichier matrice de distances
  
  observeEvent(input$file_distances, {
    val$file_distances <- read_delim(
      input$file_distances$datapath,
      col_names = FALSE,
      delim = input$sep_dist,
      escape_double = FALSE,
      trim_ws = TRUE
    )
  })
  
  # Fichier ontologies : recherche les mots clés "stop","move","lieu","accomp"
  
  observeEvent(input$files_onto, {
    tryCatch({
      onto_stop <-         read.csv(
        input$files_onto[grepl("stop", input$files_onto$name),]$datapath,
        sep = input$sep_onto,
        stringsAsFactors = FALSE,
        encoding = "UTF-8-BOM"
      )
      onto_move <-         read.csv(
        input$files_onto[grepl("move", input$files_onto$name),]$datapath,
        sep = input$sep_onto,
        stringsAsFactors = FALSE,
        encoding = "UTF-8-BOM"
      )
      onto_lieu <-         read.csv(
        input$files_onto[grepl("lieu", input$files_onto$name),]$datapath,
        sep = input$sep_onto,
        stringsAsFactors = FALSE,
        encoding = "UTF-8-BOM"
      )
      onto_accomp <-         read.csv(
        input$files_onto[grepl("accomp", input$files_onto$name),]$datapath,
        sep = input$sep_onto,
        stringsAsFactors = FALSE,
        encoding = "UTF-8-BOM"
      )
      val$ontologies <-
        list(
          stop = onto_stop,
          move = onto_move,
          lieu = onto_lieu,
          accomp = onto_accomp
        )
      # print(val$ontologies)
    },
    error = function(e) {
      showNotification(paste(
        "Fichier(s) manquant(s) :",
        nrow(input$files_onto),
        "fichier(s) chargé(s)"
      ),
      type = 'err')
    })
    if (nrow(input$files_onto) == 4) {
      showNotification("Ontologies prêtes", type = "message")
      enable(id = "noms_ciel")
    }
    
  })
  
  observeEvent(val$clust_mk, {
    # print(nrow(val$clust_mk))
    updatePickerInput(
      session,
      "dim_ciel",
      choices = colnames(val$clusterMK),
      options = list(`actions-box` = TRUE)
    )
  })
  
  # - Fonctions - #
  
  # Fonction d'attribution des couleurs selon les ID
  
  col_mk <- function(a) {
    
    # Activités stop #
    
    if (a %in% c(11, 12, 13, 14, 15, 16, 17, 'A')) {
      return("#2A6967") #("#FBD54A")
    }
    if (a %in% c(21, "B1")) {
      return("#4234B9") #("#9B5097")
    }
    if (a %in% c(22, "B2")) {
      return("#2678BF") #("#9B5097")
    }
    if (a %in% c(31, 32, 33, 34, "C")) {
      return("#FCD94B") #("#FFFB00")
    }
    if (a %in% c(41, 42, "D")) {
      return("#DC3439") #("#EC3832")
    }
    if (a %in% c(51, 52, 53, 54, 55, 56, 57, 58, 59, "E", "Ea", "Eb")) {
      return("#E25090") #("#E35090")
    }
    if (a %in% c(61, 62, 63, 64, 65, 66, 67, 68, 69, "F", "Fa", "Fb", "Fc")) {
      return("#F28F3D") #("#C92F31") 995F30
    }
    if (a %in% c(71, 72, 73, "G")) {
      return("#92912B")  #("#2A77AA")
    }
    if (a %in% c(81, 82, 83, 84, "H")) {
      return("#935220")  #("#2A77AA")
    }
    if (a %in% c("I")) {
      return("#6E309F")  #("#2A77AA")
    }
    
    # Modes de transport #
    
    if (a %in% c(101, 102, 103, 104, 'AA')) {
      return("#6FAF89")
    }
    if (a %in% c(111, 112, 113, 114, 121, 122, 123, 'BB', 'BBa', 'BBb')) {
      return("#EE3C48")
    }
    if (a %in% c(131, 132, 133, 134, 135, 'CC')) {
      return("#FFDA62")
    }
    if (a %in% c(141, 'DD')) {
      return("#94989F")
    }
    if (a %in% c('EE')) {
      return("#212121")
    }
    
    # Accompagnements #
    
    if (a %in% c(201)) {
      return("#EC6D88")
    }
    if (a %in% c(202)) {
      return("#FAADC1")
    }
    if (a %in% c(211)) {
      return("#BF077D")
    }
    if (a %in% c(212)) {
      return("#DA89B6")
    }
    if (a %in% c('AAA')) {
      return("#ff4777")
    }
    if (a %in% c('BBB')) {
      return("#76003F")
    }
    if (a %in% c('CCC')) {
      return("#76003F")
    }
    if (a %in% c('DDD')) {
      return("#76003F")
    }
    if (a %in% c('EEE')) {
      return("#E75CE3")
    }
    if (a %in% c('FFF')) {
      return("#420178")
    }
    
    # Lieux #
    
    if (a %in% c('AAAA')) {
      return("#92DDC3")
    }
    if (a %in% c('BBBB1', 321)) {
      return("#547AF1")
    }
    if (a %in% c('BBBB2', 322)) {
      return("#3CA4E1")
    }
    if (a %in% c('CCCC')) {
      return("#FEF7A7")
    }
    if (a %in% c('DDDD')) {
      return("#FF7B8F")
    }
    if (a %in% c(351, 352, 354, 'EEEE')) {
      return("#F48AF7")
    }
    if (a %in% c('FFFF')) {
      return("#F4B661")
    }
    if (a %in% c(383, 384, 'HHHH')) {
      return("#C38A45")
    }
    if (a %in% c('IIII')) {
      return("#A764F0")
    }
    
    # NULL #
    if (a %in% 'NULL'){
      return("#a3a3a3")
    }
    
    #Autres valeurs : choisit une couleur au hasard
    
    else {
      qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
      col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
      color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = TRUE)]
      return(sample(col_vector, 1))
    }
  }
  
  # Fonction d'agrégation par une variable varname (Non utilisée)
  # Return dataFrame du groupBy avec moyenne et sd
  
  # data_summary <- function(data, varname, groupnames) {
  #   require(plyr)
  #   
  #   summary_func <- function(x, col) {
  #     c(mean = mean(x[[col]], na.rm = TRUE),
  #       sd = sd(x[[col]], na.rm = TRUE))
  #   }
  #   data_sum <-
  #     ddply(data, groupnames, .fun = summary_func, varname)
  #   data_sum <- plyr::rename(data_sum, c("mean" = varname))
  #   return(data_sum)
  # }
  
  
  # Fonction d'agrégation des ID lorsque les ontologies sont fournies
  
  agg_id <- function(id) {
    
    for (ont in val$ontologies) {
      if (!is.null(ont)) {
        if (any(ont[, "ID"] == id[1])) {
          if (!grepl("NULL", ont[ont[,"ID"] == ont[ont[,"ID"]== id[1],][,"Père"][1],][,"Name"])) {
            id <- ont[ont[, "ID"] == id[1], ][, "Père"][1]
          }
        }
      }
    }
    return(as.character(id[1]))
  }
  
  # Fonction d'attribution des noms en fonctions des ID, quand l'ontologie est fournie
  # L'ontologie doit comporter une colonne "ID" et "Name"
  
  name_id <- function(id){
    for (ont in val$ontologies) {
      if (!is.null(ont)) {
        if (any(ont[, "ID"] == id[1])) {
          id <- ont[ont[, "ID"] == id[1], ][, "Name"]
        }
      }
    }
    return(as.character(id[1]))
  }
  
  # Fonction de répartition des fréquences des activités (barplot)
  
  freq_mk <- function(vect) {
    
    x <- unlist(str_split(vect, " ")) # Split des multi-activités
    # print(x)
    # print("---")
    df <- as.data.frame(table(x[x != "NULL"]))
    # print(df)
    # print("---")
    colnames(df) <- c("activity", "freq")
    df <- df[order(-df$freq), ]
    df$activity <- as.character(df$activity)
    df$Name <- as.character(lapply(df$activity,name_id))
    df$Color <- as.character(lapply(df$activity,col_mk))
    df$Group <- as.character(lapply(as.character(lapply(df[,"activity"],agg_id)),name_id))
    par(mar=c(2,0,0,0))
  
    df$activity <- factor(df$activity, levels = unique(df$activity)[order(df$freq, decreasing = TRUE)])
    fig <- plot_ly(df, 
                   x = ~activity, 
                   y = ~freq,
                   marker = list(color = df$Color),
                   hoverinfo = "text",
                   text = paste(df$Group,': ', df$Name, ' (', df$freq,')'),
                   type = "bar"
                   )
    fig <- layout(fig,yaxis = list(type = "log", title = "Fréquence"),
                  xaxis= list(title = "", showticklabels = FALSE))
    fig
    
    
    # print(df)
    # print("---")
    
    #bp<-barplot(
    #  las = 3,
      # legend = unique(df$Group),
      # args.legend = list(bty = "n", inset = c(-0.15, 0)),
    #  df[, "freq"],
    #  col = df$Color,
      # log = "y",
      # main = titre_barplot,
    #  cex.names = 0.85
    #)
    #text(bp, par("usr")[3], labels = df[,"Name"], srt = 40, adj = c(1.1,1.1), xpd = TRUE, cex=0.85) 
    #legend("topright", 
    #       legend = unique(df$Group), 
    #       fill = unique(unlist(lapply(df[,"activity"], col_mk))),
    #       bty = "n", 
    #       pt.cex = 2, 
    #       cex = 1, 
    #       text.col = "black", 
    #       horiz = F , 
    #       inset = c(0.1, 0.1))
    
    
    
  }
  
  
  
  # Onglet Ontologies #
  
  output$onto <- renderForceNetwork({
    
    # Fonction pour créer la colonne couleur au préalable :
    #
    #   onto$Couleur <- as.character(lapply(onto$ID,col_mk))
    
    
    # Lecture des fichiers des ontologies
    
    req(val$ontologies)
    tryCatch({
      switch(
        input$radio_onto,
        "1" = {
          onto <- val$ontologies[["stop"]]
        },
        "2" = {
          onto <- val$ontologies[["move"]]
        },
        "3" = {
          onto <- val$ontologies[["lieu"]]
        },
        "4" = {
          onto <- val$ontologies[["accomp"]]
        }
      )
      
      # print(onto)
      
      
    },
    error = function(e) {
      message <- "Ontologie(s) manquante(s)."
      shinyalert("Oops!", message , type = "error")
      showModal(modalDialog(div(message), easyClose = TRUE))
      stop(safeError(message))
      
    })
    
    defaultW <- getOption("warn")
    options(warn = -1)
    
    # Attribution d'une colonne "Groupe" 
    
    onto[, "Groupe"] <-
      as.character(lapply(onto[, "ID"], function(x) {
        if (is.na(as.numeric(x))) {
          group <<- onto[onto[, "ID"] == x,][, "Name"][1]
        }
        return(group)
      }))
    
    # Attribution d'une colonne "Taille" pour l'affichage des noeuds de l'ontologie.
    # La taille du noeud correspond à sa position dans la hiérarchie
    
    onto[, "Taille"] <-
      as.numeric(lapply(onto[, "ID"], function(x) {
        if (grepl("NULL", onto[onto["ID"] == x, ][, "Name"][1])) {
          return(50)
        } else{
          if (x %in% onto[, "Père"] |
              grepl("NULL", onto[onto[, "ID"] %in% onto[onto[, "ID"] == x, ][, "Père"], ][, "Name"])) {
            return(20)
          } else{
            return(1)
          }
        }
      }))
    
    options(warn = defaultW)
    
    # Attribution d'un ID de noeud
    
    onto[, "nb"] <- as.numeric(lapply(onto[, "ID"], function(x) {
      return(which(unique(onto[, "ID"]) == x) - 1)
    }))
    
    onto <- onto[order(onto[, "nb"], decreasing = FALSE), ]
    
    # Attribution d'un ID du/des noeud(s) père(s)
    
    onto[, "nbPere"] <-
      as.numeric(lapply(onto[, "Père"], function(x) {
        return(as.numeric(onto[onto[, "ID"] == x, ][, "nb"]))
      }))
    
    # Couleur des noeuds : 
    
    if (any(grepl("Couleur", colnames(onto))))
    {
      my_colors <-
        paste(
          'd3.scaleOrdinal() .domain([',
          paste(
            '"',
            unique(onto$Groupe)
            ,
            '"',
            collapse = ',',
            sep = ""
          ),
          ']) .range([',
          paste('"', onto[onto$Name %in% onto$Groupe,][,"Couleur"], '"', collapse = ","),
          ']) .unknown("grey");'
        )
    }
    else{
      my_colors <- "d3.scaleOrdinal(d3.schemeCategory20);"
    }
    
    val$onto <- onto
    
    links <- onto[, c("nb", "nbPere")]
    
    # Si cetrains noeuds ont plusieurs pères :
    
    if (any(duplicated(onto$ID))) {
      links[duplicated(links$nb), c("nb", "nbPere")] <-
        links[duplicated(links$nb), c("nbPere", "nb")]
    }
    
    onto <- onto[!duplicated(onto$ID), ]
    
    # Affichage de l'ontologie
    
    fn <- forceNetwork(
      links,
      onto,
      Source = "nbPere",
      Target = "nb",
      NodeID = "Name",
      Group = "Groupe",
      zoom = TRUE,
      legend = TRUE,
      opacity = 0.95,
      fontFamily = "sans-serif",
      # linkDistance = 30,
      fontSize = 15,
      Nodesize = "Taille",
      colourScale = my_colors
    )
    
    fn$x$nodes$border <- "#000000"
    
    
    # Javascript couleur des contours des noeuds
    
    fn <- onRender(fn,
                   'function(el, x) { d3.selectAll("circle").style("stroke", d => d.border); }')
    fn
    
  })
  
  
  # - Onglet Mosaïques - #
  
  
  # Filtrer les dimensions mosaique avec 10+ valeurs différentes (onglet Individus)
  
  output$choix_dim_mosaique <- renderUI({
    inFile <- val$file_filtre
    liste_dimensions_filtree <- c("-")
    
    liste_dimensions <-
      colnames(inFile)
    
    liste_dimensions_filtree <-
      lapply(liste_dimensions, function(x) {
        valeurs <- as.character(unique(inFile[x])[, 1])
        if (length(valeurs) > 15) {
          return(NULL)
        }
        return(x)
      })
    
    
    if (!is.null(inFile)) {
      liste_dimensions_filtree <-
        as.character(liste_dimensions_filtree[liste_dimensions_filtree != "NULL"])
    }
    tagList(
      
      # Paramètres mosaïque
      
      box(
        title = "Choix des dimensions",
        width = 3,
        solidHeader = TRUE,
        status = "primary",
        checkboxInput("shade", "Afficher les résidus", value = TRUE),
        checkboxInput("inverse", "Inverser les dimensions", value = FALSE),
        checkboxInput("nonull", "Retirer les valeurs nulles", value = TRUE),
        selectInput(
          "dimX_mosaic",
          label = "Dimension X",
          choices = liste_dimensions_filtree,
          selected = liste_dimensions_filtree[1]
        ),
        selectInput(
          "dimY_mosaic",
          label = "Dimension Y",
          choices = liste_dimensions_filtree,
          selected = liste_dimensions_filtree[2]
        ),
       #sliderInput(
      #    inputId = "cex_mosaic_ind",
       #   label = "Taille des labels",
        #  min = 0.1,
         # max = 2,
        #  value = 1.1
       # )
      ),
      
      # Affichage mosaïque
      
      box(
        title = "Mosaïque",
        width = 9,
        solidHeader = TRUE,
        status = "primary",
        plotOutput("mosaic", height = "600px")
      )
    )
    
  })
  
  # Mosaïques par cluster
  
  output$mosaiques_clusters <- renderUI({
    req(val$clust_mk)
    file <- val$clust_mk
    liste_dimensions_filtree <- c("-")
    if (!is.null(file)) {
      liste_dimensions <-
        colnames(file)
      
      liste_dimensions_filtree <-
        lapply(liste_dimensions, function(x) {
          valeurs <- as.character(unique(file[x])[, 1])
          if (length(valeurs) > 15) {
            return(NULL)
          }
          return(x)
        })
      liste_dimensions_filtree <-
        as.character(liste_dimensions_filtree[liste_dimensions_filtree != "NULL" &
                                                liste_dimensions_filtree != "id_clust"])
    }
    
    # Paramètrage mosaïques
    
    tagList(
      box(
        title = "Choix de la dimension",
        width = 3,
        solidHeader = TRUE,
        status = "primary",
        checkboxInput("shade_clusters", "Afficher les résidus", value = TRUE),
        checkboxInput("inverse_clusters", "Inverser les dimensions", value = FALSE),
        checkboxInput("nonull_clusters", "Retirer les valeurs nulles", value = TRUE),
        selectInput(
          "dim_mosaic",
          label = "Dimension",
          choices = liste_dimensions_filtree,
          selected = liste_dimensions_filtree[1]
        ),
        #sliderInput(
        #  inputId = "cex_mosaic_clusters",
        #  label = "Taille des labels",
        #  min = 0.1,
      #    max = 2,
      #   value = 1.1
       # )
      ),
      
      # Affichage mosaïque
      
      box(
        title = "Mosaïque",
        width = 9,
        height = 12,
        solidHeader = TRUE,
        status = "primary"
        ,
        plotOutput("mosaic_clusters", height = "600px")
      )
    )
    
  })
  
  # Stats
  
  output$stats <- renderTable({
    req(input$file_sequences)
    
    tryCatch({
      df <- read_delim(
        input$file_sequences$datapath,
        col_names = TRUE,
        delim = input$sep_seq,
        escape_double = FALSE,
        trim_ws = TRUE
      )
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    statsTable <-
      data.frame(
        Individus = length(unique(na.exclude(df$pers_id))),
        NbEnfants = length(unique(na.exclude(df[df$pers_parente == "Un enfant", ]$pers_id))),
        NbParents = length(unique(na.exclude(df[df$pers_parente == "Un parent" |
                                                  df$pers_parente == "Un beau-parent", ]$pers_id)))
      )
    
    return(statsTable)
    
  })
  
  ###
  # Plots
  ###
  
  # Frequency activity distribution
  
  output$barplot <- renderPlotly({
    
    inFile <- val$file_filtre
    rad_select <- input$radio
    
    switch(
      rad_select,
      "1" = {
        vect <- subset(inFile, stop != "NULL")$stop
        titre_barplot <- "Distribution des activités stop"
      },
      "2" = {
        vect <- subset(inFile, move != "NULL")$move
        titre_barplot <- "Distribution des activités move"
      },
      "3" = {
        vect <-
          subset(inFile, lieu_type != "NULL")$lieu_type
        titre_barplot <- "Distribution des lieux"
      },
      "4" = {
        vect <-
          subset(inFile, accompagnement != "NULL")$accompagnement
        titre_barplot <- "Distribution des accompagnements"
      }
    )
    freq_mk(vect)
  })
  
  # Zipf law
  
  output$zipf <- renderPlot({
    
    inFile <- val$file_filtre
    
    rad_select <- input$radio
    
    zipf_mk <- function(vect) {
      x <- unlist(str_split(vect, " "))
      df <- as.data.frame(table(x[x != "NULL"]))
      colnames(df) <- c("activity", "freq")
      df <- df[order(-df$freq), ]
      
      rank <- seq(1, length(df$freq))
      plot(
        x = rank,
        y = df$freq,
        ylab = "Fréquence",
        col = unlist(lapply(df[, 1], col_mk)),
        pch = 19,
        log = "y"
      )
      
      line <- lm(log10(df$freq) ~ rank)
      abline(line, col = "#777777")
    }
    switch(
      rad_select,
      "1" = {
        vect <- subset(inFile, stop != "NULL")$stop
        titre_barplot <- "Distribution des activités stop"
      },
      "2" = {
        vect <- subset(inFile, move != "NULL")$move
        titre_barplot <- "Distribution des activités move"
      },
      "3" = {
        vect <-
          subset(inFile, lieu_type != "NULL")$lieu_type
        titre_barplot <- "Distribution des lieux"
      },
      "4" = {
        vect <-
          subset(inFile, accompagnement != "NULL")$accompagnement
        titre_barplot <- "Distribution des accompagnements"
      }
    )
    zipf_mk(vect)
  })
  
  # Distribution des longueurs des séquences
  
  output$poisson <- renderPlot({
    # print(nrow(val$file_filtre))
    
    inFile <- val$file_filtre
    size_seq <- as.numeric(table(inFile[, "id"]))
    
    hist(
      size_seq,
      xlim = c(input$min, input$max),
      breaks = (input$max - input$min) / input$pas,
      col = "skyblue"
    )
  })
  
  # Flow Chord diagram
  
  output$flow <- renderPlot({
    build_flows <- function(sequences) {
      H <- hash()
      for (k in 1:length(sequences)) {
        s <- sequences[[k]]
        if (length(s) > 1) {
          for (i in 1:(length(s) - 1)) {
            s_i <- as.character(s[i])
            s_ip <- as.character(s[i + 1])
            
            if (!(s_i %in% keys(H))) {
              H[[s_i]] <- hash()
            }
            if (!(s_ip %in% keys(H[[s_i]]))) {
              H[[s_i]] [[s_ip]] <- 1
            }
            else {
              H[[s_i]] [[s_ip]] <- H[[s_i]] [[s_ip]] + 1
            }
          }
        }
      }
      
      v_i = c()
      v_j = c()
      tij = c()
      
      for (k in sort(keys(H))) {
        for (k2 in sort(keys(H[[k]]))) {
          v_i <- c(v_i, k)
          v_j <- c(v_j, k2)
          tij <- c(tij, H[[k]][[k2]])
        }
      }
      
      flow <- data.frame("i" = v_i,
                         "j" = v_j,
                         "tij" = tij)
      
      return(flow)
    }
    sequences <- list()
    
    file <- val$file_filtre
    
    for (i in 1:max(file[, "id"])) {
      sequences[[i]] <-
        as.vector(as.character(subset(file, id == i)$lieu_type))
    }
    sequences_flow <-
      to_list(for (seq in sequences)
        seq[seq != "NULL"])
    
    flows_non_agg <-
      build_flows(to_list(for (s in sequences_flow)
        s <- s))
    flows_agg <-
      build_flows(to_list(for (s in sequences_flow)
        
        lapply(s, agg_id)))
    if (!input$checkbox_agg_flow) {
      flow <- flows_non_agg
      # print(flow)
      
    }
    else {
      flow <- flows_agg
      # print(flow)
    }
    
    flow_filtre <- flow
    
    # Flux filtrés
    
    val$flow <-
      flow_filtre[flow_filtre$tij >= (input$filtre_flux / 100) * sum(flow$tij), ]
    
    liste_lieux <-
      unique(union(as.character(unique(val$flow[, 1])), as.character(unique(val$flow[, 2]))))
    
    df_flow <- val$flow
    
    df_flow[, 1] <- as.character(lapply(df_flow[, 1],name_id))
    df_flow[, 2] <- as.character(lapply(df_flow[, 2],name_id))
    
    defaultW <- getOption("warn")
    options(warn = -1)
    
    # print("---Liste Lieux---")
    # print(as.character(liste_lieux))
    # print(val$flow)
    # print(as.character(lapply(val$flow[, 1], col_mk)))
    
    circos.clear()
    chordDiagram(
      df_flow,
      transparency = input$transparence_flux,
      row.col = as.character(lapply(liste_lieux, col_mk)),
      grid.col = as.character(lapply(liste_lieux, col_mk)),
      annotationTrack = c("name", "grid", "axis"),
      directional = 1,
      direction.type = c("arrows", "diffHeight"),
      diffHeight  = -0.04,
      annotationTrackHeight = c(0.1, 0.1, 0.1),
      link.arr.type = "big.arrow",
      link.sort = TRUE,
      link.largest.ontop = TRUE
    )
    
    options(warn = defaultW)
    
  })
  
  
  
  
  # Daily_pattern
  
  output$daily_pattern <- renderPlot({
    source_python('~/R-workspace/daily_pattern.py')
    daily_pattern_R("~/R-workspace/sequences_EMD_paper.csv")
    daily_pattern <-
      daily_pattern_R("~/R-workspace/sequences_EMD_paper.csv")
    daily_pattern_names <-
      c(
        "2",
        "3-1",
        "3-2",
        "3-3",
        "3-*",
        "4-1",
        "4-2",
        "4-3",
        "4-*",
        "5-1",
        "5-2",
        "5-3",
        "5-*",
        "> 5"
      )
    barplot(
      daily_pattern,
      xlab = "Network id",
      ylab = "Percentage of network id",
      names.arg =  daily_pattern_names,
      col = "#1F2899"
    )
  })
  
  
  # Network daily pattern
  
  output$network <- renderPlot({
    tryCatch({
      par(mfrow = c(2, 5))
      
      id <- c('0', '1', '2', '3', '5', '6', '7', '9', '10', '11')
      
      daily_pattern_names <-
        c("2",
          "3-1",
          "3-2",
          "3-3",
          "4-1",
          "4-2",
          "4-3",
          "5-1",
          "5-2",
          "5-3")
      
      for (i in 1:10) {
        path <-
          paste("~/R-workspace/daily_pattern_graphs/daily_pattern",
                id[i],
                ".csv",
                sep = "")
        net_csv <- read_delim(path, " ", col_names = FALSE)
        network <-
          graph_from_adjacency_matrix(as.matrix(net_csv))
        col <- rep("gray", nrow(net_csv))
        red <- which.max(rowSums(net_csv))
        col[red] <-
          ifelse(max(rowSums(net_csv)) > 1, "red", "gray")
        plot(
          network,
          vertex.size = 32,
          edge.curved = 0.3,
          layout = layout.circle,
          vertex.label = NA,
          vertex.color = col,
          edge.color = "black",
          edge.arrow.size = .4,
          main = paste("Daily pattern id: ", daily_pattern_names[i], sep = "")
        )
        
      }
      
    }, error = function(e) {
      stop(safeError(e))
    })
  })
  
  
  
  #Mosaiques descriptives
  
  output$mosaic <- renderPlot({
    file <- val$file_filtre
    cond <- ""
    tryCatch({
      if (input$nonull) {
        cond <- "NULL"
      }
      
      
      if (!is.null(input$inverse)) {
        plot.new()
        if (input$inverse) {
          mosaicplot(
            main = paste(
              "Mosaïque de",
              input$dimY_mosaic,
              "et",
              input$dimX_mosaic,
              sep = " "
            ),
            table(file[, input$dimY_mosaic],
                  file[, input$dimX_mosaic],
                  exclude = cond),
            #cex.axis = input$cex_mosaic_ind,
            las = 1,
            shade = input$shade
          )
        } else {
          mosaicplot(
            main = paste(
              "Mosaïque de",
              input$dimX_mosaic,
              "et",
              input$dimY_mosaic,
              sep = " "
            ),
            table(file[, input$dimX_mosaic],
                  file[, input$dimY_mosaic],
                  exclude = cond),
            #cex.axis = input$cex_mosaic_ind,
            las = 1,
            shade = input$shade
          )
        }
      }
    },
    error = function(e) {
      warning(
        safeError(error = "Pas de données correspondantes.\nVeuillez choisir d'autres dimensions ou données.")
      )
    })
  })
  
  #output$view_ind <- renderTable({
   # file <- val$file_filtre
    
  #  table_ind <- as.data.frame(as.matrix(ftable(file[, input$dimX_mosaic],
                                          #     file[, input$dimY_mosaic])))
    #table_ind$sum_row <- rowSums(table_ind)
    #table_ind$sum_row <- rowSums(table_ind)
    #table_ind <- rbind(table_ind, colSums(table_ind))
    #row.names(table_ind)[nrow(table_ind)] <- "sum_col" 
   # table_ind
  #})
  
  # Mosaiques par cluster
  
  # TODO : dimensions activités agrégées
  
  output$mosaic_clusters <- renderPlot({
    file <- val$clust_mk
    cond <- ""
    
    # if(!is.null(input$dim_mosaic)){
    tryCatch({
      # inFile <- input$file_sequences
      
      if (input$nonull_clusters) {
        cond <- "NULL"
      }
      # print(input$dim_mosaic)
      # print(file)
      # print(file[,input$dim_mosaic])
      if (!is.null(input$inverse_clusters)) {
        if (!input$inverse_clusters) {
          mosaicplot(
            main = paste("Mosaïque de",
                         input$dim_mosaic,
                         "par cluster",
                         sep = " "),
            table(file[, "id_clust"],
                  file[, input$dim_mosaic]),
            las = 1,
            shade = input$shade_clusters
          )
        } else {
          mosaicplot(
            main = paste("Mosaïque de",
                         input$dim_mosaic,
                         "par cluster",
                         sep = " "),
            table(file[, input$dim_mosaic],
                  file[, "id_clust"]),
                  #exclude = cond),
            #cex.axis = input$cex_mosaic_clusters,
            las = 1,
            shade = input$shade_clusters
          )
        }
      }
    }, error = function(e) {
      stop(safeError(e))
    })
    # }
  })
  
  #Clustering
  
  output$clusters <- renderPlot({
    inFile <- input$file_distances
    
    tryCatch({
      df <- val$file_distances
      val$ced <- df
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    
    
    
    # print("----------")
    # print(val$ced)
    df <-
      val$ced[rownames(val$ced) %in% which(colnames(val$ced) %in% unique(val$file_filtre[, "id_seq"])), colnames(val$ced) %in% unique(val$file_filtre[, "id_seq"])]
    
    # print(length(colnames(df)))
    # print(length(rownames(df)))
    
    val$ced_filtre <- df
    
    cedhc <- NULL
    
    tryCatch({
      cedhc <-
        hclust(as.dist(val$ced_filtre), method = input$cluster_method)
      
      
    },
    error = function(e) {
      message <-   "Mauvais format de fichier.
            \nVeuillez vérifier d'avoir bien rentré les bon fichiers avec les bons séparateurs dans l'onglet de chargement."
      shinyalert("Oops!", message , type = "error")
      showModal(modalDialog(div(message), easyClose = TRUE))
      showNotification(paste0(message), type = 'err')
      stop(safeError(message))
      
    })
    
    
    
    val$hc <- NULL
    val$hc <- cedhc
    
    tryCatch({
      groups <- cutree(cedhc, k = input$cluster_nb)
      
      
    },
    error = function(e) {
      stop(
        safeError(
          "Trop de clusters pour peu de séquences. Veuillez réduire le nombre de clusters ou diminuer le filtre dans \"Filtrer les données\""
        )
      )
    })
    
    # Fonction de Sorting du clustering hiérarchique
    # Réordonne les classes comme disposées sur le dendrogramme de gauche à droite
    
    sort_hc <- function(groups, hc) {
      
      #Indexation sur colnames(ced)
      
      orderClust <- colnames(val$ced_filtre)[hc$order]
      id_seq <- names(groups)
      num_clust <- groups[1:length(groups)]
      id_clust <- rep(0, length(groups))
      df_groups <- data.frame(id_seq, num_clust, id_clust)
      i <- 1
      clust_prop <- data.frame(table(groups))
      
      for (k in seq(max(num_clust))) {
        c <- df_groups[df_groups$id_seq == orderClust[i],]$num_clust
        df_groups[df_groups$num_clust == c,]$id_clust <- k
        i <- i + clust_prop[clust_prop$groups == c,]$Freq
      }
      
      
      df_groups <- df_groups[, -2]
      groups[1:length(groups)] <- df_groups$id_clust
      return(groups)
    }
    
    val$groups <- sort_hc(groups, val$hc)
    
    groups <- as.data.frame(sort_hc(groups, val$hc))
    
    groups <- tibble::rownames_to_column(groups, "id_seq")
    colnames(groups) <- c("id_seq", "id_clust")
    
    val$groups_merged <- groups
    val$clust_mk <-
      merge(
        x = val$groups_merged,
        y = val$file_filtre,
        by = "id_seq",
        all = FALSE
      )
    
    A2Rplot(val$hc, k = input$cluster_nb, boxes = TRUE)
    
  })
  
  
  # Quality summary
  
  output$quality <- renderTable({
    req(val$clust_mk)
    
    diameter <- function(id_seq_clust, dist_mat) {
      sub_mat <- dist_mat[id_seq_clust, id_seq_clust]
      return(max(sub_mat))
    }
    
    medoid <-  function(id_seq_clust, dist_mat) {
      return(id_seq_clust[which.min(rowSums(dist_mat))])
    }
    
    radius <- function(id_seq_clust, dist_mat) {
      sub_mat <- dist_mat[id_seq_clust, id_seq_clust]
      return(max(sub_mat[, which.min(rowSums(sub_mat))]))
    }
    
    radius_95 <- function(id_seq_clust, dist_mat) {
      sub_mat <- dist_mat[id_seq_clust, id_seq_clust]
      return(quantile(sub_mat[[which.min(rowSums(sub_mat))]], 0.95))
    }
    
    tryCatch({
      sil <- silhouette(val$groups, val$ced_filtre)
      
      summary_data <- data.frame(id = seq(input$cluster_nb))
      summary_data$card <- summary(sil)$clus.sizes
      summary_data$prop <-
        summary(sil)$clus.sizes / sum(summary(sil)$clus.sizes)
      summary_data$sil <- summary(sil)$clus.avg.widths
      summary_data$diam <-
        to_vec(for (i in seq(input$cluster_nb))
          diameter(which(val$groups == i), val$ced_filtre))
      summary_data$diam_95 <-
        to_vec(for (i in seq(input$cluster_nb))
          quantile(as.dist(val$ced_filtre[which(val$groups == i), which(val$groups == i)]), 0.95))
      summary_data$rad <-
        to_vec(for (i in seq(input$cluster_nb))
          radius(which(val$groups == i), val$ced_filtre))
      summary_data$rad_95 <-
        to_vec(for (i in seq(input$cluster_nb))
          radius_95(which(val$groups == i), val$ced_filtre))
      
      
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    return(summary_data)
  })
  
  output$boxplot_length <- renderPlot({
    # print(nrow(val$file_filtre))
    
    val$clust_mk
    
    
  })
  
  # Inertia
  
  output$inertia <- renderPlot({
    max_clust <- 10
    req(val$clust_mk)
    
    tryCatch({
      sil_values <- c()
      for (i in 2:max_clust) {
        groups <- cutree(val$hc, k = i)
        sil <- silhouette(groups, val$ced_filtre)
        sil_values <-
          c(sil_values, mean(summary(sil)$clus.avg.widths))
      }
      
      inertie <- sort(val$hc$height, decreasing = TRUE)
      col = rep("black",  max_clust)
      col[input$cluster_nb - 1] = "red"
      
      par(mfrow = c(1, 2))
      
      plot(
        seq(2, max_clust),
        inertie[1:max_clust - 1] - inertie[2:max_clust],
        pch = 19,
        col = col,
        xlab = "Number of clusters",
        ylab = "Inertia gap",
        main = "Inertia gap"
      )
      lines(seq(2, max_clust), inertie[1:max_clust - 1] - inertie[2:max_clust], lty = 1)
      
      plot(
        seq(2, max_clust),
        sil_values,
        pch = 19,
        col = col,
        xlab = "Number of clusters",
        ylab = "Avg Silhouette",
        main = "Avg Silhouette"
      )
      
      lines(seq(2, max_clust), sil_values, lty = 1)
      
    }, error = function(e) {
      stop(safeError(e))
    })
  })
  
  
  #Stack
  
  output$stack <- renderPlot({
    req(val$clust_mk)
    
    stack_df <- function(j, clust_mk) {
      # colonne j
      
      id_clust <- c()
      act_col <- c()
      
      for (i in seq(nrow(clust_mk))) {
        
        pourcentage <- as.integer(i / nrow(clust_mk))
        
        
        # Pause for 0.1 seconds to simulate a long computation.
        act_split <- unlist(strsplit(as.character(clust_mk[i, j]), " "))
        id_clust <-
          c(id_clust, rep(clust_mk[i,]$id_clust, length(act_split)))
        act_col <- c(act_col, act_split)
        
        # Update le texte.
        setProgress(value = pourcentage,session = session,
                    message = "Veuillez patienter...",
                    detail = paste(pourcentage*100, "%")
        )
        
      }
      
      df <- as.data.frame(id_clust)
      df$act <- act_col
      return(df)
    }
    
    
    tryCatch({
      
      # Barre de progression
      
      withProgress(
        message = "Veuillez patienter...",
        # min = 0,
        # max = 100,
        value = 0,
        {
          if (!input$stack_nonull) {
            stack_act <- stack_df(input$radio_stack, val$clust_mk)
            stack_act$act <-
              as.character(lapply(stack_act$act, agg_id))
            stack_act$name <- unlist(lapply(stack_act$act, name_id))
          }
          else{
            stack_act <- stack_df(input$radio_stack, val$clust_mk)
            stack_act <- subset(stack_act, act != "NULL")
            stack_act$act <-
              as.character(lapply(stack_act$act, agg_id))
            stack_act$name <- unlist(lapply(stack_act$act, name_id))
          }
        }
      )
      
      
      freq_act <- as.data.frame(table(stack_act$act))
      freq_act$Name <- as.character(lapply(freq_act[, 1], name_id))
      freq_act$Color <- as.character(lapply(freq_act[, 1], col_mk))
      freq_act$Group <-
        as.character(lapply(as.character(lapply(freq_act[, 1], agg_id)), name_id))
      
      ggplot(stack_act) +
        geom_bar(
          width = 0.7,
          mapping = aes(x = as.factor(id_clust), fill = name),
          position = "fill"
        ) +
        scale_fill_manual(values = unlist(lapply(freq_act[order(freq_act$Name, decreasing = FALSE), ][, 1], col_mk))) +
        theme(panel.background = element_rect("white", "black", size = 0.5))
      # legend("topright", 
      #        legend = unique(freq_act$Group), 
      #        fill = unique(unlist(lapply(freq_act[,1], col_mk))),
      #        bty = "n", 
      #        pt.cex = 2, 
      #        cex = 1, 
      #        text.col = "black", 
      #        horiz = F , 
      #        inset = c(0.1, 0.1))
      
    }, error = function(e) {
      stop(safeError(e))
    })
  })
  
  
  
  
  
  # Sankey
  
  output$sankey <- renderSankeyNetwork({
    #====DONNEES=====
    
    
    clusterMK <-
      read.csv("~\\Data\\cluster_mk2.csv",
               sep = ";",
               fileEncoding = "UTF-8-BOM")
    clusterMK$jour_semaine <- weekdays(as.Date(clusterMK$d))
    clusterMK$activite <- as.character(clusterMK$activite)
    clusterMK$activite <- gsub(" ", "|", clusterMK$activite)
    clusterMK$activite <- sapply(clusterMK$activite, function(x) {
      if (grepl("11\\|.*12\\|.*13\\|?.*", x)) {
        x <- gsub("11\\|", "A\\|", x)
        x <- gsub("12\\|", "", x)
        x <- gsub("13\\|", "", x)
        x <- gsub("\\|13$", "", x)
        x <- gsub("\\|$", "", x)
      }
      else{
        x <- x
      }
    })
    clusterMK <- merge(
      clusterMK,
      cbind(
        as.character(infos_enfants$id_seq),
        infos_enfants$niveau_actuel
      ),
      by.x = "id_seq",
      by.y = "V1",
      all.x = T
    )
    names(clusterMK)[21] <- "niveau"
    clusterMK$niveau <- as.character(clusterMK$niveau)
    clusterMK$niveau <- replace_na(clusterMK$niveau, "Adulte")
    clusterMK$niveau <-
      gsub("CM1|CM2", "Primaire", clusterMK$niveau)
    clusterMK$niveau <- gsub("6e", "Collège", clusterMK$niveau)
    
    infos_enfants <- read.xlsx(xlsxFile =
                                 "~:\\Data\\Infos_cohortes_MKBDD_20210218.xlsx",
                               sheet = "Bilan",
                               sep = ";")
    infos_enfants <-
      infos_enfants[infos_enfants$parente == "Un enfant", ]
    infos_enfants <-
      cbind(infos_enfants[, 4], infos_enfants[, 7:9])
    names(infos_enfants)[1] <- "pers_id"
    names(infos_enfants)[3] <- "niveau"
    names(infos_enfants)[4] <- "passage_6e"
    
    infos_enfants <- merge(
      x = infos_enfants,
      y = clusterMK %>% select(1:6, 19),
      by.x = "pers_id",
      by.y = "pers_id",
      all.x = TRUE
    )
    
    # infos_enfants$bis <- ifelse(grepl("b",infos_enfants$Temps),"1","0")
    infos_enfants$periode <-
      ifelse(
        grepl("b", infos_enfants$Temps),
        ifelse(
          dmy(infos_enfants$d) %within% interval(dmy("20/11/2018"), dmy("04/02/2019")),
          "T1bis",
          "T2bis"
        ),
        ifelse(
          dmy(infos_enfants$d) %within% interval(dmy("13/03/2018"), dmy("06/06/2018")),
          "T1",
          "T2"
        )
      )
    
    infos_enfants <- as.data.frame(unique(infos_enfants))
    names(infos_enfants)[3] <- "niveau_initial"
    infos_enfants$niveau_actuel <-
      ifelse(
        infos_enfants$niveau_initial == "CM1",
        ifelse(grepl("2", infos_enfants$periode),
               "CM2",
               "CM1"),
        ifelse(
          grepl("2", infos_enfants$periode) & infos_enfants$passage_6e == 1,
          "6e",
          "CM2"
        )
      )
    
    
    enfants_source <-
      infos_enfants[grepl("1", infos_enfants$periode), ]
    names(enfants_source)[12] <- c("niveau_source")
    
    enfants_target <-
      infos_enfants[grepl("2", infos_enfants$periode), ]
    names(enfants_target)[12] <- c("niveau_target")
    
    infos_enfants$niveau_source <- enfants_source$niveau_actuel
    infos_enfants$niveau_target <- enfants_source$niveau_actuel
    enfants_source <- enfants_source %>% select(1, 6, 12)
    enfants_target <- enfants_target %>% select(1, 6, 12)
    
    ordre_clust <- table(infos_enfants$id_clust)
    
    enfants_source$cluster_dominant_source <-
      sapply(enfants_source$pers_id, function(x) {
        temp <-
          table(enfants_source[enfants_source$pers_id == x, ]$id_clust)
        temp <-
          merge(as.data.frame(temp), as.data.frame(ordre_clust), by = "Var1")
        temp <- temp[order(temp$Freq.y, decreasing = FALSE), ]
        temp <- temp[order(temp$Freq.x, decreasing = TRUE), ]
        return(head(temp, 1)$Var1)
      })
    
    enfants_target$cluster_dominant_target <-
      sapply(enfants_target$pers_id, function(x) {
        temp <-
          table(enfants_target[enfants_target$pers_id == x, ]$id_clust)
        temp <-
          merge(as.data.frame(temp), as.data.frame(ordre_clust), by = "Var1")
        temp <- temp[order(temp$Freq.y, decreasing = FALSE), ]
        temp <- temp[order(temp$Freq.x, decreasing = TRUE), ]
        return(head(temp, 1)$Var1)
      })
    
    enfants_source <- enfants_source[, -2]
    enfants_target <- enfants_target[, -2]
    enfants_source <- unique(enfants_source)
    enfants_target <- unique(enfants_target)
    
    
    
    sankey <-
      merge(enfants_source, enfants_target, by = "pers_id")
    sankey$source <-
      paste(sankey$niveau_source,
            sankey$cluster_dominant_source,
            sep = "_")
    sankey$target <-
      paste(sankey$niveau_target,
            sankey$cluster_dominant_target,
            sep = "_")
    
    
    nodes <- data.frame(name = c(
      as.character(sankey$source),
      as.character(sankey$target)
    ) %>% unique())
    nodes$group <-
      substr(nodes$name, nchar(as.character(nodes$name)), nchar(as.character(nodes$name)))
    
    table_sankey <-
      as.data.frame(table(sankey$source, sankey$target))
    names(table_sankey) <- c("source", "target", "value")
    
    table_sankey$IDsource <-
      match(table_sankey$source, nodes$name) - 1
    table_sankey$IDtarget <-
      match(table_sankey$target, nodes$name) - 1
    table_sankey$value <- as.numeric(table_sankey$value)
    
    colors <- brewer.pal(7, name = "Pastel1")
    my_colors <-
      paste(
        'd3.scaleOrdinal() .domain([',
        paste('"', unique(c(
          as.character(table_sankey$source),
          as.character(table_sankey$target)
        )), '"', collapse = ',', sep = ""),
        ']) .range([',
        paste('"', colors, '"', collapse = ","),
        '])'
      )
    
    links <- table_sankey[table_sankey$value != 0, ]
    links$group <- links$source
    
    # Make the Network
    tryCatch({
      sankeyNetwork(
        Links = table_sankey[table_sankey$value != 0, ],
        Nodes = nodes,
        Source = "IDsource",
        Target = "IDtarget",
        Value = "value",
        NodeID = "name",
        NodeGroup = "group",
        sinksRight = FALSE,
        colourScale = my_colors,
        fontSize = 15,
        fontFamily = "Segoe UI Semibold"
      )
      
    }, error = function(e) {
      stop(safeError(e))
    })
    
    
  })
  
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
  
  # Ciel de mots
  
  output$ciel <- renderPlot({
    
    req(val$clust_mk)
    
    nbclusters <- c(1:input$cluster_nb)
    
    df <- val$clust_mk
    df_all <- NULL
    
    # Boucle sur chaque colonne
    for (col in colnames(df)) {
      
      # Ne pas considérer les colonnes ayant plus de 50 valeurs distinctes ou la colonne des clusters
      if (!grepl("move|stop|lieu|accomp", col)) {
        if (length(unique(df[, col])) >= 50 | grepl("clust", col)) {
          # print(paste(col, "NEXT"))
          next
        }
      }
      
      # print("---")
      # print(col)
      id_all <- NULL
      
      split <- NULL
      
      # Vérifier le séparateur entre ID (exemple : pour "221 222", le séparateur est " ")
      for (s in c(" ", "\\|", "\\/"))
      {
        if (any(grepl(s, df[, grepl(col, colnames(df))]))) {
          split <- s
        }
      }
      
      
      # Compte les fréquences d'apparition de chaque id
      for (i in nbclusters) {
        # Si le cluster i existe dans df
        if (length(df[df[, grepl("clust", colnames(df))] == i, col]) != 0) {
          
          #Si la colonne ne fait pas partie de move,lieu,stop,accomp
          # et/ou si elle n'a pas d'espace dans ses tuples
          
          if (!grepl("move|stop|lieu|accomp", col) ||
              is.null(split)) {
            
            # Crée une table de fréquences
            
            temp <-
              as.data.frame(table(data.frame(as.character(df[df[, grepl("clust", colnames(df))] == i, col]))))
          }
          else{
            
            # Crée une table de fréquences après avoir split
            
            temp <- as.data.frame(table(data.frame(unlist(
              strsplit(
                as.character(df[df[, grepl("clust", colnames(df))] == i, col]),
                split = split,
                fixed = TRUE
              )
            ))))
          }
        }
        
        #S'il n'y a pas de cluster i dans df, on passe au prochain cluster
        
        else{
          next
        }
        
        # Colonne cluster
        temp$Cluster <- rep(i, nrow(temp))
        
        temp$Var1 <- as.character(temp$Var1)
        id_all <- rbind(id_all, temp)
        
      }
      
      names(id_all)[1] <- "ID"
      names(id_all)[2] <- "Freq"
      
      
      
      id_all$ID  <-
        as.character(gsub("NULL", paste("NULL", col, sep = "_"), id_all$ID))
      id_all <-
        id_all[order(id_all$ID),]
      
      # Si l'une des colonnes font partie de celles des ontologies, agréger les ID
      if (grepl("move|stop|lieu|accomp", col)) {
        id_all$ID <- unlist(lapply(id_all$ID, agg_id))
      }
      
      # print("-----")
      # print(id_all)
      
      id_all <-
        aggregate(id_all$Freq,
                  by = list(id_all$ID, id_all$Cluster),
                  FUN = sum)
      
      
      names(id_all)[1] <- "ID"
      names(id_all)[2] <- "Cluster"
      names(id_all)[3] <- "Freq"
      
      # Colonne mot
      
      id_all$Name <- id_all$ID
      if (grepl("move|stop|lieu|accomp", col)) {
        id_all$Name <- unlist(lapply(id_all$ID, agg_id))
        id_all$Name <- unlist(lapply(id_all$Name, name_id))
      }
      
      # Colonne dimension
      
      id_all$Column <- col
      
      id_all_dup <-
        as.data.frame(lapply(id_all, rep, id_all$Freq))
      defaultW <- getOption("warn")
      options(warn = -1)
      
      temp <- CrossTable(
        x = id_all_dup$ID,
        y = id_all_dup$Cluster,
        sresid = TRUE,
        chisq = TRUE,
        format = "SPSS"
      )
      
      options(warn = defaultW)
      
      if (nrow(temp$tab) > 1) {
        id_all <-
          merge(
            id_all,
            round(temp$prop.col, 3),
            by.x = c("ID", "Cluster"),
            by.y = c("x", "y"),
            all.x = TRUE
          )
      }
      else{
        id_all[,"Freq.y"] <- 1
      }
      
      # Colonne résidu
      
      temp <- as.data.frame(temp$CST$residuals)
      
      names(temp)[c(which(names(temp) == "x"),
                    which(names(temp) == "y"),
                    which(grepl("Freq", names(temp))))] <-
        c("ID", "Cluster", "Residuals")
      
      id_all <- merge(id_all, temp)
      
      names(id_all)[which(names(id_all) == "Freq.x")] <- "Freq"
      names(id_all)[which(names(id_all) == "Freq.y")] <-
        "Freq_pct"
      names(id_all)[which(grepl("\\$CST",names(id_all)))] <-
        "Residuals"
      id_all$Name <-
        gsub("Autre.*", paste("Autre", col, sep = "_"), id_all$Name)
      id_all$Name <-
        gsub("Autre_accompagnement", "Autre_accomp", id_all$Name)
      id_all$Name <-
        gsub("NULL_accompagnement", "NULL_accomp", id_all$Name)
      
      if (is.null(df_all)) {
        df_all <- id_all
      }
      else{
        names(df_all)[which(names(df_all) == "Freq.x")] <- "Freq"
        names(df_all)[which(names(df_all) == "Freq.y")] <-
          "Freq_pct"
        names(df_all)[which(grepl("\\$CST",names(df_all)))] <-
          "Residuals"
        df_all <- rbind(df_all, id_all)
      }
    }
    df_all<-df_all[order(df_all$Freq,decreasing = TRUE),]
    df_all<-subset(df_all, !grepl("NULL", Name, fixed=TRUE))
    print(df_all)
    
    # Couleur du mot en fonction de sa colonne
    
    df_all$Color <-
      unlist(lapply(as.numeric(as.factor(df_all$Column)), function(x) {
        return(c(
          brewer.pal(n = 8, "Dark2"),
          "#c71a1a",
          "#7b53b0",
          "#0d8ea8",
          "#a7c433"
        )[x])
      }))
    
    
    # COlonne et fonction score (freq * résidu)
    
    score_function <- function(r, x, n) {
      return(r * x)
    }  
    df_all$Score <-
      mapply(
        score_function,
        df_all$Residuals,
        df_all$Freq_pct,
        df_all$Name
      )
    
    # Retirer les frequences à 100%
    df_all <- df_all[df_all$Freq_pct < 1, ]
    
    # TODO : Importer les polices si pas faites 
    
    # options(warn=0) 
    # font_import(prompt = FALSE)
    
    # Fonction d'affichage des nuages
    
    display_clouds <- function(n) {
      options(scipen = 999)
      par(mfrow=c(1,1))
      plot.new()
      par(mfrow = c(max(ceiling(sqrt(
        max(nbclusters)
      ))), max(ceiling(sqrt(
        max(nbclusters)
      )))),
      #Layout du ciel
      mar = c(0, 0, 0, 0))
      
      # Boucle en fonction du nombre de clusters
      
      for (i in nbclusters) {
        
        if(nrow(df_all[df_all$Cluster == i, ])==0){
          next
        }
        temp <-
          top_n(df_all[df_all$Cluster == i, ], n, df_all[df_all$Cluster == i, ]$Score)[1:n, ]
        
        par(mar = c(0, 0, 0, 0))
        
        # Nuage de mots
        
        wc <- wordcloud(
          temp$Name,
          temp$Freq_pct,
          rot.per = 0,
          max.words = n,
          # family = "Segoe UI",
          # scale = c((200 * cloud_i$`nbSeq%`) ^ (1 / 3), 1),
          # scale = c((cloud_i$`nbSeq%`*50)^(2/3),1)
          scale = c(2.9, 1),
          random.order = FALSE,
          ordered.colors = TRUE,
          colors = temp[temp$Cluster == i, ]$Color
        )
        
        # Cercle de taille du cluster (non ajusté à SIMBA)
        
        # drawcircle(
        #   x = 3 * (0.8 + (i - 1) %% 3) + (dfCloud$`nbSeq%`[i]),
        #   y = 7.3 - (2.4 * ceiling(i / 3)) + (dfCloud$`nbSeq%`[i]) / 2,
        #   radius = 0.05 + (dfCloud$`nbSeq%`[i]) ^ 1.8
        # )
        title(
          main = paste("Cluster ", i),
          line = -18,
          cex.main = 1.5
        )
        
      }
      mtext(
        "Ciel de mots",
        # (Score 4 = Top 6 Freq. et Rés.)
        family = "Segoe UI Semibold",
        side = 3,
        line = -3,
        outer = TRUE,
        cex = 2.5
        
      )
      plot.new()
      
      # Légende couleur par dimension
      
      legend(
        title = "Types de données",
        "center",
        legend = unique(df_all$Column),
        fill =  unique(df_all$Color),
        cex = 1.5
        
      )
      # plot.new()
      # drawCircle(
      #   x = 7.8,
      #   y = 0.8,
      #   radius = 0.05 + (dfC$`nbSeq%`[i]) ^ 1.5
      # )
      # title(main = "Taille cluster (nb séquences) ",
      #       line = -15,
      #       cex.main = 1)
    }
    
    nbmots <- input$nbmots
    
    # Affichage des nuages en fonction du nombre de mots par nuages (slider)
    
    display_clouds(input$nbmots)
    
    
  })
  
}


