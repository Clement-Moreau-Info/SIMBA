
# -- Bontons concernant l'onglet de Filtrage -- #

myInputs <- reactiveValues(rendered = c(1))


#Observer

filterButton <- function(myInputs, myData, input, session, output, val) {
                    return (observeEvent(lapply(paste0("drop", myInputs$rendered), 
                                                function(x)
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
                            }))
                 }
                
                
# - Bouton de défilement dans l'onglet filtrage - #
 
filterUI <- function(myInputs, myData, input, session, output, val) {
                return (renderUI({
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
            }))
        }
# -  Event bouton d'ajout filtre - #

filterButtonAdd <- function(myInputs, myData, input, session, output, val) {
                        return (observeEvent(input$add, {
                                    myInputs$rendered <- c(myInputs$rendered, max(myInputs$rendered) + 1)
                        }))
                   }
    
# - Event bouton suppression filtre - #

filterButtonDel <- function(myInputs, myData, input, session, output, val) {
                        return (observeEvent(input$delete, {
                                if(length(myInputs$rendered) > 1) {
                                    removeUI(selector = paste("div:has(> #drop", 
                                                             max(myInputs$rendered), ")", 
                                                             sep = ""))
                                    removeUI(selector = paste("div:has(> #select", 
                                                             max(myInputs$rendered), ")", 
                                                             sep = ""))
                                    myInputs$rendered <- myInputs$rendered[-length(myInputs$rendered)]
                                 }
                        }))
                    }
                
# - Event bouton refresh - #

filterButtonRefresh <- function(myInputs, myData, input, session, output, val) {
                        return (observeEvent(input$calc, {
                                    showData <- NULL
                                    for (i in 1:length(myInputs$rendered)) {
                                        if (!is.null(input[[paste0("select", i)]])) {
                                            if (is.null(showData)) {
                                                showData <- filter(myData(), 
                                                                   myData()[, input[[paste0("drop", i)]]] %in% input[[paste0("select", i)]])
                                            }
                                            else {
                                                showData <- filter(showData, 
                                                                   showData[, input[[paste0("drop", i)]]] %in% input[[paste0("select", i)]])
                                            }
                                        }
                                    }
                                    if (is.null(showData)) {
                                        showData <- myData()
                                    }
                                    val$file_filtre <- showData
                                    output$table_filtree <- renderDataTable({showData})
    
                                    updateTabsetPanel(session, "clustering", selected = NULL)
    
                                    }))
                
                        }
                        
# - Bouton Clustering - #                        
                        
clusterButton <- function(myInputs, myData, input, session, output, val) {
                    return (observeEvent(input$tabs, {
                    if(input$tabs == "Clustering") {
                        updateTabsetPanel(session, 
                                          "clustering_tabs",
                                          selected = NULL)
                        }
                    }))
                 }
                 
# -  Ajout automatique de nouveaux filtres - #                 

filterButtonAutoAdd <-   function(myInputs, myData, input, session, output, val) {
                            return (observe({
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
                                do.call(shiny::tagList, rows)})
                                if (!is.null(myData())) {
                                    output$table_filtree <- renderDataTable({
                                        myData()
                                    })
                                }
                            }))
                         }


# - Export filter Data - #

exportFilteredDate <- function(myInputs, myData, input, session, output, val) {
                        return (downloadHandler(
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
                        ))
                      }
                        
# - Export des clusters - #

exportDataCluster <- function(myInputs, myData, input, session, output, val) {
                        return(downloadHandler(
                            filename = function() {
                                paste("cluster_", 
                                      input$cluster_nb,
                                      ".csv",
                                      sep = ""
                                     )
                            },
                            content = function(file) {
                            write.csv2(val$clust_mk,
                            file,
                            sep = input$sep_seq)
                            }
                        ))
                     }
                     
# - Observer Matrice de distance - #

distMatrixObs <- function(myInputs, myData, input, session, output, val) {
                    return(observeEvent(input$file_distances, {
                        val$file_distances <- read_delim(
                            input$file_distances$datapath,
                            col_names = FALSE,
                            delim = input$sep_dist,
                            escape_double = FALSE,
                            trim_ws = TRUE
                        )
                    }))
                 }
                 
# - Observer Ontologies 

ontologyObs <- function(myInputs, myData, input, session, output, val) {
                    return(observeEvent(input$files_onto, {
                        tryCatch({
                          onto_stop <- read.csv(
                            input$files_onto[grepl("stop", input$files_onto$name),]$datapath,
                            sep = input$sep_onto,
                            stringsAsFactors = FALSE,
                            encoding = "UTF-8-BOM"
                          )
                          onto_move <- read.csv(
                            input$files_onto[grepl("move", input$files_onto$name),]$datapath,
                            sep = input$sep_onto,
                            stringsAsFactors = FALSE,
                            encoding = "UTF-8-BOM"
                          )
                          onto_lieu <- read.csv(
                            input$files_onto[grepl("lieu", input$files_onto$name),]$datapath,
                            sep = input$sep_onto,
                            stringsAsFactors = FALSE,
                            encoding = "UTF-8-BOM"
                          )
                          onto_accomp <- read.csv(
                            input$files_onto[grepl("accomp", input$files_onto$name),]$datapath,
                            sep = input$sep_onto,
                            stringsAsFactors = FALSE,
                            encoding = "UTF-8-BOM"
                          )
                          val$ontologies <- list(
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
                    }))
               }