 ## -- Onglet Ontologies -- ##


# - Fonctions liées à l'ontologie - # 

  
  # Fonction d'agrégation des ID
  
  agg_id <- function(id, val) {
      
    if(grepl("A|B|C", id)) {
        return(substr(as.character(id[1]),  1, 2))
    } else {
        return(substr(as.character(id[1]),  1, 1))
    }
  }
  
  # Attribution des noms en fonctions des ID
  # L'ontologie doit comporter une colonne "ID" et "Name"
  
  name_id <- function(id, val) {
    for (ont in val$ontologies) {
      if (!is.null(ont)) {
        if (any(ont[, "ID"] == id[1])) {
          id <- ont[ont[, "ID"] == id[1], ][, "Name"]
        }
      }
    }
    return(as.character(id[1]))
  }
  
  # Attribution des couleurs en fonctions des ID
  # L'ontologie doit comporter une colonne "ID" et "Couleur"
  
  col_id <- function(id, val) {
    for (ont in val$ontologies) {
      if (!is.null(ont)) {
        if (any(ont[, "ID"] == id[1])) {
          id <- ont[ont[, "ID"] == id[1], ][, "Couleur"]
        }
      }
    }
    return(as.character(id[1]))
  }



#####
# Affichage de l'ontologie
#####
  
  ontologyRender <- function(myInputs, myData, input, session, output, val) {
                        return (renderForceNetwork({
    
                            ##
                            # Lecture des fichiers des ontologies
                            ##
                            
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
        
                            },
                            error = function(e) {
                              message <- "Ontologie(s) manquante(s)."
                              shinyalert("Oops!", message , type = "error")
                              showModal(modalDialog(div(message), easyClose = TRUE))
                              stop(safeError(message))
                              
                            })
    
                            defaultW <- getOption("warn")
                            options(warn = -1)
    
                            ##
                            # Attribution d'une colonne "Groupe" 
                            ##
                            onto[, "ID"] <- as.character(onto[, "ID"])
                                onto[, "Groupe"] <-
                                    as.character(lapply(onto[, "ID"], 
                                                 function(x) {
                                                      group <- onto[onto[, "ID"] == x,][, "Name"][1]
                                                    return(group)
                                                 }))
                            ##
                            # Attribution d'une colonne "Taille" pour l'affichage des noeuds de l'ontologie.
                            # La taille du noeud correspond à sa position dans la hiérarchie
                            ##
                            
                            onto[, "Taille"] <-
                                as.numeric(lapply(onto[, "ID"], 
                                           function(x) {
                                               if (grepl("NULL", onto[onto["ID"] == x, ][, "Name"][1])) {
                                                  return(50)
                                               } else{
                                                    if (x %in% onto[, "Père"] | grepl("NULL", onto[onto[, "ID"] %in% onto[onto[, "ID"] == x, ][, "Père"], ][, "Name"])) {
                                                        return(20)
                                                  } else{
                                                        return(1)
                                                  }
                                                }
                                          }))
    
                            options(warn = defaultW)
                            
                            ##
                            # Attribution d'un ID de noeud
                            ##
    
                            onto[, "nb"] <- as.numeric(lapply(onto[, "ID"], function(x) {
                                return(which(unique(onto[, "ID"]) == x) - 1)
                            }))
    
                            onto <- onto[order(onto[, "nb"], decreasing = FALSE), ]
    
                            ##
                            # Attribution d'un ID du/des noeud(s) père(s)
                            ##
                            
        
                            onto[, "nbPere"] <- as.numeric(lapply(onto[, "Père"], function(x) {
                                return(as.numeric(onto[onto[, "ID"] == x, ][, "nb"]))
                            }))
    
                            ##
                            # Couleur des noeuds : 
                            ##
                            
                            if (any(grepl("Couleur", colnames(onto)))) {
                              my_colors <- paste(
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
                            } else{
                                my_colors <- "d3.scaleOrdinal(d3.schemeCategory20);"
                            }
    
                            val$onto <- onto
    
                            links <- onto[, c("nb", "nbPere")]
                            
                            
                            # -- Si certains noeuds ont plusieurs pères :
                            
                            if (any(duplicated(onto$ID))) {
                              links[duplicated(links$nb), c("nb", "nbPere")] <-
                                links[duplicated(links$nb), c("nbPere", "nb")]
                            }
    
                            onto <- onto[!duplicated(onto$ID), ]
                            
                            ##
                            # Affichage de l'ontologie
                            ##
                            
                            fn <- forceNetwork(
                                links,
                                onto,
                                Source = "nbPere",
                                Target = "nb",
                                NodeID = "Name",
                                Group = "Groupe",
                                zoom = TRUE,
                                legend = FALSE,
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
                                    'function(el, x) { d3.selectAll("circle").style("stroke", d => d.border); }'
                                    )
                                    
                            fn   
                     }))
                }
  
