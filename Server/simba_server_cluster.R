# ======================= #  
# CLUSTERING HIERARCHIQUE # 
# ======================= #

# Fonction de Sorting du clustering hiérarchique
# Réordonne les classes comme disposées sur le dendrogramme de gauche à droite 
sort_hc <- function(groups, hc, val) {
      
    #Indexation sur colnames(ced)
      
    orderClust <- colnames(val$ced_filtre)[hc$order]
    Id_seq <- names(groups)
    num_clust <- groups[1:length(groups)]
    Id_clust <- rep(0, length(groups))
    df_groups <- data.frame(Id_seq, num_clust, Id_clust)
    i <- 1
    clust_prop <- data.frame(table(groups))
      
    for (k in seq(max(num_clust))) {
        c <- df_groups[df_groups$Id_seq == orderClust[i],]$num_clust
        df_groups[df_groups$num_clust == c,]$Id_clust <- k
        i <- i + clust_prop[clust_prop$groups == c,]$Freq
    }
      
    df_groups <- df_groups[, -2]
    groups[1:length(groups)] <- df_groups$Id_clust
    return(groups)
}


# -- Affichage et génération du clustering hiérarchique -- #

clustHC  <- function(myInputs, myData, input, session, output, val) { 
    return(renderPlot({
    
    req(val$file_distances)
    inFile <- input$file_distances
    
    tryCatch({
      df <- val$file_distances
      val$ced <- df
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    pasteX <- function(x) {    return(paste("X",x,sep=""))    }
    
    delX <- function(x) {    return(substr(x, 2, nchar(x)))    }
    
    # Filtrage des individus dans la matrice de distances
  
    df <- val$ced[rownames(val$ced) %in% which(colnames(val$ced) %in% unique(unlist(lapply(val$file_filtre[, "Id_seq"], pasteX)))),
                  colnames(val$ced) %in% unique(unlist(lapply(val$file_filtre[, "Id_seq"], pasteX)))]
                  
    
    val$ced_filtre <- df
    
    cedhc <- NULL
    
    tryCatch({
      cedhc <- hclust(as.dist(val$ced_filtre), method = input$cluster_method)

    },
    error = function(e) {
      message <-   "Mauvais format de fichier.
            \nVeuillez vérifier d'avoir bien rentré les bon fichiers avec les bons séparateurs dans l'onglet de chargement."
      shinyalert("Oops!", message , type = "error")
      showModal(modalDialog(div(message), easyClose = TRUE))
      showNotification(paste0(message), type = 'err')
      stop(safeError(message)) 
    })
    
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
    
    # Ré-ordonnancement des clusters
    val$groups <- sort_hc(groups, val$hc, val)
    
    # Extraction des groupes
    groups <- as.data.frame(sort_hc(groups, val$hc, val))
    groups <- tibble::rownames_to_column(groups, "Id_seq")
    colnames(groups) <- c("Id_seq", "Id_clust")
    # Suppression des 'X' devant le numero de séquence
    groups$Id_seq <- unlist(lapply(groups$Id_seq, delX))
    val$groups_merged <- groups
    
    # Stockage du dataframe avec id de cluster
    val$clust_mk <-
      merge(
        x = val$groups_merged,
        y = val$file_filtre,
        by = "Id_seq",
        all = FALSE
      )
    
    # Affichage du dendrogramme
    A2Rplot(val$hc, k = input$cluster_nb, boxes = TRUE)
    
  })
  )}

# ======================= #
# QUALITY INDICATOR TABLE
# ======================= #

qualityTable  <- function(myInputs, myData, input, session, output, val) { 
    return(renderTable({
    
    req(val$clust_mk)
    
    diameter <- function(Id_seq_clust, dist_mat) {
      sub_mat <- dist_mat[Id_seq_clust, Id_seq_clust]
      return(max(sub_mat))
    }
    
    medoid <-  function(Id_seq_clust, dist_mat) {
      return(Id_seq_clust[which.min(rowSums(dist_mat))])
    }
    
    radius <- function(Id_seq_clust, dist_mat) {
      sub_mat <- dist_mat[Id_seq_clust, Id_seq_clust]
      return(max(sub_mat[, which.min(rowSums(sub_mat))]))
    }
    
    radius_95 <- function(Id_seq_clust, dist_mat) {
      sub_mat <- dist_mat[Id_seq_clust, Id_seq_clust]
      return(quantile(sub_mat[[which.min(rowSums(sub_mat))]], 0.95))
    }
    
    tryCatch({
        sil <- silhouette(val$groups, val$ced_filtre)
      
        summary_data          <- data.frame(id = seq(input$cluster_nb))
         # Cardinalité des clusters
        summary_data$card     <- summary(sil)$clus.sizes
        # Proportition
        summary_data$prop     <- summary(sil)$clus.sizes / sum(summary(sil)$clus.sizes)
        # Silhouette
        summary_data$sil      <- summary(sil)$clus.avg.widths
        # Diametre
        summary_data$diam     <- to_vec(for (i in seq(input$cluster_nb))
                                    diameter(which(val$groups == i), val$ced_filtre))
        # Diametre à 95%                          
        summary_data$diam_95  <- to_vec(for (i in seq(input$cluster_nb))
                                    quantile(as.dist(val$ced_filtre[which(val$groups == i), which(val$groups == i)]), 0.95))
        # Rayon                              
        summary_data$rad      <- to_vec(for (i in seq(input$cluster_nb))
                                    radius(which(val$groups == i), val$ced_filtre))
        # Rayon à 95%
        summary_data$rad_95   <- to_vec(for (i in seq(input$cluster_nb))
                                    radius_95(which(val$groups == i), val$ced_filtre))
         
    },
    error = function(e) {
        stop(safeError(e))
    })
    
    return(summary_data)
    })
  )}
  
  
# =================== #
# SILHOUETTE, INERTIE #
# =================== #

silhouetteInertie  <- function(myInputs, myData, input, session, output, val) { 
    return(renderPlot({
        max_clust <- 10
        
        req(val$clust_mk)
        
        tryCatch({
            
            # Calcul de silhouette
            sil_values <- c()
            for (i in 2:max_clust) {
                groups <- cutree(val$hc, k = i)
                sil <- silhouette(groups, val$ced_filtre)
                sil_values <- c(sil_values, mean(summary(sil)$clus.avg.widths))
            }
          
            # Calcul inertie
            inertie <- sort(val$hc$height, decreasing = TRUE)
            col = rep("black",  max_clust)
            col[input$cluster_nb - 1] = "red"
          
            par(mfrow = c(1, 2))
          
            # Graphique Inertie
            plot(
                seq(2, max_clust),
                inertie[1:max_clust - 1] - inertie[2:max_clust],
                pch = 19,
                col = col,
                xlab = "Number of clusters",
                ylab = "Inertia gap",
                main = "Inertia gap"
            )
            lines(seq(2, max_clust),
                 inertie[1:max_clust - 1] - inertie[2:max_clust], lty = 1)
          
            # Graphique Silhoiette
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
          
        }, 
        error = function(e) {
                stop(safeError(e))
        })
      })
  )}
  
  
# ============ #
# STATISTIQUES #
# ============ #  


## -- Stackplot -- ##

stackplot <- function(myInputs, myData, input, session, output, val) { 
    return(renderPlot({
        req(val$clust_mk)
        
        stack_df <- function(j, clust_mk) {
          # colonne j
          
          Id_clust <- c()
          act_col <- c()
          
          for (i in seq(nrow(clust_mk))) {
            
            pourcentage <- as.integer(i / nrow(clust_mk))
            
            
            # Pause for 0.1 seconds to simulate a long computation.
            act_split <- unlist(str_split(as.character(clust_mk[i, j]), "\\|"))
           
            
            Id_clust <- c(Id_clust, rep(clust_mk[i,]$Id_clust, length(act_split)))
            act_col <- c(act_col, act_split)
            
            # Update le texte.
            setProgress(value = pourcentage,session = session,
                        message = "Veuillez patienter...",
                        detail = paste(pourcentage*100, "%")
            )
            
          }
          
          df <- as.data.frame(Id_clust)
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
                  as.character(lapply(stack_act$act, agg_id, val = val))
                stack_act$name <- unlist(lapply(stack_act$act, name_id, val = val))
              }
              else{
                stack_act <- stack_df(input$radio_stack, val$clust_mk)
                stack_act <- subset(stack_act, act != "")
                stack_act$act <-
                  as.character(lapply(stack_act$act, agg_id, val = val))
                stack_act$name <- unlist(lapply(stack_act$act, name_id, val = val))
              }
            }
          )
          
          
          freq_act <- as.data.frame(table(stack_act$act))
          freq_act$Name <- as.character(lapply(freq_act[, 1], name_id, val = val))
          freq_act$Color <- as.character(lapply(freq_act[, 1], col_id, val = val))
          freq_act$Group <-
            as.character(lapply(as.character(lapply(freq_act[, 1], agg_id, val = val)), name_id, val = val))
          
          ggplot(stack_act) +
            geom_bar(
              width = 0.7,
              mapping = aes(x = as.factor(Id_clust), fill = name),
              position = "fill"
            ) +
            scale_fill_manual(values = unlist(lapply(freq_act[order(freq_act$Name, decreasing = FALSE), ][, 1], col_id, val = val))) +
            theme(panel.background = element_rect("white", "black", size = 0.5))
          
        }, error = function(e) {
          stop(safeError(e))
        })
        })
    )}
  
 
## -- Mosaic -- ##

mosaicCluster <- function(myInputs, myData, input, session, output, val) { 
    return(renderUI({
    
        req(val$clust_mk)
        
        file <- val$clust_mk
        liste_dimensions_filtree <- c("-")
        if (!is.null(file)) {
          liste_dimensions <- colnames(file)
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
                                                    liste_dimensions_filtree != "Id_clust"])
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
          ),
          
          # Affichage mosaïque
          
          box(
            title = "Mosaïque",
            width = 9,
            height = 12,
            solidHeader = TRUE,
            status = "primary",
            plotOutput("print_mosaic_clusters")
          )
        )
      })
  )}

  