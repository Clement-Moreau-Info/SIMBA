## --- Fonctions --- ##  
  
# ======= #  
# BARPLOT # 
# ======= #


  
### Calcul de la distribution ###
  
  freq_mk <- function(vect, val) {
    
    x <- unlist(str_split(vect, " ")) # Split des multi-activités

    df <- as.data.frame(table(x[x != "NULL"]))

    colnames(df) <- c("activity", "freq")
    df <- df[order(-df$freq), ]
    df$activity <- as.character(df$activity)
    df$Name <- as.character(lapply(df$activity,name_id, val=val))
    df$Color <- as.character(lapply(df$activity,col_id, val=val))
    df$Group <- as.character(lapply(as.character(lapply(df[,"activity"],agg_id, val=val)),name_id, val=val))
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
    }
    
  
  ### Affichage et choix de la dimension pour le barplot ###
  
  
  barplotAct <- function(myInputs, myData, input, session, output, val) {
      return (renderPlotly({
      
        req(val$file_filtre)
        
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
                vect <- subset(inFile, lieu_type != "NULL")$lieu_type
                titre_barplot <- "Distribution des lieux"
            },
            "4" = {
                vect <- subset(inFile, accompagnement != "NULL")$accompagnement
                titre_barplot <- "Distribution des accompagnements"
            }
        )
        freq_mk(vect, val)
    }))
    }
  
  
# ======== #  
# ZIPF LAW # 
# ======== #
  
  zipfAct <- function(myInputs, myData, input, session, output, val) {
    return (renderPlot({
        req(val$file_filtre)
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
                col = unlist(lapply(df[, 1], col_id, val=val)),
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
                vect <- subset(inFile, lieu_type != "NULL")$lieu_type
                titre_barplot <- "Distribution des lieux"
             },
            "4" = {
                vect <- subset(inFile, accompagnement != "NULL")$accompagnement
                titre_barplot <- "Distribution des accompagnements"
            }
        )
        zipf_mk(vect)
      }))
  }
  
# ====================== #  
# LONGUEUR DES SÉQUENCES # 
# ====================== #
  
  longueurSeq <- function(myInputs, myData, input, session, output, val) {
    return (renderPlot({
        req(val$file_filtre)
        inFile <- val$file_filtre
        size_seq <- as.numeric(table(inFile[, "id"]))
        
        hist(
            size_seq,
            xlim = c(input$min, input$max),
            breaks = (input$max - input$min) / input$pas,
            col = "skyblue"
        )
    }))
  }
  
# ============= #  
# CHORD DIAGRAM # 
# ============= #
  
 
  chord <- function(myInputs, myData, input, session, output, val) {
      return (renderPlot({
        build_flows <- function(sequences) {
        H <- hash()
        
        # Calcul des flux entre activités
        
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
        req(val$file_filtre)
        file <- val$file_filtre
        for (i in 1:max(file[, "id"])) {
            sequences[[i]] <- as.vector(as.character(subset(file, id == i)$lieu_type))
        }
        sequences_flow <- to_list(for (seq in sequences) seq[seq != "NULL"])
        
        # Calcul des flots agrégés et non-agrégés
        
        flows_non_agg <- build_flows(to_list(for (s in sequences_flow) s <- s))
        flows_agg <- build_flows(to_list(for (s in sequences_flow) lapply(s, agg_id, val = val)))
        
        if (!input$checkbox_agg_flow) {
            flow <- flows_non_agg
        }
        else {
            flow <- flows_agg
        }
        
        flow_filtre <- flow
        
        # Flux filtrés
        
        val$flow <- flow_filtre[flow_filtre$tij >= (input$filtre_flux / 100) * max(flow$tij), ]
        
        liste_lieux <- unique(union(as.character(unique(val$flow[, 1])), as.character(unique(val$flow[, 2]))))
        
        df_flow <- val$flow
        

        df_flow[, 1] <- as.character(lapply(df_flow[, 1],name_id, val=val))
        df_flow[, 2] <- as.character(lapply(df_flow[, 2],name_id, val=val))
       
        defaultW <- getOption("warn")
        options(warn = -1)
        
        # Affichage du graphique
        
        
        chordDiagram(
          df_flow,
          transparency = input$transparence_flux,
          row.col = as.character(lapply(liste_lieux, col_id, val=val)),
          grid.col = as.character(lapply(liste_lieux, col_id, val=val)),
          annotationTrack = c("name", "grid", "axis"),
          directional = 1,
          direction.type = c("arrows", "diffHeight"),
          diffHeight  = -0.04,
          annotationTrackHeight = c(0.1, 0.1, 0.1),
          link.arr.type = "big.arrow",
          link.sort = TRUE,
          link.largest.ontop = TRUE
        )
        circos.clear()
        options(warn = defaultW)
        
      })
      )}





