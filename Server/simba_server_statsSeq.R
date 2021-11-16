## --- Fonctions --- ##  
  
# ======= #  
# BARPLOT # 
# ======= #


  
### Calcul de la distribution ###
  
  freq_mk <- function(vect, val) {
    
    x <- unlist(str_split(vect, "\\|")) # Split des multi-activités
    
    df <- as.data.frame(table(x[x != ""]))

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
                vect <- subset(inFile, Act != "")$Act
                titre_barplot <- "Distribution des activités stop"
            },
            "2" = {
                vect <- subset(inFile, Move != "")$Move
                titre_barplot <- "Distribution des activités move"
            },
            "3" = {
                vect <- subset(inFile, Place != "")$Place
                titre_barplot <- "Distribution des lieux"
            },
            "4" = {
                vect <- subset(inFile, Acc != "")$Acc
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
            x <- unlist(str_split(vect, "\\|"))
            df <- as.data.frame(table(x[x != ""]))
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
                vect <- subset(inFile, Act != "")$Act
                titre_barplot <- "Distribution des activités stop"
            },
            "2" = {
                vect <- subset(inFile, Move != "")$Move
                titre_barplot <- "Distribution des activités move"
            },
            "3" = {
                vect <- subset(inFile, Place != "")$Place
                titre_barplot <- "Distribution des lieux"
             },
            "4" = {
                vect <- subset(inFile, Acc != "")$Acc
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
        size_seq <- as.numeric(table(inFile[, "Id_seq"]))
        
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
      return (renderChorddiag({
        
        source_python('Server/simba_flow_matrix.py')
        od_matrix("Data/seq_mk.csv")
        flow <- read_delim("Data/flow.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)
        
        flow[flow < max(flow) * input$filtre_flux / 100] <- 0
        
        flow.col <- as.character(lapply(names(flow), col_id, val=val))
        
        names(flow) <- as.character(lapply(names(flow), name_id, val=val))
        
        p <- chorddiag(as.matrix(flow), groupColors = flow.col, showTicks = F, groupnameFontsize = 12, groupnamePadding = 10)
        p
        })
      )}

# ============= #  
# DAILY PATTERN # 
# ============= #


daily_pat <- function(myInputs, myData, input, session, output, val) {
    return(renderPlot({ 
        source_python('Server/simba_daily_pattern.py')
        daily_pattern <- daily_pattern_freq("Data/seq_mk.csv")
        daily_pattern_names <- c("1", "2-1", "2-2", "3-1", "3-2", "3-3", "3-*", "4-1", "4-2", "4-3", "4-*", "5-1", "5-2", "5-3", "5-*", "> 5")
        barplot(unlist(daily_pattern,recursive=FALSE),
             xlab = "Network id",
             ylab = "Percentage of network id",
             names.arg =  daily_pattern_names,
             col = "#1F2899")
        })
        )}
        
        
        # Network daily pattern
daily_net <- function(myInputs, myData, input, session, output, val) {
     return(renderPlot({ 
        tryCatch({
             
            par(mfrow=c(2,6))
            
            daily_pattern_names <- c("1", "2-1", "2-2", "3-1", "3-2", "3-3", "4-1", "4-2", "4-3", "5-1", "5-2", "5-3")
    
            for(i in 1:length(daily_pattern_names)) {
            
                path <- paste("Daily_pattern/graph", daily_pattern_names[i], ".csv", sep = "")
                net_csv <- read_delim(path, ";", col_names = FALSE)
                network <- graph_from_adjacency_matrix(as.matrix(net_csv))
                col <- rep("gray", nrow(net_csv))
                red <- which.max(rowSums(net_csv))
                col[red] <- ifelse(max(rowSums(net_csv)) > 1, "red", "gray")
                plot(network, vertex.size=32, edge.curved=0.3, layout=layout.circle, 
                vertex.label=NA, vertex.color=col, edge.color="black", edge.arrow.size=.25, 
                main = paste("Daily pattern id: ", daily_pattern_names[i], sep = ""))
    
            }
            
        }, error=function(e){
        
           stop(safeError(e))
        })
    })
    )}



