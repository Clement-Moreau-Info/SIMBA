  # Ciel de mots
  
  ciel_de_mots <- function(myInputs, myData, input, session, output, val) { 
  
      return(renderPlot({
        
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
          
          
          
          id_all$ID  <- as.character(gsub("NULL", paste("NULL", col, sep = "_"), id_all$ID))
          id_all <- id_all[order(id_all$ID),]
          
          # Si l'une des colonnes font partie de celles des ontologies, agréger les ID
          if (grepl("move|stop|lieu|accomp", col)) {
            id_all$ID <- unlist(lapply(id_all$ID, agg_id, val = val))
          }
          
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
            id_all$Name <- unlist(lapply(id_all$ID, agg_id, val = val))
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
        }
        
        nbmots <- input$nbmots
        
        # Affichage des nuages en fonction du nombre de mots par nuages (slider)
        
        display_clouds(input$nbmots)
     
      })
  )}
