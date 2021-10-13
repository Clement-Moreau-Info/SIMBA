LoadItem <- tabItem(
      "chargement",
      
      # Fichier de séquences #
      box(
        title = "Séquences",
        fileInput(inputId = 
                    "file_sequences",label = 
                    "Charger le fichier des séquences",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
        ),
        radioButtons(
          inputId = "sep_seq",
          label = "Séparateur",
          choices = c(
            Virgule = ",",
            Espace = " ",
            "Point-virgule" = ";",
            Tabulation = "\t"
          ),
          selected = ";"
        )
        ,
        status = "primary",
        solidHeader = TRUE
      ),
      
      # Fichier des individus #
      box(
        title = "Individus",
        fileInput(
          inputId = "file_individus",
          label = "Charger le fichier des individus",
          multiple = FALSE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),  
        radioButtons(
          inputId = "sep_ind",
          label = "Séparateur",
          choices = c(
            Virgule = ",",
            Espace = " ",
            "Point-virgule" = ";",
            Tabulation = "\t"
          ),
          selected = ";"
        ), 
        status = "primary",
        solidHeader = TRUE
      ),
      tags$hr(),
      
      # Fichier de matrice des distances #
      box(
        title = "Distances",
        fileInput(
          inputId = "file_distances",
          label = "Charger la matrice des distances",
          multiple = FALSE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ), 
        radioButtons(
          inputId = "sep_dist",
          label = "Séparateur",
          choices = c(
            Virgule = ",",
            Espace = " ",
            "Point-virgule" = ";",
            Tabulation = "\t"
          ),
          selected = " "
        ),
        status = "primary",
        solidHeader = TRUE
        
      )
    )