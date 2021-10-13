# - Fonctions statistiques descriptives sur les individus - #

mosaicInd <- function(myInputs, myData, input, session, output, val) { 
    return(renderUI({
    
        # Filtrer les dimensions mosaique avec 10+ valeurs différentes (onglet Individus)

        inFile <- val$file_filtre
        liste_dimensions <- colnames(inFile)
        liste_dimensions_filtree <- lapply(liste_dimensions, 
                                        function(x) {
                                            valeurs <- as.character(unique(inFile[x])[, 1])
                                            if (length(valeurs) > 15) {
                                                return(NULL)
                                            }
                                            return(x)
                                        }
                                    )
    
        if (!is.null(inFile)) {
            liste_dimensions_filtree <- as.character(liste_dimensions_filtree[liste_dimensions_filtree != "NULL"])
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
    )}