 OntologyItem <- tabItem("ontologie",
            tabsetPanel(
              
              # Onglet Chargement ontologies (4 fichiers) #
              
              tabPanel(
                "Charger les ontologies",
                box(
                  title = "Fichiers des ontologies",
                  fileInput(
                    inputId = "files_onto",
                    label = "Charger les ontologies (stop,move,lieux et accompagnements)",
                    multiple = TRUE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                  ),
                  radioButtons(
                    inputId = "sep_onto",
                    label = "Séparateur",
                    choices = c(
                      Virgule = ",",
                      Espace = " ",
                      "Point-virgule" = ";",
                      Tabulation = "\t"
                    ),
                    selected = ","
                  )
                  ,
                  status = "primary",
                  solidHeader = TRUE
                )
              ),
              
              # Onglet Visualisation des ontologies #
              
              tabPanel(
                "Visualisation de l'ontologie",
                box(
                  status = "primary",
                  solidHeader = TRUE,
                  width = 3,
                  title = "Choix des dimensions à afficher",
                  radioButtons(
                    inputId = "radio_onto",
                    label = "Choisir la dimension à afficher",
                    choices = list(
                      "Stop" = 1,
                      "Move" = 2,
                      "Lieux" = 3,
                      "Accompagnements" = 4
                    ),
                    selected = 1
                  )
                ),
                box(
                  title = "Ontologie",
                  width = 9,
                  height = 12,
                  solidHeader = TRUE,
                  status = "primary",
                  forceNetworkOutput("onto", height = "600px")
                )
              )
            ))