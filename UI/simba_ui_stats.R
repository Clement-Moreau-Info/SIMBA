# - Onglet Statistiques sur les séquences - #

StatsSeqItem <- tabItem(
      "sequences",
      tabsetPanel(
        selected = "Longueurs des séquences",
        
        # Onglet longueurs des séquences #
        tabPanel(
          "Longueurs des séquences",
          box(
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            title = "Paramètres",
            sliderInput(
              inputId = "pas",
              label = "Bar width",
              min = 1,
              max = 10,
              value = 2
            ),
            sliderInput(
              inputId = "min",
              label = "Valeur minimum sur l'axe:",
              min = 0,
              max = 50,
              value = 0
            ),
            sliderInput(
              inputId = "max",
              label = "Valeur maximum sur l'axe:",
              min = 10,
              max = 100,
              value = 50
            )
          ),
          
          box(
            status = "primary",
            title = "Histogramme",
            width = 8,
            solidHeader = TRUE,
            plotOutput(outputId = "longueur")
          )
        ),
        
        # Onglet Distribution des fréquences #
        tabPanel(
          "Distribution des fréquences",
          box(
            title = "Distribution des fréquences",
            status = "primary",
            solidHeader = TRUE,
            plotlyOutput(outputId = "barplot",width = "100%")
          ),
          box(
            title = "Loi de Zipf",
            status = "primary",
            solidHeader = TRUE,
            plotOutput(outputId = "zipf")
          ),
          box(
            title = "Choix des dimensions",
            radioButtons(
              "radio",
              "Choisir la dimension à afficher",
              choices = list(
                "Stop" = 1,
                "Move" = 2,
                "Lieux" = 3,
                "Accompagnements" = 4
              ),
              selected = 1
            )
          )
        ),
        
        # Onglet diagramme de flux #
        tabPanel(
          "Diagramme de flux",
          box(
            width = 4,
            checkboxInput("checkbox_agg_flow", "Agréger les flux", value = FALSE),
            sliderInput(
              inputId = "filtre_flux",
              label = "Conserver les flux représentants au moins (en %) :",
              min = 0,
              max = 25,
              value = 5
            )
          ),
          box(width = 8,
              chorddiagOutput(outputId = "flow", height = 600))
        )
        
        # Onglet Daily patterns #
         ,tabPanel("Daily Patterns"
         ,plotOutput(outputId = "daily_pattern"),
         plotOutput(outputId = "network")),
        
        
        
        # TODO Tapis de séquences #
        tabPanel("Tapis de séquences")
      )
    )
    
    
    
    # - Onglet statistiques sur les individus - #
    
StatsIndItem <- tabItem("individus",
           uiOutput("choix_dim_mosaique"),
            #box(
            #  title = "Table de contingence",
            #  solidHeader = TRUE,
            #  tableOutput("view_ind")
            #),
          )