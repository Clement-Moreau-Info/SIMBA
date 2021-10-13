ClusterItem <- tabItem(
      "clustering",
      tabsetPanel(
        id = "clustering_tabs",
        
        # Onglet Clustering Hiérarchique #
        
        tabPanel(
          "Clustering Hiérarchique",
          box(
            sliderInput(
              inputId = "cluster_nb",
              label = "Number of clusters:",
              min = 2,
              max = 10,
              value = 6
            ),
            radioButtons(
              inputId = "cluster_method",
              label = "Method :",
              choices = list(
                "Ward" = "ward.D2",
                "Complete" = "complete",
                "Single Linkage" = "single"
              ),
              selected = "ward.D2"
            )
          ),
          box(status = "primary",
              title = "Dendrogramme",
              plotOutput("clusters"),
              solidHeader = TRUE),
          tags$div(
            class = "text-center",
            downloadButton(
              outputId = "save_cluster",
              label = "Exporter les clusters (CSV)",
              icon = icon(name = "download"),
              class = "btn btn-primary",
              style = "color:#fff"
            )
          )
        ),
        
        # Onglet Qualité des clusters #
        
        tabPanel(
          "Qualité",
          box(
            status = "primary",
            title = "Qualité des clusters",
            solidHeader = TRUE,
            width = 5,
            tableOutput("quality")
          ),
          box(
            status = "primary",
            title = "Inertie et silhouette selon le nombre de clusters",
            solidHeader = TRUE,
            width = 7,
            plotOutput(outputId = "inertia")
          )
        ),
        
        # Longueur des séquences / cluster
        
        tabPanel(
          "Boxplot des longueurs",
          
          box(plotOutput(outputId = "boxplot_length"))
        ),
        
        
        # onglet barres empilées par cluster et par dimensions #
        
        tabPanel(
          "Barres empilées",
          box(
            "Choix du type de dimension",
            width = 4,
            status = "primary",
            solidHeader = TRUE,
            radioButtons(
              inputId = "radio_stack",
              label = "",
              choices = list(
                "Stop" = "stop",
                "Move" = "move",
                "Lieux" = "lieu_type",
                "Accompagnement" = "accompagnement"
              ),
              selected = "stop"
            ),
            checkboxInput(
              inputId = "stack_nonull",
              label = "Ne pas afficher les valeurs nulles",
              value = TRUE
            )
          ),
          box(status = "primary",
              title = "Barres empilées",
          plotOutput(outputId = "stack"),
          solidHeader = TRUE)
        ),
        
        # Onglet mosaïques par cluster et par dimension #
        
        tabPanel("Mosaïque par cluster",
                 uiOutput("mosaiques_clusters")),
        
        # TODO : Onglet Diagramme de Sankey #
        
        # tabPanel("Sankey",
        #          box(sankeyNetworkOutput("sankey"))),
        
        # Onglet Ciel de mots #
        
        tabPanel("Ciel de mots",
                 tags$div(class = "container-fluid",
                          tags$div(
                            class = "row",
                            tags$div(
                              class = "col-sm-3",
                              box(
                                width = 12,
                                title = "Choix des dimensions (Non fonctionnel)",
                                status = "primary",
                                solidHeader = TRUE,
                                uiOutput("choix_dim_ciel"),
                                checkboxInput(
                                  inputId = "agg_ciel",
                                  label = "Agréger les valeurs",
                                  value = FALSE
                                ),
                                disabled(
                                  checkboxInput(
                                    inputId = "noms_ciel",
                                    label = "Changer les ID en noms (nécessite les fichiers d'ontologie)",
                                    value = FALSE
                                  )
                                )
                                
                              ),
                              box(
                                width = 12,
                                title = "Nombre de mots par nuage",
                                status = "primary",
                                solidHeader = TRUE,
                                sliderInput(
                                  inputId = "nbmots",
                                  label = "",
                                  min = 1,
                                  max = 15,
                                  value = 6
                                )
                              )
                            ),
                            tags$div(
                              class = "col-sm-9",
                              plotOutput("ciel", height = "600px", width = "800px")
                            )
                          )))
      )
    )