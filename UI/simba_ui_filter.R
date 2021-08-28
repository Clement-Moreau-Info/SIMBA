FilterItem <- tabItem("filtrage",
            tags$div(class = "container-fluid",
                     tags$div(
                       class = "row",
                       
                       # Panel boutons filtrage + Refresh #
                       tags$div(
                         class = "col-4",
                         box(
                           width = 4,
                           title = "Filtrer les données",
                           status = "primary",
                           solidHeader = TRUE,
                           uiOutput("inputs"),
                           uiOutput("buttons")
                         )
                       ),
                       
                       # Panel aperçu des données filtrées + Export #
                       tags$div(
                         class = "col-sm-8",
                         tags$div(style = "overflow-y: scroll;overflow-x: scroll",
                                  DT::dataTableOutput("table_filtree")),
                         tags$div(
                           class = "text-center",
                           downloadButton(
                             outputId = "save_donnees_filtrees",
                             label = "Exporter les données filtrées (CSV)",
                             icon = icon(name = "download"),
                             class = "btn btn-primary",
                             style = "color:#fff"
                           )
                         )
                       )
                     )
                    )
              )