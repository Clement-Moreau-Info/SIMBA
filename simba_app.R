# Installation des packages requis

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  shiny,
  shinydashboard,
  shinyjs,
  stats,
  hash,
  plotly,
  circlize,
  readr,
  comprehenr,
  ggplot2,
  clValid,
  raster,
  reticulate,
  igraph,
  stringr,
  lubridate,
  networkD3,
  openxlsx,
  plyr,
  dplyr,
  DataCombine,
  data.table,
  purrr,
  arules,
  arulesViz,
  tidyverse,
  RColorBrewer,
  OpenImageR,
  ggpubr,
  gplots,
  vcd,
  wesanderson,
  gmodels,
  descr,
  SciViews,
  vcd,
  wordcloud,
  extrafont,
  gridExtra,
  magrittr,
  cowplot,
  plotfunctions,
  draw,
  fpc,
  dbscan,
  factoextra,
  spdep,
  ape,
  optrees,
  dendextend,
  FactoMineR,
  shiny,
  sf,
  shinyWidgets,
  DT,
  shinydashboard,
  shinyjs,
  igraph,
  networkD3,
  htmlwidgets,
  shinyalert,
  grDevices,
  ROCR
)

library(shiny)
library(shinydashboard)
library(shinyjs)

# Encodage et taille maximale d'upload de fichiers

options(shiny.maxRequestSize = 80*1024^2)
options(encoding="UTF-8")

source('simba_ui.R', local = TRUE)
source('simba_server.R')

# Lancement de l'application

shinyApp(ui = ui, server = server)
