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
  chorddiag,
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
  ape,
  optrees,
  dendextend,
  FactoMineR,
  shiny,
  chorddiag,
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

options(shiny.maxRequestSize = 150*1024^2)
options(encoding="UTF-8")

source('./UI/simba_ui.R', chdir=T)
source('./Server/simba_server.R', chdir=T)

# Lancement de l'application

shinyApp(ui = ui, server = server)
