# Simba app

Coded by Clément Moreau and Mathis Rharbal. 

For questions and remarks contact : cmoreau.info@gmail.com

## Preliminary installations 

Please install an [Anaconda](https://www.anaconda.com/products/individual) / Python / Miniconda version on your computer. 

## Run the app

For run the Simba : 

1. Dowload the project.
 
2. Place it in your R workspace.

3. Simply run the file named `simba_app.R` on RStudio.

## Play with data

Simba can be tested using the files in the folder "Data". 

* `sequences.csv`    is the file containing all the semantic mobility sequences.

* `individuals.csv`  is the file containing all the socio-demographic variables that provide information on the individuals who perform the sequences. 

* `dist.csv`         is the distance matrix between all paired sequences. 

Simba uses a set of ontologies to compare and display statistics and data. Ontologies can be imported using the knowledge graph format used in the folder `Ontology`. 

## UI File: `simba_ui.R`

UI file codes SIMBA user interface. Currently composed of 5 main panels which can be found in the UI folder : 

* Loading panel   ==> `simba_ui_load.R`

* Ontology panel  ==> `simba_ui_ontology.R`

* Filter panel    ==> `simba_ui_filter.R`

* Stats panel     ==> `simba_ui_stats.R`   (Composed of two sub-panels for stats (i) on sequences (ii) on individuals)    

* Cluster panel   ==> `simba_ui_cluster.R`

## Server File: `simba_server.R`

* Button and action gestion   ==> `simba_server_button.R`

* Ontology display ==> `simba_server_ontology.R`

* Statistical functions on Sequences ==> `simba_server_statSeq.R`

* Statistical functions on Individuals ==> `simba_server_statInd.R`

* Clustering and statistics on clusters ==> `simba_server_cluster.R`

* Cloud words ==> `simba_server_ciel.R`
