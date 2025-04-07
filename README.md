
This folder has R scripts that can reproduce the results from 

El Graoui, M., M.E. Ghanem, M. Amri, R.J. Hijmans, 2025. A distance-based framework for assessing the ex-situ conservation status of plants. Submitted for publication. 

Before you run any of them, create a new directory and make that your working directory. 

For example 

dir.create("vigna", FALSE, FALSE)
setwd("vigna")

You need to following packages from CRAN: terra, igraph, geodata, rJava, predicts, remotes. You can install these with

install.packages(c("terra", "igraph", "geodata", "rJava", "predicts", "remotes"))

You also need the "XSdata" and "conexus" package that you can install with 

remotes::install_github("rspatial/conexus")
remotes::install_github("elgraouim/XSdata")


The scripts need to be run in the order of the numbering. That is 

0_data.R
1_sdm_model.R 
2_geo_env_model.R 
etc...

