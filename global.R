library(shiny)
library(ggplot2)
library(dplyr)
library(data.table)
library(DT)

# set paths
sim_path = "R/sim"
methods_path = paste0(sim_path, "/methods")
modules_path = "R/modules"
server_path = "R/server"

# load default scenarios
source("defaultScenarios.R")

# load simulation files
for (fname in list.files(sim_path, recursive = T)) source(file.path(sim_path, fname))
# load modules
for (fname in list.files(modules_path, recursive = T)) source(file.path(modules_path, fname))

#define inputfile
scenarios <- reactiveValues()

