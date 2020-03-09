library(testthat)
# load simulation files
sim_path <- 'C:/myProjects/ShinyApps/MultiplicitySim/R/sim'
for (fname in list.files(sim_path, recursive = T)) source(file.path(sim_path, fname))
# run tests
test_dir('C:/myProjects/ShinyApps/MultiplicitySim/tests/')
