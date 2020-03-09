###### get inputs ######
# Scenarios
if (input$dataSource == "upload") {
  userData <- isolate(get_file())
  # extract labels
  endpointLabels <- as.character(unique(userData[, Endpoint]))
  groupLabels <- as.character(unique(userData[, Group]))[-1]
  if ("EndpointLabels" %in% names(userData)) {
    if (length(userData[EndpointLabels != "", EndpointLabels]) > 0) {
      endpointLabels <- userData[EndpointLabels != "", EndpointLabels]
    }
  }
  if ("GroupLabels" %in% names(userData)) {
    if (length(userData[GroupLabels != "", GroupLabels]) > 0) {
      groupLabels <- userData[GroupLabels != "", GroupLabels][-1]
    }
  }
  if (length(groupLabels) > 1) {
    groupOutcomeLabels = c("At Least One", "All", groupLabels)
  } else {
    groupOutcomeLabels = groupLabels
  }
  endpointLabels_default <- as.character(unique(userData[, Endpoint]))
  #create scenarios list
  if ("Value2" %in% names(userData)) {
    scenariosFile = dcast(userData, Scenario + Endpoint ~ Group, value.var = c("Value", "Value2"))
  } else {
    scenariosFile = dcast(userData, Scenario + Endpoint ~ Group, value.var = c("Value"))
    scenariosFile = cbind(scenariosFile, matrix(data=NA,nrow=length(scenariosFile),ncol=3))
  }
  #prepare for sim functions
  scenariosFile[, Scenario := factor(Scenario)]
  valueCols <- grep("[^Scenario|Endpoint]", names(scenariosFile), value = T)
  scenarios = lapply(levels(scenariosFile$Scenario),
                     function(x)
                       as.data.frame(scenariosFile[Scenario == x, ..valueCols]))
  
  scenarios <- lapply(scenarios, 'rownames<-' , unique(scenariosFile[, Endpoint]))
  names(scenarios) <- levels(scenariosFile$Scenario)
} else {
  scenarios = defaultScenarios
  endpointLabels = c("Primary endpoint",
                     "Key secondary 1",
                     "Key secondary 2",
                     "Key secondary 3",
                     "Key secondary 4")
  groupOutcomeLabels = c("At least one dose",
                         "Both doses",
                         "Low dose",
                         "High dose")
  endpointLabels_default <- c("pe1", paste0("se", seq(1:4)))
}
if (length(grep("pe", endpointLabels_default)) > 1) {
  endpointLabels <- c(paste(endpointLabels[which(grepl("pe", endpointLabels_default))], collapse = "/"), 
                      endpointLabels[which(grepl("se", endpointLabels_default))])
}
print(scenarios)
# Sample sizes
nStart = isolate(get_nStart())
nEnd = isolate(get_nEnd())
nBy = isolate(get_nBy())
nAlloc = isolate(get_nAlloc())
if (length(nAlloc) != 3) {
  stop("Length of sample size allocation vector not equal to 3.")
}
# subgroup proportions per endpoint
subgrp_prop = isolate(get_subgrp_prop())
if (length(subgrp_prop) == 0) {
  subgrp_prop = rep(1, dim(scenarios[[1]])[1])
} else {
  if (length(subgrp_prop) != dim(scenarios[[1]])[1]) {
    stop("Length of subgroup proportion vector not equal to number of endpoints.")
  } 
}
# gamma
gamma = seq(from = isolate(get_gammaStart()),
            to = isolate(get_gammaEnd()),
            by = isolate(get_gammaBy()))
# the rest
rho = isolate(get_rho())
alpha = isolate(get_alpha())
nsim = isolate(get_nsim())
seed = isolate(get_seed())
grUpdate = input$gr_update 

source(file.path(server_path, "Simulator.R"), local = T)
source(file.path(server_path, "Display.R"), local = T)