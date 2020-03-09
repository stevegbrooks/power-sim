rm(list = ls())
library(data.table)
library(dplyr)

#read in file
userData <- fread("C:/myDocuments/ShinyApps/MultiplicitySim/userScenarios_contTest.csv", header=TRUE)
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
#create scenarios list
if ("Value2" %in% names(userData)) {
  scenariosFile = dcast(userData, Scenario + Endpoint ~ Group, value.var = c("Value", "Value2"))
} else {
  scenariosFile = dcast(userData, Scenario + Endpoint ~ Group, value.var = c("Value"))
  scenariosFile = cbind(scenariosFile, matrix(data=NA,nrow=length(scenariosFile),ncol=3))
}
scenariosFile[, Scenario := factor(Scenario)]
valueCols <- grep("[^Scenario|Endpoint]", names(scenariosFile), value = T)
scenarios = lapply(levels(scenariosFile$Scenario),
                   function(x)
                     as.data.frame(scenariosFile[Scenario == x, ..valueCols]))

scenarios <- lapply(scenarios, 'rownames<-' , unique(scenariosFile[, Endpoint]))

names(scenarios) <- levels(scenariosFile$Scenario)
print(scenarios)

if (length(groupLabels) > 1) {
  groupOutcomeLabels = c("At Least One", "All", groupLabels)
} else {
  groupOutcomeLabels = groupLabels
}

####Simulate Data#########
library(copula)
library(gsDesign)

groupParams <- rbind(c(140, 140, 140), scenarios[[1]])
rho <- .5
c <- c(1,1,.4,.3,1)
nsim <- 100

simCorrData <- function(groupParams, c, rho, nsim) {
  numGroups <- 3
  numEndpoints <- dim(groupParams)[1] - 1
  cop <- normalCopula(rho, dim = numEndpoints, dispstr = "ex")
  sim <- lapply(seq(1:numGroups), 
                function(x) {
                  randx <- rCopula(nsim, cop)
                  lapply(seq(numEndpoints), 
                         function(y) {
                           if (is.na(groupParams[y+1, x + numGroups])) {
                             as.integer(qbinom(randx[, y], 
                                               floor(c[[y]] * groupParams[1, x]), #group size
                                               groupParams[y+1, x])) #probability of success
                           } else {
                             as.numeric(qnorm(randx[, y],
                                              groupParams[y+1, x], #mean
                                              groupParams[y+1, x + numGroups])) #sd
                           }
                         }
                  )
                }
  )
  return(sim)
}

testContinuous <- function(x1, x2, n1, n2, sd1, sd2) {
  numerator <- x1 - x2
  pooledSD <- sqrt(((n1 - 1)*(sd1^2) + (n2 - 1)*(sd2^2))/(n1+n2-2))
  equalVarDenom <- pooledSD * sqrt((1/n1) + (1/n2))
  return(numerator/equalVarDenom)
}

genTestStats <- function(groupParams, sim, c) {
  g1 <- sim[[1]]
  zScores <- lapply(seq(sim[2:length(sim)]), function(x) {
    lapply(seq(sim[[x + 1]]), function(y) {
      if (all(is.integer(sim[[x + 1]][[y]]))) {      
        testBinomial(x1 = sim[[x + 1]][[y]], 
                     x2 = g1[[y]], 
                     n1 = floor(c[[y]] * groupParams[1, x + 1]), 
                     n2 = floor(c[[y]] * groupParams[1, 1]),
                     delta0 = 0, chisq = 0, adj = 0,
                     scale = "Difference", tol = .1e-10)
      } else {
        testContinuous(x1 = sim[[x+1]][[y]],
                       x2 = g1[[y]],
                       n1 = groupParams[1, x + 1],
                       n2 = groupParams[1, 1],
                       sd1 = groupParams[y + 1, (x + 1) + 3],
                       sd2 = groupParams[y + 1, 4])
      }
    }
    )
  }
  )
  return(zScores)
}

#############TEST##################
library(microbenchmark)
library(ggplot2)
groupParams.c <- groupParams
groupParams.b <- cbind(groupParams[1:3], matrix(data = NA, nrow = 6, ncol = 3))
groupParams.b[c(3,5),] <- groupParams.b[c(3,5),]/100

# autoplot(microbenchmark(c = simCorrData(groupParams.c, c, rho, nsim),
#                         b = simCorrData(groupParams.b, c, rho, nsim)))

sim.c <- simCorrData(groupParams.c, c, rho, nsim)
sim.b <- simCorrData(groupParams.b, c, rho, nsim)

# autoplot(microbenchmark(c = genTestStats(groupParams.c, sim.c, c),
#                         b = genTestStats(groupParams.b, sim.b, c)))

testStats.b <- genTestStats(groupParams.b, sim.b, c)

pValues <- lapply(testStats.b, function(x) lapply(x, pnorm, lower.tail = F))

endpointLabels_default <- c("pe1", "pe2", "se1", "se2", "se3")

pValues <- lapply(pValues, setNames, nm = endpointLabels_default)
