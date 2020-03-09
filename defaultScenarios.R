#########################################################
#title: Scenario Settings for Power Simulation - 1368.20
#date: April 9, 2018
#########################################################
###########################################################################
scenarioChanger <- function(baseScenario, mat, times) {
  output <- list()
  for (i in 1:times) {
    if (i == 1) {
      output[[i]] <- baseScenario
    } else {
      output[[i]] <- output[[i - 1]] + mat
    }
  }
  return(output)
}
###########################################################################
ne <- 5 #number of endpoints
base <- cbind(c(0.20, 0.20, 0.20, 0.25, 0.15),
              c(0.50, 0.50, 0.50, 0.60, 0.30),
              c(0.50, 0.50, 0.50, 0.60, 0.30))
################################################################################################################
lowDoseDiminish <- scenarioChanger(baseScenario = base, 
                                   mat = cbind(rep(0,ne), 
                                               rep(-.05,ne), 
                                               rep(0,ne)),
                                   times = 5)
lowDoseDiminish[[5]][5,2] <- 0.15
names(lowDoseDiminish) <- c("BASE ", "BASE, LD-", "BASE, LD--", "BASE, LD---", "BASE, LD----")
################################################################################################################
# PLC+/BASE  keep the original difference, i.e. 0.55, 0.55, 0.55, 0.65, and 0.35
plcPlusBase <- base + .05
# PLC++/BASE  keep the original difference, i.e. 0.60, 0.60, 0.60, 0.70, and 0.4
plcPlusPlusBase <- base + .10
# PLC+/BASE-  PLC increase by 5% AND difference vs PLC reduced by 5
plcPlusBaseMinus <- cbind(base[,1] + .05,
                          plcPlusBase[,2:3] - .05)
#   PLC++/BASE--  PLC increase by 10% AND difference vs PLC reduced by 10
plcPlusPlusBaseMinusMinus <- cbind(base[,1] + .10,
                                   plcPlusPlusBase[,2:3] - .10)
#   PLC+/BASE--  PLC increase by 5% AND difference vs PLC reduced by 10
plcPlusBaseMinusMinus <- cbind(base[,1] + .05,
                               plcPlusBase[,2:3] - .10)
#   PLC++/BASE-  PLC increase by 10% AND difference vs PLC reduced by 5%
plcPlusPlusBaseMinus <- cbind(base[,1] + .10,
                              plcPlusPlusBase[,2:3] - .05)
# Others:
baseMinus <- cbind(base[,1],
                   base[,2:3] - .05)
baseMinusMinus <- cbind(base[,1],
                        base[,2:3] - .10)
#Please order as:
#BASE, BASE-, BASE--, PLC+/BASE, PLC+/BASE-, PLC+/BASE--, 
#PLC++/BASE, PLC++/BASE-, PLC++/BASE-- … and then the new scenarios in order of decreasing low dose effect…
plcAugmentBaseDiminish <- list('BASE' = base,
                               'BASE-' = baseMinus,
                               'BASE--' = baseMinusMinus,
                               'PLC+/BASE' = plcPlusBase,
                               'PLC+/BASE-' = plcPlusBaseMinus,
                               'PLC+/BASE--' = plcPlusBaseMinusMinus,
                               'PLC++/BASE' = plcPlusPlusBase,
                               'PLC++/BASE-' = plcPlusPlusBaseMinus,
                               'PLC++/BASE--' = plcPlusPlusBaseMinusMinus)
################################################################################################################
plcPlusLowDoseDiminish <- scenarioChanger(baseScenario = plcPlusBase,
                                          mat = cbind(rep(0,ne), 
                                                      rep(-.05,ne), 
                                                      rep(0,ne)),
                                          times = 5)
plcPlusLowDoseDiminish[[5]][5,2] <- 0.20
names(plcPlusLowDoseDiminish) <- c("PLC+/BASE ", 
                                   "PLC+/BASE, LD-", 
                                   "PLC+/BASE, LD--", 
                                   "PLC+/BASE, LD---", 
                                   "PLC+/BASE, LD----")
################################################################################################################
################################################################################################################
# Part 1
#defaultScenarios <- plcAugmentBaseDiminish
# Part 2
defaultScenarios <- lapply(plcPlusLowDoseDiminish, function(x) cbind(x, matrix(NA, dim(x)[1], 3)))
# Part 3
#defaultScenarios <- lowDoseDiminish
