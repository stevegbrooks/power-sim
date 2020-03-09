###############################################################################################################################
############################# Graphical Procedure Simulation Code #############################################################
###############################################################################################################################
#
# Authors: Hu Na, Frank Fleischer, Steven Brooks 
#
# Last update: 22-MAR-2019
#
# Version: 0.01
# Status: unvalidated
# Description: functions for running a simulation of two groups being compared to a third group
# using the graphical procedure for multiple testing over multiple endpoints.
# Dependencies: Uses the 'GraphicalUtilities.R' script.
###############################################################################################################################
# Parameters: 
###############################################################################################################################
# g1 and g2
#
# The parameters 'g1' and 'g2' must be lists, one for each group, containing vectors of p-values.
# Each vector in each list corresponds to an endpoint, and the contents of the vectors are generated from 
# the hypothesis tests against a third group, usually placebo.
# 
# The first vector in the list is assumed to be the primary endpoint,
# and the rest are assumed to be secondary endpoints.
###############################################################################################################################
# primary
###############################################################################################################################
# prev
###############################################################################################################################
# alpha
###############################################################################################################################
# gamma
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################

library(assertthat)
library(dplyr)

graphical <- function(g1, g2, primary, prev, alpha0, alpha1) {
  if (length(g1) == 1) {
    if (all(is.na(prev))) {
      return(data.frame(graphical.se2(g1, g2, primary, prev = primary, alpha0, alpha1),
                        fix.empty.names = F))
    } else {
      return(graphical.se2(g1, g2, primary, prev, alpha0, alpha1))
    }
  } else {
    if (all(is.na(prev))) {
      curr <- graphical.se1(g1[[1]], g2[[1]], primary, alpha0, alpha1)
    } else {
      curr <- graphical.se2(g1, g2, primary, prev, alpha0, alpha1)
    }
    return(data.frame(curr, Recall(g1 = g1[-1], 
                                   g2 = g2[-1], 
                                   primary = primary, 
                                   prev = curr, 
                                   alpha0 = alpha0, 
                                   alpha1 = alpha1),
                      fix.empty.names = F))
  }
}

graphical.update <- function(g1, g2, results.gr, alpha0) {
  primary <- results.gr[1]
  secondary <- results.gr[-1]
  numSE <- length(secondary)
  
  toUpdate <- if_else(primary != 3, 0,
                      if_else(secondary[numSE] == 1, 2,
                              if_else(secondary[numSE] == 2, 1, 0)))
  
  updated <- graphical.update.recur(g1, g2, toUpdate, prev = NA, alpha0)
  
  updated <- as.data.frame(cbind(primary, as.matrix(secondary) + as.matrix(updated)))
  updated <- apply(updated, c(1,2), function(x) if (x > 3) x <- 3 else x)
  updated <- as.data.frame(updated)
  names(updated) <- NULL
  return(updated)
}