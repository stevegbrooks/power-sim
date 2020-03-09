###############################################################################################################################
############################# Truncated Hochberg Procedure Simulation Code ####################################################
###############################################################################################################################
#
# Authors: Hu Na, Frank Fleischer, Steven Brooks 
#
# Last update: 22-MAR-2019
#
# Version: 0.01
# Status: unvalidated
# Description: functions for running a simulation of two groups being compared to a third group
# using the truncated hochberg procedure for multiple testing over multiple endpoints.
# Dependencies: uses the 'TruncatedHochbergUtilities.R' script
#
# Parameters: requires an input of two lists, one for each group. Each element in each list corresponds
# to a vector of p-values generated from simulated data comparing the group to the third group, usually placebo.
# Each vector represents an endpoint. The first vector in the list is assumed to be the primary endpoint,
# and the rest are assumed to be secondary endpoints.
#
###############################################################################################################################
###############################################################################################################################
###############################################################################################################################


truncHoch <- function(g1, g2, prev, alpha, gamma) {
  if (length(g1) == 1) {
    return(truncHoch.se(g1, g2, prev, alpha, gamma, last = T))
  } else {
    if (all(is.na(prev))) {
      curr <- truncHoch.cope(g1 = g1[which(grepl("pe", names(g1)))], 
                             g2 = g2[which(grepl("pe", names(g2)))],
                             alpha, gamma)
      g1 <- g1[which(grepl("pe1|se[0-9]", names(g1)))]
      g2 <- g2[which(grepl("pe1|se[0-9]", names(g2)))]
    } else {
      curr <- truncHoch.se(g1, g2, prev, alpha, gamma)
    }
    return(data.frame(curr, Recall(g1 = g1[-1], 
                                   g2 = g2[-1], 
                                   prev = curr, 
                                   alpha = alpha, 
                                   gamma = gamma), 
                      fix.empty.names = F))
  }
}
