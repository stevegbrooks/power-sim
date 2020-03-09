rm(list = ls())
library(dplyr)

#global inputs
alpha <- 0.025
gamma <- 0.5

peTest.classical <- function(primaryResults) {
  unlist(lapply(seq(primaryResults[[1]]), 
                function(x) {
                  if_else(primaryResults[[1]][x] == primaryResults[[2]][x],
                          primaryResults[[1]][x],
                          if_else(primaryResults[[1]][x] == 3
                                  | primaryResults[[2]][x] == 3,
                                  min(primaryResults[[1]][x], primaryResults[[2]][x]),
                                  0))
                }))
}

truncHoch.pe <- function(g1, g2, alpha, gamma) {
  g1 <- unlist(g1); g2 <- unlist(g2)
  indicator <- g1 >= g2
  pmin <- apply(cbind(g1, g2), 1, min)
  pmax <- apply(cbind(g1, g2), 1, max)
  both <- if_else(pmax <= (1 + gamma) * alpha/2, 1, 0)
  single <- if_else(pmax > (1 + gamma) * alpha/2 & pmin <= alpha/2, 1, 0)
  out <- rep(NA, length(g1))
  out[both == 1] <- 3
  out[single == 1 & indicator == 1] <- 2
  out[single == 1 & indicator == 0] <- 1
  out[single == 0 & both == 0] <- 0
  return(out)
}

truncHoch.cope <- function(g1, g2, alpha, gamma) {
  numPE <- length(g1[which(grepl("pe", names(g1)))])
  if (numPE == 0) {
    return(truncHoch.pe(g1[[1]], g2[[1]], alpha, gamma))
  } else {
    primaryResults <- lapply(seq(1:numPE), 
                             function(x) 
                               truncHoch.pe(g1[[x]], g2[[x]], alpha, gamma)
                             )
    if (length(primaryResults) > 1) {
      return(peTest.classical(primaryResults))
    } else {
      return(unlist(primaryResults))
    }
  }
}

truncHoch.se <- function(g1, g2, prev, alpha, gamma, last = F) {
  if (last) 
    out <- truncHoch.cope(g1, g2, alpha, 1)
  else
    out <- truncHoch.cope(g1, g2, alpha, gamma)
  out <- if_else(prev == 0, 0, out)
  out <- if_else(prev == 1,
                 if_else(g1[[1]] > (alpha * (1 - gamma))/2, 0, 1), 
                 out)
  out <- if_else(prev == 2,
                 if_else(g2[[1]] > (alpha * (1 - gamma))/2, 0, 2),
                 out)
  out[prev == 3 & out == 0] <- 0
  out[prev == 3 & out == 1] <- 1
  out[prev == 3 & out == 2] <- 2
  out[prev == 3 & out == 3] <- 3
  return(out)
}

truncHoch.new <- function(g1, g2, prev, alpha, gamma) {
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

#truncHoch.cope
g1      <- list('pe1' = c(.01, .001,   1,    1, .02, .01, .02, .01, 1))
g2      <- list('pe1' = c(  1,    1, .01, .001, .02, .02, .01, .01, 1))
expected <-             c(  1,    1,   2,    2,   0,   1,   2,   3, 0)
all.equal(truncHoch.cope(g1, g2, alpha, gamma), expected)

#truncHoch.se - last = F
g1       <- list('se1' = c(1, .001, 1,    1, .02, .01, .02, .01, 1))
g2       <- list('se1' = c(1,    1, 1, .001, .02, .02, .01, .01, 1))
prev     <-              c(1,    1, 2,    2,   3,   3,   3,   3, 0)
expected <-              c(0,    1, 0,    2,   0,   1,   2,   3, 0)
all.equal(truncHoch.se(g1, g2, prev, alpha, gamma), expected)

#truncHoch.se - last = T
g1       <- list('se1' = c(1, .001, 1,    1, .03, .01, .03, .01, 1))
g2       <- list('se1' = c(1,    1, 1, .001, .03, .03, .01, .01, 1))
prev     <-              c(1,    1, 2,    2,   3,   3,   3,   3, 0)
expected <-              c(0,    1, 0,    2,   0,   1,   2,   3, 0)
all.equal(truncHoch.se(g1, g2, prev, alpha, gamma, last = T), expected)

#truncHoch.cope
g1      <- list('pe1' = c(  1, .001,    1, .001, .001, .001,    1,    1, .001, .001),
                'pe2' = c(  1,    1, .001, .001, .001, .001, .001,    1,    1, .001))
g2      <- list('pe1' = c(  1,    1, .001,    1,    1, .001, .001, .001, .001, .001),
                'pe2' = c(  1, .001,    1, .001,    1,    1, .001, .001, .001, .001))
expected <-             c(  0,    0,    0,    1,    1,    1,    2,    2,    2,    3)
all.equal(truncHoch.cope(g1, g2, alpha, gamma), expected)

#trunchHoch with new cope function and co-primary endpoints
g1      <- list('pe1' = c(  1, .001,    1, .001, .001, .001,    1,    1, .001, .001, .001, .001, .001),
                'pe2' = c(  1,    1, .001, .001, .001, .001, .001,    1,    1, .001, .001, .001, .001),
                'se1' = c(  1, .001,    1, .001,    1, .001,    1, .001, .001, .001,    1,    1, .001))
g2      <- list('pe1' = c(  1,    1, .001,    1,    1, .001, .001, .001, .001, .001, .001, .001, .001),
                'pe2' = c(  1, .001,    1, .001,    1,    1, .001, .001, .001, .001, .001, .001, .001),
                'se1' = c(  1,    1, .001,    1, .001, .001, .001, .001,    1,    1, .001,    1, .001))
expected <-  data.frame(c(  0,    0,    0,    1,    1,    1,    2,    2,    2,    3,    3,    3,    3),
                        c(  0,    0,    0,    1,    0,    1,    2,    2,    0,    1,    2,    0,    3),
                        fix.empty.names = F)
all.equal(truncHoch.new(g1, g2, prev = NA, alpha, gamma), expected)

#trunchHoch with new cope function and only one primary endpoint
g1      <-        list('pe1' = c(  1, .001,    1, .001, .001, .001, .001, .001, .001, .001),
                       'se1' = c(  1, .001,    1, .001,    1, .001,    1, .001, .001, .001),
                       'se2' = c(  1, .001,    1, .001,    1, .001,    1,    1, .001,    1))
g2      <-        list('pe1' = c(  1,    1, .001, .001, .001, .001, .001, .001, .001, .001),
                       'se1' = c(  1,    1, .001, .001,    1,    1, .001, .001, .001, .001),
                       'se2' = c(  1,    1, .001, .001,    1,    1, .001,    1,    1, .001))
expected <-         data.frame(c(  0,    1,    2,    3,    3,    3,    3,    3,    3,    3),
                               c(  0,    1,    2,    3,    0,    1,    2,    3,    3,    3),
                               c(  0,    1,    2,    3,    0,    1,    2,    0,    1,    2),
                               fix.empty.names = F)
all.equal(truncHoch.new(g1, g2, prev = NA, alpha, gamma), expected)

