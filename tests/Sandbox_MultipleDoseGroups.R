rm(list = ls())
library(dplyr)
library(data.table)


#global inputs
alpha <- 0.025
gamma <- 0.5


truncHoch.pe <- function(pValues, alpha, gamma) {
  
  primary <- as.data.frame(lapply(pValues, function(x) x[1]))
  
  indicator <- apply(primary, 1, which.min)
  
  all <- if_else(apply(primary, 1, max) <= (1 + gamma) * alpha/2, 1, 0)
  
  single <- if_else(apply(primary, 1, max) > (1 + gamma) * alpha/2 
                    & apply(primary, 1, min) <= alpha/2, 1, 0)
  
  out <- rep(0, dim(primary)[1])
  out[all == 1] <- (1 + dim(primary)[2])
  out[single == 1] <- indicator[single == 1]
  return(out)
}

#two groups
#truncHoch.pe
g1      <-        list(c(  1, .001,    1, .001, .001, .001, .001, .001, .001, .001),
                       c(  1, .001,    1, .001,    1, .001,    1, .001, .001, .001),
                       c(  1, .001,    1, .001,    1, .001,    1,    1, .001,    1))
g2      <-        list(c(  1,    1, .001, .001, .001, .001, .001, .001, .001, .001),
                       c(  1,    1, .001, .001,    1,    1, .001, .001, .001, .001),
                       c(  1,    1, .001, .001,    1,    1, .001,    1,    1, .001))
pValues <- list(g1, g2)
expected <-            c(  0,    1,    2,    3,    3,    3,    3,    3,    3,    3)

all.equal(truncHoch.pe(pValues = lapply(pValues, function(x) x[1]), 
                       alpha = alpha, 
                       gamma = gamma), 
          expected)
#one group
#truncHoch.pe
g1      <-        list(c(  1, .001,    1, .001, .001, .001, .001, .001, .001, .001),
                       c(  1, .001,    1, .001,    1, .001,    1, .001, .001, .001),
                       c(  1, .001,    1, .001,    1, .001,    1,    1, .001,    1))
pValues <- list(g1)
expected <-            c(  0,    2,    0,    2,    2,    2,    2,    2,    2,    2)

all.equal(truncHoch.pe(pValues = lapply(pValues, function(x) x[1]), 
                       alpha = alpha, 
                       gamma = gamma), 
          expected)
#three groups
#truncHoch.pe
g1      <-        list(c(  1, .001,    1,    1, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001,    1),
                       c(  1, .001,    1,    1, .001,    1, .001,    1,    1, .001,    1,    1,    1,    1,    1, .001, .001, .001,    1, .001, .001,    1),
                       c(  1, .001,    1,    1, .001,    1,    1,    1,    1, .001,    1,    1, .001,    1,    1,    1,    1,    1,    1, .001, .001,    1))
g2      <-        list(c(  1,    1, .001,    1, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001,    1, .001),
                       c(  1,    1, .001,    1, .001,    1,    1, .001,    1,    1, .001,    1,    1,    1,    1, .001, .001,    1, .001, .001,    1, .001),
                       c(  1,    1, .001,    1, .001,    1,    1,    1,    1,    1, .001,    1,    1, .001,    1,    1,    1,    1,    1, .001,    1, .001))
g3      <-        list(c(  1,    1,    1, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001, .001,    1, .001, .001),
                       c(  1,    1,    1, .001, .001,    1,    1,    1, .001,    1,    1, .001,    1,    1,    1, .001,    1, .001, .001,    1, .001, .001),
                       c(  1,    1,    1, .001, .001,    1,    1,    1,    1,    1,    1, .001,    1,    1, .001,    1,    1,    1,    1,    1, .001, .001))
pValues <- list(g1, g2, g3)
expected <- data.frame(c(  0,    1,    2,    3,    4,    4,    4,    4,    4,    4,    4,    4,    4,    4,    4,    4,    4,    4,    4,    ),
                       c(  0,    1,    2,    3,    0,    1,    2,    3,    3,    3),
                       c(  0,    1,    2,    3,    0,    1,    2,    0,    1,    2))

all.equal(truncHoch.pe(pValues = lapply(pValues, function(x) x[1]), 
                       alpha = alpha, 
                       gamma = gamma), 
          expected)


expected <- data.frame(c(  0,    1,    2,    3,    3,    3,    3,    3,    3,    3),
                       c(  0,    1,    2,    3,    0,    1,    2,    3,    3,    3),
                       c(  0,    1,    2,    3,    0,    1,    2,    0,    1,    2))


truncHoch.se <- function(g1, g2, prev, alpha, gamma, last = F) {
  if (last) 
    out <- truncHoch.pe(g1[[1]], g2[[1]], alpha, 1)
  else
    out <- truncHoch.pe(g1[[1]], g2[[1]], alpha, gamma)
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

truncHoch <- function(pValues, prev, alpha, gamma) {
  if (length(pValues[[1]]) == 1) {
    return(truncHoch.se(pValues = pValues, 
                        prev = prev, 
                        alpha = alpha, 
                        gamma = gamma, last = T))
  } else {
    if (all(is.na(prev))) {
      curr <- truncHoch.pe(pValues = lapply(pValues, function(x) x[1]), 
                           alpha = alpha, 
                           gamma = gamma)
    } else {
      curr <- truncHoch.se(pValues = pValues, 
                           prev = prev, 
                           alpha = alpha, 
                           gamma = gamma, 
                           last = F)
    }
    return(data.frame(curr, Recall(pValues = lapply(pValues, function(x) x[-1]), 
                                   prev = curr, 
                                   alpha = alpha, 
                                   gamma = gamma), 
                      fix.empty.names = F))
  }
}

#truncHoch
g1      <-        list(c(  1, .001,    1, .001, .001, .001, .001, .001, .001, .001),
                       c(  1, .001,    1, .001,    1, .001,    1, .001, .001, .001),
                       c(  1, .001,    1, .001,    1, .001,    1,    1, .001,    1))

g2      <-        list(c(  1,    1, .001, .001, .001, .001, .001, .001, .001, .001),
                       c(  1,    1, .001, .001,    1,    1, .001, .001, .001, .001),
                       c(  1,    1, .001, .001,    1,    1, .001,    1,    1, .001))

pValues <- list(g1, g2)

expected <- data.frame(c(  0,    1,    2,    3,    3,    3,    3,    3,    3,    3),
                       c(  0,    1,    2,    3,    0,    1,    2,    3,    3,    3),
                       c(  0,    1,    2,    3,    0,    1,    2,    0,    1,    2))

all.equal(truncHoch(pValues, prev = NA, alpha, gamma), expected)

