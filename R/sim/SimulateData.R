library(copula)
library(gsDesign)

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
  output <- lapply(seq(sim[2:length(sim)]), function(x) {
    lapply(seq(sim[[x + 1]]), function(y) {
      if (all(is.integer(sim[[x + 1]][[y]]))) {      
        testBinomial(sim[[x + 1]][[y]], 
                     g1[[y]], 
                     floor(c[[y]] * groupParams[1, x + 1]), 
                     floor(c[[y]] * groupParams[1, 1]),
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
  return(output)
}

