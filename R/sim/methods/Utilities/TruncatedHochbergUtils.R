library(dplyr)

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