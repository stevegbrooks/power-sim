library(dplyr)

graphical.se1 <- function(g1, g2, primary, alpha0, alpha1) {
  g1 <- unlist(g1); g2 <- unlist(g2)
  pri_g1 <- if_else(primary == 1,
                    if_else(g1 > alpha1, 0, 1),
                    0)
  pri_g2 <- if_else(primary == 2,
                    if_else(g2 > alpha1, 0, 2),
                    0)
  pri_both <- if_else(primary == 3,
                      if_else(g1 > alpha0/2 & g2 > alpha0/2, 0,
                              if_else(g1 <= alpha0/2 & g2 > alpha0/2, 1,
                                      if_else(g1 > alpha0/2 & g2 <= alpha0/2, 2, 3))),
                      0)
  return(pri_g1 + pri_g2 + pri_both)
}

graphical.se2 <- function(g1, g2, primary, prev, alpha0, alpha1) {
  prev_g1 <- if_else(prev == 1,
                     if_else(primary == 3, 
                             if_else(g1[[1]] > alpha0/2, 0, 1),
                             if_else(g1[[1]] > alpha1, 0, 1)), 
                     0)
  prev_g2 <- if_else(prev == 2,
                     if_else(primary == 3, 
                             if_else(g2[[1]] > alpha0/2, 0, 2),
                             if_else(g2[[1]] > alpha1, 0, 2)), 
                     0)
  prev_both <- if_else(prev == 3 & primary == 3,
                       if_else(g1[[1]] > alpha0/2 & g2[[1]] > alpha0/2, 0, 
                               if_else(g1[[1]] <= alpha0/2 & g2[[1]] > alpha0/2, 1,
                                       if_else(g1[[1]] > alpha0/2 & g2[[1]] <= alpha0/2, 2, 3))), 
                       0)
  return(prev_g1 + prev_g2 + prev_both)
}

graphical.update.se1 <- function(g1, g2, toUpdate, alpha0) {
  g1 <- unlist(g1); g2 <- unlist(g2)
  out <- rep(-1, length(g1))
  out <- if_else(toUpdate == 0, 0, out)
  out <- if_else(toUpdate == 1, 
                 if_else(g1 > alpha0, 0, 1),
                 out)
  out <- if_else(toUpdate == 2,
                 if_else(g2 > alpha0, 0, 2),
                 out)
  out[out == -1] <- NA
  return(out)
}

graphical.update.se2 <- function(g1, g2, toUpdate, prev, alpha0) {
  out <- rep(-1, length(g1[[1]]))
  out <- if_else(prev == 0, 0, out)
  out <- if_else(prev == 1 & toUpdate == 1,
                 if_else(g1[[1]] > alpha0, 0, 1), 
                 out)
  out <- if_else(prev == 2 & toUpdate == 2,
                 if_else(g2[[1]] > alpha0, 0, 2),
                 out)
  out[out == -1] <- NA
  return(out)
}

graphical.update.recur <- function(g1, g2, toUpdate, prev, alpha0) {
  if (length(g1) == 1) {
    if (all(is.na(prev))) {
      stop("Only one secondary endpoint - no way to run graphical update.")
    } else {
      return(graphical.update.se2(g1, g2, toUpdate, prev, alpha0))
    }
  } else {
    if (all(is.na(prev))) {
      curr <- graphical.update.se1(g1[[1]], g2[[1]], toUpdate, alpha0)
    } else {
      curr <- graphical.update.se2(g1, g2, toUpdate, prev, alpha0)
    }
    return(data.frame(curr, Recall(g1 = g1[-1], 
                                   g2 = g2[-1], 
                                   toUpdate = toUpdate, 
                                   prev = curr, 
                                   alpha0 = alpha0),
                      fix.empty.names = F))
  }
}
