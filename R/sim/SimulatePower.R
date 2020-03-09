library(assertthat)
library(data.table)

calcPower <- function(simResults) {
  setDT(simResults)
  g1 <- simResults[, lapply(.SD, function(x) sum(x %in% c(1,3))/.N), keyby = procedureType]
  g2 <- simResults[, lapply(.SD, function(x) sum(x %in% c(2,3))/.N), keyby = procedureType]
  atLeastOne <- simResults[, lapply(.SD, function(x) sum(x != 0)/.N), keyby = procedureType]
  both <- simResults[, lapply(.SD, function(x) sum(x == 3)/.N), keyby = procedureType]
  out <- rbind(g1, g2, atLeastOne, both)
  out[, groupOutcome := c(rep("g1", 2), rep("g2", 2), rep("atLeastOne", 2), rep("both", 2))]
  measureVars <- setdiff(names(out), c("procedureType", "groupOutcome"))
  out <- melt(out, 
              id.vars = c("procedureType", "groupOutcome"), 
              measure.vars = measureVars, 
              variable.name = "endpoint", 
              value.name = "power")
  return(out)
}

simPower <- function(pValues, c, rho, alpha, gamma, nsim, grUpdate = T) {
  doseGroup1 <- pValues[[1]]
  doseGroup2 <- pValues[[2]]
  doseGroup1_se <- doseGroup1[-which(grepl("pe", names(doseGroup1)))]
  doseGroup2_se <- doseGroup2[-which(grepl("pe", names(doseGroup2)))]
  #Run TruncHoch
  results.th <- truncHoch(g1 = doseGroup1, 
                          g2 = doseGroup2, 
                          prev = NA, 
                          alpha = alpha, 
                          gamma = gamma)
  #Run Graphical
  results.gr <- graphical(g1 = doseGroup1_se,
                          g2 = doseGroup2_se,
                          primary = results.th[[1]],
                          prev = NA,
                          alpha0 = alpha,
                          alpha1 = (alpha * (1-gamma))/2)
  results.gr <- cbind(results.th[[1]], results.gr)
  if (grUpdate & length(doseGroup1_se) > 1) {
    results.gr <- graphical.update(g1 = doseGroup1_se,
                                   g2 = doseGroup2_se,
                                   results.gr = results.gr,
                                   alpha0 = alpha)
  }
  #test that results obey hierarchy
  names(results.th) <- paste0("e", seq(1:(dim(results.th)[2])))
  names(results.gr) <- names(results.th)
  testResult <- all(unlist(lapply(seq(seq(dim(results.th)[2])), 
                                  function(x) 
                                    if (x != 1) 
                                      all(results.gr[x] <= results.gr[x - 1] 
                                          & results.th[x] <= results.th[x - 1]))))
  assert_that(testResult == T,
              msg = "Not all secondary outcomes have values <= the previous endpoint(s).")
  #output
  results <- rbind(results.gr, results.th)

  results$procedureType <- c(rep("graphical", nsim), rep("truncHoch", nsim))
  return(calcPower(results))
}