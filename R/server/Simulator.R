simulator = reactive({
  
  withProgress(message = 'Running simulation', value = 0, {
    
    resultsTable <- data.frame()
    equal.n <- rep(nStart, length(nAlloc))
    n <- equal.n * nAlloc
    
    for (i in 1:length(seq(nStart, nEnd, nBy))) {
      
      print(paste0("#### Processing sample size ", n[[1]], " ####"))
      scenarios_x <- lapply(scenarios, function(x) rbind(n, x))
      results_scenario <- data.frame()
      for (j in seq(scenarios_x)) {
        
        print(paste0("Processing scenario ", names(scenarios)[j]))
        incProgress(1/(length(scenarios_x) * length(seq(nStart, nEnd, nBy))))
        
        results_gamma <- data.frame()
        for (k in seq(gamma)) {
          sim <- simCorrData(groupParams = scenarios_x[[j]], 
                             c = subgrp_prop, 
                             rho = rho, 
                             nsim = nsim)
          
          testStats <- genTestStats(groupParams = scenarios_x[[j]], 
                                    sim = sim, 
                                    c = subgrp_prop)
          
          pValues <- lapply(testStats, function(x) lapply(x, pnorm, lower.tail = F))
          pValues <- lapply(pValues, setNames, nm = endpointLabels_default)
          #simulate power based on above
          results <- simPower(pValues = pValues,
                              c = subgrp_prop,
                              rho = rho,
                              alpha = alpha,
                              gamma = gamma[k],
                              nsim = nsim,
                              grUpdate = grUpdate)
          
          results$gamma <- gamma[k]
          results_gamma <- rbind(results_gamma, results)
        }
        results_gamma$scenario <- names(scenarios)[j]
        results_scenario <- rbind(results_scenario, results_gamma)
      }
      results_scenario$sampleSize <- n[[1]]
      n <- n + nBy
      resultsTable <- rbind(resultsTable, results_scenario)
    }
    setDT(resultsTable)
    resultsTable[, scenario := factor(scenario)]
    resultsTable[, procedureType := factor(procedureType)]
    resultsTable[, endpoint := factor(endpoint, labels = endpointLabels)]
    resultsTable[, groupOutcome := factor(groupOutcome, labels = groupOutcomeLabels)]
    print("Saving ResultsAndScenarios.RDS to the 'markdown_data' folder for RMarkdown display...")
    print("Use the markdown template 'RMarkdownReportBuilder.Rmd' to build a new report.")
    saveRDS(list(resultsTable, scenarios_x), "markdown_data/ResultsAndScenarios.RDS")
    return(resultsTable)
  })
})

