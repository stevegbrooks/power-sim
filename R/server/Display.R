output$groupOutcomeSelect <- renderUI({
  req(nrow(simulator()) > 0)
  powerTable <- simulator()
  selectInput("groupOutcome", "Select outcome", levels(powerTable$groupOutcome))
})

output$xAxisSelect <- renderUI({
  selectInput("xAxis", "Select x-axis var", c("gamma", "sampleSize"))
})

output$xAxisSlider <- renderUI({
  req(nrow(simulator()) > 0)
  powerTable <- as.data.table(simulator())
  otherVar <- switch(req(input$xAxis),
                     "sampleSize" = "gamma",
                     "gamma" = "sampleSize")
  start <- range(powerTable[[otherVar]])[1]
  end <- range(powerTable[[otherVar]])[2]
  by <- max(diff(powerTable[[otherVar]]))
  sliderInput("slider", otherVar, 
              min = start, 
              max = end, 
              value = start, 
              step = by,
              animate = T)
})

output$mainPlot <- renderPlot({
  req(nrow(simulator()) > 0)
  req(input$slider)
  req(input$xAxis)
  req(input$groupOutcome)
  powerTable <- simulator()
  xAxisCol <- "sampleSize"
  if (input$xAxis == "sampleSize") {
    powerTable$gamma <- factor(powerTable$gamma) 
    xAxisCol <- "gamma"
  }
  powerTable <- powerTable[get(xAxisCol) == input$slider]
  if (req(is.null(powerTable)==F)) {
    ggplot(powerTable[groupOutcome == input$groupOutcome], 
           aes(x = get(input$xAxis),
               y = power,
               color = scenario,
               linetype = procedureType)) + 
      geom_line(size = .75) + 
      geom_hline(yintercept = .8) +
      facet_grid(~ endpoint) + 
      labs(x = input$xAxis, y = 'Power')
  }
})

output$dataTable = renderDataTable({
  req(nrow(simulator()) > 0)
  powerTable <- simulator()
  table1 <- dcast(powerTable,
                  sampleSize + endpoint + groupOutcome + gamma + scenario ~ procedureType,
                  value.var = "power")
  table1[, `:=`(sampleSize = factor(sampleSize),
                gamma = factor(gamma))]
  DT::datatable(table1, filter = 'top', options = list(pageLength = 10, autoWidth = T))
})

output$downloadData <- renderUI({
  req(nrow(simulator())>0)
  downloadButton("download")
})

output$download = downloadHandler(
  filename = function(){"Results.csv"}, 
  content = function(fname){
    write.csv(simulator(), fname, row.names = F)
  }
)