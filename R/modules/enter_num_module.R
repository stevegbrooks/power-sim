enter_num_ui = function(id, head, ...) {
  # head: string, title of control element
  ns = NS(id)
  numericInput(ns("num"), head, ...)
}

enter_num = function(input, output, session) {
  reactive({
    input$num
  })
}

