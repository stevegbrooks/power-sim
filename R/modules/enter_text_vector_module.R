enter_text_vector_ui = function(id, head, ...) {
  ns = NS(id)
  textInput(ns("text"), head, ...)
}

enter_text_vector = function(input, output, session) {
  reactive({
    as.numeric(unlist(strsplit(input$text, ",")))
  })
}
