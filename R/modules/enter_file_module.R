#File reader 
# Module UI function
enter_file_ui <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), label)
  )
}

# Module server function
enter_file <- function(input, output, session) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = "Please upload a scenario settings file."))
    input$file
  })
  # The user's data, parsed into a datatable
  data <- reactive({
    data.table::fread(userFile()$datapath, header = T, fill = T)
  })
  observe({
    msg <- sprintf("File %s was uploaded", userFile()$name)
    cat(msg, "\n")
  })
  # Return the reactive that yields the data
  return(data)
}
