server = function(input, output) {
        observeEvent(input$go_bi, {
                source(file.path(server_path, "ModuleCaller.R"), local = T)
                source(file.path(server_path, "Controller.R"), local = T)
        })
}
shinyServer(server)
