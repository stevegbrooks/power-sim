
navbarPage(title = "Power Simulation for Comparing Multiple Testing Methods",
           windowTitle = "Power Simulation for Comparing Multiple Testing Methods",
           #theme = "spacelab.css", 
           
           tabPanel("Readme", br(),
                    p(h3("This app helps to compare power for late phase trials under 
                          different approaches to multiple testing.")),
                    br(),
                    p(h4("Current limitations:"), 
                      h5("Only supports binary endpoints and continuous endpoints (equal variance assumed);"),
                      h5("Only supports comparison between the Truncated Hochberg and Graphical procedures;"),
                      h5("Must provide 3 groups, where the first is placebo, and the second and third are the treatment arms.")),
                    br(),
                    p(h4("Instructions:")),
                    p(h5("1). Define your scenarios (or use preloaded test scenarios). If you define your own, then you will need to create a CSV file (.csv):")),
                    p(h6("'Scenario' - enter your scenario names"),
                      h6("'Group' - where '1' is placebo, '2' is 2nd dose group, and '3' is third dose group"),
                      h6("'Endpoint' - where 'pe1' is the primary endpoint, 'se1' is the first secondary endpoint, 'se2' is the second secondary, etc..."),
                      h6("————————Note: it is also possible to specify co-primary endpoints. Simply label the first 'pe1' and the second 'pe2'.————————"),
                      h6("'Value' - enter the mean/success rate for each endpoint, each group, and each scenario."),
                      h6("(optional) 'Value2' - enter the standard deviation in the case of a continuous endpoint."),
                      h6("(optional) 'EndpointLabels' - enter the names of the endpoints tested using the same order of the 'Endpoint' column."),
                      h6("(optional) 'GroupLabels' - enter the names of the groups using the same order of the 'Group' column.")),
                    tags$a(img(src = "scenarioExample.png", width = "600px")),
                    p(h5("2). On the 'Application' page, upload your file (if you made one), adjust parameters, and then hit 'Run'.")),
                    p(h5("3). After the simulation is finished, adjust the graph options or filter the table to explore the results.")),
                    br(),
                    p(h4("Made by",
                         a("Biostatistics & Data Sciences",
                           href="https://mybi17.eu.boehringer.com/sites/functional/hp/medicine/corpmedicine/organization/Function/bds/Pages/bds.aspx", target="_blank"))),
                    p(h5("For technical issues, please contact steven.brooks@boehringer-ingelheim.com")),
                    tags$a(img(src = "logo.png", width = "400px"))
           ),
           
           tabPanel("Application",
                    
                    sidebarPanel(
                      h3("Scenarios:"),
                      selectInput("dataSource", "Use uploaded data or use test scenarios?", c("upload", "test")),
                      enter_file_ui("file", "Choose .csv file if 'upload' selected. See 'Readme' for instructions."),
                      h3("Group sample sizes:"),
                      fluidRow(column(3, enter_num_ui("nStart", "Start", 100)),
                               column(3, enter_num_ui("nEnd", "End", 100)),
                               column(3, enter_num_ui("nBy", "By", 10))
                      ),
                      h3("Sample size allocation"),
                      enter_text_vector_ui("nAlloc", 
                                           "Enter a comma delimited vector. Number of elements must be equal to 3.", 
                                           "1, 1, 1"),
                      h3("Truncated Hochberg fraction (gamma):"),
                      fluidRow(column(3, enter_num_ui("gammaStart", "Start", 0)),
                               column(3, enter_num_ui("gammaEnd", "End", 0.9)),
                               column(3, enter_num_ui("gammaBy", "By", .1))
                      ),
                      h3("Endpoint proportions:"),
                      enter_text_vector_ui("subgrp_prop", 
                                           paste("Enter a comma delimited vector.",
                                                 paste("Number of elements must be equal to number of endpoints.",
                                                       "If nothing is entered, the app will default to 1s.")), ""),
                      h3("Other parameters"),
                      enter_num_ui("rho", "Correlation between endpoints", 0.5),
                      enter_num_ui("alpha", "Global type I error (1-sided)", 0.025),
                      enter_num_ui("nsim", "Number of Simulations", 1000),
                      enter_num_ui("seed", "Seed", 73),
                      checkboxInput("gr_update", "Graphical testing with alpha passing", value = F),
                      br(),
                      actionButton("go_bi", "Run"),
                      br(), br()
                    ),
                    mainPanel(
                      uiOutput("groupOutcomeSelect"),
                      uiOutput("xAxisSelect"),
                      uiOutput("xAxisSlider"),
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      plotOutput("mainPlot"),
                      br(),
                      dataTableOutput('dataTable'),
                      uiOutput("downloadData")
                    )
           )
)

