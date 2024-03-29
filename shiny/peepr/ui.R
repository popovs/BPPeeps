ui <- shinyUI(fluidPage(
  
  # Application title
  title = "Peep the peep data 👀",
  
  # Add a sidebar
  sidebarLayout(
    sidebarPanel(
      width = 2,
      h2("PeepR:"),
      h4("Explore and model peeps data"),
      "Note that if you filter the data, you will need to re-run the model to use the updated data.",
      
      # Add a checkbox to filter out records that say "Canoe Pass" & "Brunswick Point" in the "station_n" column
      checkboxInput("canoe_pass", "Filter out 'Canoe Pass'", TRUE),
      checkboxInput("brunswick_point", "Filter out 'Brunswick Point'", TRUE),
      
      # Add a slider to filter the data by "year" range
      sliderInput("year_range", "Year range:",
                  min = min(data$year), max = max(data$year),
                  value = c(min(data$year), max(data$year)),
                  step = 1),
      
      # Add a checkbox group to let the user select the stations that should be "N"
      # in the `n_s` column
      selectizeInput("selected_stations", "Select Northern stations:",
                  choices = unique(data$station_n),
                  selected = unique(data[["station_n"]][data$n_s == "N"]), # Defaults to whatever is already labelled as N in base dataset
                  multiple = TRUE,
                  options = list(maxOptions = 6)
                  ),
      
      # Add a checkbox to aggregate data by N/S
      checkboxInput("aggregate_ns", "Aggregate data by N/S station", TRUE),
      
      ), # Close sidebar panel
    
    # Add tabs in the main panel
    mainPanel(
      tabsetPanel(
        
        # First tab
        tabPanel("Model",
                 # Add a text input for the user to type in a linear model
                 textAreaInput("model", 
                               "Linear model:", 
                               value = "log_wesa ~ n_s + dos + I(dos^2) + year + scale(flow) + n_s*scale(flow) + (dos + I(dos^2) | year)",
                               width = "80%"),
                 # Add a button to run the model
                 actionButton("run", "Run model"),
                 checkboxInput("stepwise", "Use backwards stepwise selection"),
                 verbatimTextOutput("model_summary"),
                 h4("Model data structure:"),
                 verbatimTextOutput("data_str")
        ),
        
        # Second tab
        tabPanel("Diagnostics",
                 # Add plots for model diagnostic
                 h2("Observed vs. Fitted"),
                 plotOutput("observed_vs_predicted"),
                 h2("Fitted vs. Residuals"),
                 plotOutput("residuals_vs_fitted"),
                 h2("Quantile-Quantile"),
                 plotOutput("qq_plot"),
                 h2("Residuals frequency"),
                 plotOutput("residuals_hist")
        ),
        
        # Third tab
        tabPanel("Plots",
                 h4("Here you can make exploratory plots of the same data used in the model."),
                 fluidRow(
                   
                   column(12,
                          
                          fluidRow(
                            column(2,
                                   radioButtons("plot_type",
                                                "Plot type",
                                                c("scatter", "boxplot"))),
                            column(width = 2,
                                   #uiOutput('custom_x')
                                   selectInput("custom_x",
                                               "x-axis",
                                               c(names(data)),
                                               selected = "year")
                            ), # end first column
                            column(width = 2,
                                   #uiOutput('custom_y')
                                   selectInput("custom_y",
                                               "y-axis",
                                               c(names(data)),
                                               selected = "wesa_count")
                            ), # end second column
                            column(width = 2,
                                   #uiOutput('custom_color_by')
                                   selectizeInput("custom_color_by",
                                                  "Color by",
                                                  c(names(data)),
                                                  selected = "n_s",
                                                  multiple = TRUE,
                                                  options = list(maxItems = 1)
                                   )
                            ), # end third column
                            column(width = 2,
                                   #uiOutput('facet_rows')
                                   selectizeInput("facet_rows",
                                                  "Facet 1",
                                                  c(names(data)),
                                                  multiple = TRUE,
                                                  options = list(maxItems = 1)
                                   )
                            ), # end fourth column
                            column(width = 2,
                                   #uiOutput('facet_cols')
                                   selectizeInput("facet_cols",
                                                  "Facet 2",
                                                  c(names(data)),
                                                  multiple = TRUE,
                                                  options = list(maxItems = 1)
                                   )
                            ) # end fifth column
                          ), # close fluidRow
                          
                          fluidRow(column(12, 
                                          plotOutput("customplot",
                                                     brush = brushOpts(id = "custom_brush")
                                          )
                          )),
                          fluidRow(verbatimTextOutput("custom_brushinfo")),
                          fluidRow(verbatimTextOutput("custom_lm")),
                   )
                 )), # Close plots
        
        # Fourth tab
        tabPanel("Data table",
                 # Add a table to show the data
                 DT::dataTableOutput("data_table")
        ) # Close fourth tabPanel
      ) # Close tabsetPanel
    ) # Close mainPanel
  )
))
