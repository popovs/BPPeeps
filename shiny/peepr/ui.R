#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  title = "Peep the peep data 👀",
  
  fluidRow(
    
    column(12,
      
      fluidRow(
        column(2,
               radioButtons("plot_type",
                    "Plot type",
                    c("scatter", "boxplot"))),
        column(width = 2,
               selectInput("custom_x",
                           "x-axis",
                           c("placeholder"))
        ), # end first column
        column(width = 2,
               selectInput("custom_y",
                           "y-axis",
                           c("placeholder"))
        ), # end second column
        column(width = 2,
               selectizeInput("custom_color_by",
                              "Color by",
                              c("placeholder"),
                              multiple = TRUE,
                              options = list(maxItems = 1)
               )
        ), # end third column
        column(width = 2,
               selectizeInput("facet_rows",
                              "Facet 1",
                              c("placeholder"),
                              multiple = TRUE,
                              options = list(maxItems = 1)
               )
        ), # end fourth column
        column(width = 2,
               selectizeInput("facet_cols",
                              "Facet 2",
                              c("placeholder"),
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
  )
))