#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    # selectedData ----
    selectedData <- reactive({
        if (input$daily_total_yn == "Daily total counts") {
            df <- counts[counts$in_daily_total_yn %in% c("total", "only total"),]
        } else {
            df <- counts[counts$in_daily_total_yn == "TRUE", ]
        }
    })

    # Reactive custom plot menu ----
    # When selectedData is filtered...
    observe({
        updateSelectInput(session, "custom_x", choices = names(selectedData()), selected = "date")
        updateSelectInput(session, "custom_y", choices = names(selectedData()), selected = "final_count")
        updateSelectInput(session, "custom_color_by", choices = names(selectedData()))
        updateSelectInput(session, "facet_rows", choices = names(selectedData()))
        updateSelectInput(session, "facet_cols", choices = names(selectedData()))
    })
    
    # ** Custom LM ----
    customLM <- reactive({
        lm(get(input$custom_y) ~ get(input$custom_x), data = selectedData())
    })
    
    output$custom_lm <- renderPrint(summary(customLM()))
    
    # Custom plot ----
    
    output$customplot <- renderPlot({
        p <- ggplot2::ggplot(selectedData(),
                             ggplot2::aes_string(input$custom_x,
                                                 input$custom_y,
                                                 color = input$custom_color_by)
        ) +
            ggplot2::labs(x = input$custom_x, y = input$custom_y) +
            ggplot2::theme_minimal()
        
        if (isTruthy(input$facet_rows)) {
            p <- p + ggplot2::facet_grid(rows = ggplot2::vars(get(input$facet_rows)),
                                         scales = "free") + # scales = "free" not working
                ggplot2::theme(strip.background = ggplot2::element_rect(fill="grey"))
        } 
        
        if (isTruthy(input$facet_cols)) {
            p <- p + ggplot2::facet_grid(cols = ggplot2::vars(get(input$facet_cols)),
                                         scales = "free") +
                ggplot2::theme(strip.background = ggplot2::element_rect(fill="grey"))
        } 
        
        if (isTruthy(input$facet_rows) & isTruthy(input$facet_cols)) {
            p <- p + ggplot2::facet_grid(rows = ggplot2::vars(get(input$facet_rows)),
                                         cols = ggplot2::vars(get(input$facet_cols)),
                                         scales = "free") +
                ggplot2::theme(strip.background = ggplot2::element_rect(fill="grey"))
        }
        
        if (input$plot_type == "scatter") {
            
            formula <- y ~ x
            
            p <- p + ggplot2::geom_point() + 
                ggplot2::stat_smooth(method = "lm") #+
                # ggpmisc::stat_poly_eq(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                #              label.x.npc = "right", label.y.npc = 0.15,
                #              formula = formula, parse = TRUE, size = 3
                # ) +
                # ggpmisc::stat_fit_glance(method = 'lm',
                #                 method.args = list(formula = formula),
                #                 geom = 'text',
                #                 aes(label = paste("p =", signif(..p.value.., digits = 4), sep = "")),
                #                 label.x.npc = 'right', label.y.npc = 0.2, size = 3)
            
        } else if (input$plot_type == "boxplot") {
            
            p <- p + 
                ggplot2::geom_boxplot() +
                ggplot2::geom_point(
                    color = "grey35",
                    size = 0.8,
                    position = ggplot2::position_jitter(0.2)
                )
            
        }
        
        p
    })
    
    output$custom_brushinfo <- renderPrint({
        brushedPoints(selectedData(),
                      input$custom_brush)
    })

})
