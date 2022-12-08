server <- shinyServer(function(input, output, session) {
  
  # Load packages
  library(ggplot2) # for plots
  library(magrittr) # for %>% and %<>%
  
  # 01 Data filter (sidebar) ----
  # Define a reactive expression to filter out records with "Canoe Pass" in the "station_n" column
  data_filtered <- reactive({
    
    # Set dates, factors etc.
    data$survey_date <- as.Date(data$survey_date)
    station_levels <- c("Canoe Pass", "Brunswick dike", "Brunswick Point", "View corner", "Pilings", "Bend", "34th St pullout", "Coal Port")
    data$station_n <- factor(data$station_n, levels = station_levels)
    data$station_s <- factor(data$station_s, levels = station_levels)
    data$n_s <- factor(data$n_s, levels = c("N", "S"))
    
    # Now filter
    if (input$canoe_pass) {
      data_filtered <- data[data$station_n != "Canoe Pass", ]
    } else {
      data_filtered <- data
    }
    
    # Filter the data by "year" range
    data_filtered <- dplyr::filter(data_filtered, year >= input$year_range[1], year <= input$year_range[2])
    
    # Update the `n_s` column based on the selected stations
    data_filtered <- dplyr::mutate(data_filtered, 
                                   n_s = ifelse(station_n %in% input$selected_stations,
                                                "N", "S"))
    
    # Group data by selected N/S
    data_filtered <- sqldf::sqldf("select year, 
                                  survey_date, 
                                  julian_day, 
                                  min(start_time) as start_time, 
                                  n_s, 
                                  sum(final_count) as final_count, 
                                  sum(wesa_count) as wesa_count, 
                                  sum(dunl_count) as dunl_count, 
                                  p_wesa, 
                                  predicted_p_wesa, 
                                  avg(raptor_count) as raptor_count, 
                                  elev_min, 
                                  elev_max, 
                                  elev_median, 
                                  elev_mean, 
                                  elev_range, 
                                  flow, 
                                  total_precip, 
                                  mean_temp, 
                                  u, 
                                  v, 
                                  windspd, 
                                  wind_deg 
                                  from data_filtered 
                                  group by survey_date, n_s;") %>%
      dplyr::mutate(dos = scale(julian_day),
                    log_wesa = log(wesa_count + 1),
                    log_dunl = log(dunl_count + 1)) %>%
      dplyr::select(year, survey_date, julian_day, dos, start_time, n_s, 
                    final_count, wesa_count, dunl_count, log_wesa, log_dunl,
                    dplyr::everything())
    
    # Return filtered data
    data_filtered
  })
  
  output$data_str <- renderPrint({ str(data_filtered() )})
  
  # 02 Model tab ----
  # Define a reactive expression to run the linear model when the user clicks on the "Run model" button
  # First, define stepwise result, if stepwise is chosen
  s <- eventReactive(input$run, {
    if (input$stepwise) {
      # Fit the model using stepwise selection
      lmerTest::step(lmerTest::lmer(formula(input$model), data = data_filtered()))
      }
    })
  # Then, define the model
  fit <- eventReactive(input$run, {
    if(input$stepwise) {
      # Use the result of stepwise selection to fit the model
      model <- lme4::lmer(lmerTest::get_model(s())@call$formula, data = data_filtered())
    } else {
      # Use the lmer() function from the lme4 package to run the linear model
      model <- lme4::lmer(input$model, data = data_filtered())
    }
  })
  
  # Define an output to show the model summary
  output$model_summary <- renderPrint({
    if (input$stepwise) {
      print(summary(fit()))
      writeLines("\n##########################\nSTEPWISE SELECTION SUMMARY\n##########################\n")
      print(s())
    } else {
      summary(fit())
    }
  })
  
  # 03 Diagnostic plots tab ----
  # Define reactive model response variable
  response <- reactive({
    sub("\\s\\~.*", "", input$model)
  })
  
  data_plot <- eventReactive(input$run, {
    data_plot <- broom.mixed::augment(fit(), data_filtered())
    data_plot <- janitor::clean_names(data_plot)
  })

  # Define outputs for the model diagnostic plots
  output$observed_vs_predicted <- renderPlot({
    ggplot(data_plot(), 
           aes_string(x = response(),
                      y = "fitted"
                      )) +
      geom_point() + 
      #ggtitle("Observed vs. Predicted") +
      xlab(paste0("Observed (", response(), ")")) +
      ylab("Predicted") +
      theme_minimal()
  })
  
  output$residuals_vs_fitted <- renderPlot({
    ggplot(data_plot(), 
           aes_string(y = "resid",
                      x = "fitted"
                      )) +
      geom_point() +
      #ggtitle("Fitted vs. Residuals") +
      ylab("Residuals") +
      xlab("Fitted") +
      theme_minimal()
  })
  
  output$qq_plot <- renderPlot({
    ggplot(data_plot(), aes_string(sample = "resid")) +
      stat_qq() + 
      stat_qq_line() +
      geom_hline(yintercept = 0,
                 linetype = "dashed") +
      #ggtitle("Quantile-Quantile") +
      xlab("Theoretical") + 
      ylab("Sample") +
      theme_minimal()
  })
  
  # 04 Custom plots tab ----
  # When data_filtered is filtered...
  observe({
    updateSelectInput(session, "custom_x", choices = names(data_filtered()), selected = "flow")
    updateSelectInput(session, "custom_y", choices = names(data_filtered()), selected = "final_count")
    updateSelectInput(session, "custom_color_by", choices = names(data_filtered()), selected = "station_n")
    updateSelectInput(session, "facet_rows", choices = names(data_filtered()))
    updateSelectInput(session, "facet_cols", choices = names(data_filtered()), selected = "station_n")
  })
  
  # ** Custom LM ----
  customLM <- reactive({
    lm(get(input$custom_y) ~ get(input$custom_x), data = data_filtered())
  })
  
  output$custom_lm <- renderPrint(summary(customLM()))
  
  # Custom plot ----
  
  output$customplot <- renderPlot({
    p <- ggplot2::ggplot(data_filtered(),
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
        ggplot2::stat_smooth(method = "lm")
      
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
    brushedPoints(data_filtered(),
                  input$custom_brush)
  })
  
  # 05 Data table tab ----
  # Define an output for the data table
  output$data_table <- DT::renderDataTable({
    DT::datatable(data_filtered(), 
                  rownames = FALSE,
                  filter = "top", 
                  options = list(
                    scrollX = TRUE,
                    scrollY = TRUE)
                  )
  })
})
