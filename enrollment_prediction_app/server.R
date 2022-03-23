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
shinyServer(function(input, output) {
  
  # Define the district colors
  # afuhsd_colors <- c(
  #   "Priorities" = "#B31942", 
  #   "Dreams" = "#0A3161", 
  #   "Barriers" = "#FFFFFF")
  
  
  
  
  # DataTable options ----
  dt_options <- list(
    dom = 'Bfrtip', 
    buttons = 
      c('csv', 'excel', 'pdf', 'print', 'colvis')
    # c('excel','pdf','print','colvis')
    # list('colvis', list(extend = "csv", text = "CSV", filename = "data",
    #                     exportOptions = list(
    #                         modifier = list(page = "all")
    #                     )))
    , 
    # initComplete = DT::JS("function(settings, json) {",
    #                       "$(this.api().table().header()).css({'background-color': '#369BE9', 'color': '#fff'});",
    #                       "}"), 
    scrollX = TRUE, 
    scrollY = '40vh', 
    pageLength = 12000 #,
    # scrollCollapse = TRUE
  ) # End of datatable options 
  
  # Import all data sources -----
  city_data <- read_csv("web/city_data_app.csv")
  enroll_data <- read_csv("web/enroll_data_app.csv")
  
  # Import the super saturday data -----
  # letters_vect <- rev(LETTERS[1:14])
  # names(letters_vect) <- rev(1:length(letters_vect))
  
  # super_saturday_feedback <- read_csv("web/sup_sat_topic_modeling_final.csv") %>% 
  #   replace_na(list(topic_name = "NA")) %>% 
  #   mutate(topic = paste("Topic", topic)) %>% 
  #   group_by(topic) %>% 
  #   mutate(
  #     n = 1, 
  #     tot_resp = sum(n, na.rm = T)
  #   ) %>% 
  #   ungroup() %>% 
  #   mutate(
  #     topic = factor(topic),
  #     topic = fct_reorder(topic, tot_resp, max)) %>% 
  #   mutate(
  #     # topic_name = paste(
  #     #   "Topic", 
  #     #   letters_vect[as.character(as.numeric(topic)+1)]
  #     #        )
  #     numeric_topic = as.numeric(topic),
  #     topic_name = paste("Topic", letters_vect[numeric_topic])
  #   )
  
  city_choices <- sort(unique(city_data$name))
  lea_choices <- sort(unique(enroll_data$lea_abbr))
  
  output$city_choices <- renderUI({
    # checkboxGroupInput(
    #   inputId = "topics",
    #   label = "Select Topics:",
    #   choices = city_choices,
    #   selected = city_choices
    # )
    pickerInput(
      inputId = "cities",
      label = "Select Cities:",
      selected = c("Avondale", "Buckeye", "Goodyear", "Litchfield Park"),
      choices = city_choices,
      options = list(
        `actions-box` = TRUE, 
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
  output$lea_choices <- renderUI({
    # checkboxGroupInput(
    #   inputId = "topics",
    #   label = "Select Topics:",
    #   choices = city_choices,
    #   selected = city_choices
    # )
    pickerInput(
      inputId = "districts",
      label = "Select School District:",
      selected = "Agua Fria UHSD",
      choices = lea_choices,
      options = list(
        `actions-box` = TRUE,
        liveSearch = TRUE,
        # liveSearchPlaceholder = "Type LEA Name",
        # mobile = TRUE,
        size = 10,
        `selected-text-format` = "count > 3"
      ), 
      multiple = TRUE
    )
  })
  
 
  
  # Create a reactive data frame for linear regression ----
  ml_df <- reactive({
    
    filtered_enrollment <- enroll_data %>% 
      filter(
        lea_abbr %in% input$districts, 
        grade == "total"
      ) %>% 
      group_by(fiscal_yr, pop_year) %>% 
      summarise(tot_enrollment = sum(students, na.rm = T)) #%>% 
    # left_join()
    
    filtered_population <- city_data %>% 
      filter(
        name %in% input$cities
      ) %>% 
      group_by(Year) %>% 
      summarise(tot_pop = sum(Population, na.rm = T)) %>% 
      left_join(filtered_enrollment, by = c("Year" = "pop_year"))
    
    return(filtered_population)
    
  }) # End of reactive dataframe for linear regression

  # Text of selected LEAs
  output$lea_text <- renderText(glue_collapse(input$districts, sep = "; ")) 
  
  # Text of selected cities
  output$cities_text <- renderText(
    paste(
      "based on population of ",
      glue_collapse(input$cities, sep = "; ")
      )
    )
  
  # Output valuebox with estimated enrollment
  # output$enrollment_value <- renderValueBox({
  #   valueBox(
  #     subtitle = "Estimated Enrollment ", 
  #     # input$count,
  #     value = 43,
  #     icon = icon("credit-card")
  #   )
  # })
  
  lm_model <- reactive({
    
    train <- ml_df() 
    
    # Find the latest year of enrollment data (model will predict the following year)
    prediction_yr <- max(train$fiscal_yr, na.rm = T) + 1
    
    model <- lm(tot_enrollment ~ tot_pop, train[complete.cases(train), ])
    
    return(model)
  })
  
  
  predicted_enrollment <- reactive({
    
    train <- ml_df()
    
    # Find the latest year of enrollment data (model will predict the following year)
    prediction_yr <- max(train$fiscal_yr, na.rm = T) + 1
    # 
    test <- dplyr::filter(train, Year == prediction_yr -1)
    # 
    # model <- lm(tot_enrollment ~ tot_pop, train[complete.cases(train), ])
    
    enrollment_prediction <- predict(lm_model(), newdata = test)
    
    return(enrollment_prediction)
    
  })
  
  # Value box definition -----
  output$enrollment_value <- renderValueBox({
    
    if(all(is.na(input$districts))|all(is.na(input$cities))){
      valueBox(
        # subtitle = glue("FY {prediction_yr} Estimated Enrollment "), 
        subtitle = "Select Cities, LEA to estimate", 
        value = "____",
        icon = icon("exclamation-circle"), 
        color = "red"
      )
    } else {
      valueBox(
        # subtitle = glue("FY {prediction_yr} Estimated Enrollment "), 
        subtitle = glue("Estimated Enrollment "), 
        value = as.character(round(predicted_enrollment(), 0)),
        icon = icon("users"), 
        color = "green"
      )
    }
    
  }) # End of value box definition
  
  # Create a dataframe based on the model
  fit <- reactive({
    fit_df <- augment(lm_model())
  })
  
  # Create plot of regression trend
  output$regression_plot <- renderHighchart({
    
    fit2 <- fit()
    
    fit2 %>% 
      # fit() %>% 
      #       left_join(ml_df(), by = c("tot_pop", "tot_enrollment")) %>%
      hchart('scatter', hcaes(x = tot_pop, y = tot_enrollment)) %>%
      hc_add_series(
        fit2, 
        type = "line", hcaes(x = tot_pop, y = .fitted),
        name = "Fit", id = "fit"
      )
    
  })
  
  output$chart1 <- renderHighchart({
    
    highcharts_demo()
    
  })
  
  
  # Output the data to visualize during testing
  # If this is useful, it can be a separate tab for exporting
  output$table_of_data <- renderDT({
    
    data_for_table <- ml_df()
    # data_for_table <- fit()
    
    enroll_pop_dt <- data_for_table %>% 
      datatable(rownames = FALSE, extensions = c('Buttons'), options = dt_options, filter = 'top')
    
  })
  
  # Create a table to display the underlying data for the linear regression model
  output$ml_data_dt <- renderDT({
    
    # ml_df_dt <- ml_df() %>% 
    ml_df_dt <- fit() %>% 
      left_join(ml_df(), by = c("tot_pop", "tot_enrollment")) %>% 
      select(Year, fiscal_yr, everything()) %>%
      datatable(rownames = FALSE, extensions = c('Buttons'), options = dt_options, filter = 'top')
    
  })
  
  
  output$topics_plot <- renderPlotly({
    
    filtered_responsesDF <- filtered_responses()
    
    # Plotly counts by category ----
    sup_sat_cat_count <- count(
      filtered_responsesDF, 
      area, topic_name,
      sort = TRUE
    ) %>% 
      group_by(topic_name) %>% 
      mutate(tot_resp = sum(n, na.rm = T)) %>% 
      ungroup() %>% 
      # filter(!is.na(topic_name)) %>% 
      # filter(topic_description %in% input$topics) %>%
      mutate(
        area = factor(
          area, 
          ordered = TRUE, 
          levels = c("Priorities", "Dreams", "Barriers")),
        # topic_description = str_wrap(topic_description, width = 10), 
        # topic_name = factor(topic_name, ordered = TRUE),
        # topic_name = fct_reorder(topic_name, tot_resp, max)
        # numeric_topic = as.numeric(topic), 
        # topic_name = letters_vect[numeric_topic]
      ) %>% 
      ggplot(aes(x = topic_name, y = n, fill = area, label = n)) +
      # theme_dark() +
      geom_bar(position = "dodge", stat = "identity", color = "gray50") +
      # geom_text_repel() +
      geom_text(
        size = 3, 
        position = position_dodge(width = .9), 
        aes(y = n + 5)
      ) +
      # facet_wrap(~area, nrow = 1) +
      scale_fill_manual(values = afuhsd_colors)+
      theme(legend.position = "bottom") +
      ylim(0, 175) +
      theme(
        axis.text.x = element_text(
          angle = 90, vjust = 0.5, hjust=1)
      ) +
      labs(
        title = "Super Saturday Feedback", 
        x = "Topic Description", 
        y = "# of Related Responses"
      )
    
    ggplotly(sup_sat_cat_count)
    
  }) # End of topics plotly
  
  # Create a word cloud of Super Saturday terms ----
  output$sup_sat_cloud <- renderWordcloud2({
    sup_sat_DT_wcloud <- filtered_responses()
    # prep_for_wc <- super_saturday_feedback %>% 
    #   filter(topic_description %in% input$topics)
    
    sup_sat_DT_wcloud_clean <- sup_sat_DT_wcloud %>% 
      select(sentence) %>% 
      # filter(str_detect(source, "Super Sat")) %>% 
      unnest_tokens(word, sentence) %>% 
      anti_join(custom_stop_words)
    
    sup_sat_DT_wcloud_clean$stemmed <- stem_hunspell(sup_sat_DT_wcloud_clean$word)
    
    # Extract the responses column
    word_cloud_col <- sup_sat_DT_wcloud_clean$stemmed
    # word_cloud_col <- prep_for_wc$sentence
    # Create a word cloud object
    create_wordcloud(word_cloud_col)
    
  })
  
  # Create data table of Super Saturday feedback ----
  output$sup_sat_dt <- renderDataTable({
    
    sup_sat_DT <- filtered_responses()
    
    dt_for_sup_sat <- sup_sat_DT %>% 
      select(area, sentence, topic_name) %>% 
      mutate(
        area = factor(
          area, 
          ordered = T, 
          levels = c("Dreams", "Priorities", "Barriers")
        ), 
        topic_name = factor(topic_name)
      ) %>% 
      arrange(topic_name, area) %>% 
      rename(
        "Area" = "area", 
        "Response" = "sentence", 
        "Topic" = "topic_name"
      ) %>% 
      datatable(rownames = FALSE, extensions = c('Buttons'), options = dt_options, filter = 'top')
    
    
  }) # End of Super Saturday data table
  
  # Create a data table of all sources ----
  output$all_sources_dt <- renderDataTable({
    
    names(city_pops) <- toTitleCase(names(city_pops))
    
    city_pops %>% 
      select(Source, Area, Response) %>% 
      mutate(
        Area = factor(
          Area, 
          ordered = T, 
          levels = c("Dreams", "Priorities", "Barriers")), 
        Source = factor(Source)
      ) %>% 
      # arrange(Source, Area) %>% 
      datatable(rownames = FALSE, extensions = c('Buttons'), options = dt_options, filter = 'top')
    
  })
  
}) # End of server function ----
