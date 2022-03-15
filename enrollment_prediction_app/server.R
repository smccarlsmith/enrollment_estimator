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
  afuhsd_colors <- c(
    "Priorities" = "#B31942", 
    "Dreams" = "#0A3161", 
    "Barriers" = "#FFFFFF")
  
  
  
  
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
  city_pops <- read_csv("web/cities_population.csv")
  
  # Import the super saturday data -----
  letters_vect <- rev(LETTERS[1:14])
  # names(letters_vect) <- rev(1:length(letters_vect))
  
  super_saturday_feedback <- read_csv("web/sup_sat_topic_modeling_final.csv") %>% 
    replace_na(list(topic_name = "NA")) %>% 
    mutate(topic = paste("Topic", topic)) %>% 
    group_by(topic) %>% 
    mutate(
      n = 1, 
      tot_resp = sum(n, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      topic = factor(topic),
      topic = fct_reorder(topic, tot_resp, max)) %>% 
    mutate(
      # topic_name = paste(
      #   "Topic", 
      #   letters_vect[as.character(as.numeric(topic)+1)]
      #        )
      numeric_topic = as.numeric(topic),
      topic_name = paste("Topic", letters_vect[numeric_topic])
    )
  
  city_choices <- sort(unique(city_pops$name))
  
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
  
  # Create a reactive Sup Sat data frame based on the input ----
  filtered_responses <- reactive({
    # Get selected topics from input
    selected_topics<-input$cities
    # Filter responses based on checkbox input
    filtered_responses_df <- super_saturday_feedback %>% 
      filter(topic_name %in% input$cities)
    
  }) # End of reactive Super Saturday responses 
  
  
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
