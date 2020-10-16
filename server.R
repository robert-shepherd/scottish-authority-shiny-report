#Script 3/5: Server for shiny app

shinyServer(function(input,output,session) {
  
  observe({
    
    #Creating a list of variables for the selected ref area
    ref_area_variables <- unique_variables %>%
      filter(ref_area_type == input$selected_ref_area) %>%
      select(variable) %>%
      arrange(variable)
    
    #Updating X variables based on ref area selected
    updateSelectInput(session,"x_variable",
                      choices = ref_area_variables$variable)
    
    
    #Updating Y variables based on ref area selected
    updateSelectInput(session,"y_variable",
                      choices = ref_area_variables$variable)
  })
  
  observe({
    #Updating year where both X and Y variable are present
    selected_data <- scottish_neighbourhoods %>%
      filter(ref_area_type == input$selected_ref_area &
               !is.na(scottish_neighbourhoods[input$x_variable]) &
               !is.na(scottish_neighbourhoods[input$y_variable])) %>%
      select(year) %>%
      distinct %>%
      arrange(year)
    
    updateSelectInput(session,"year",
                      choices = selected_data$year)
  })
  
  #Filtering data for use in the plot
  filtered_data <- reactive({
    scottish_neighbourhoods %>%
      mutate(x = scottish_neighbourhoods[[input$x_variable]]
             ,y = scottish_neighbourhoods[[input$y_variable]]) %>%
      filter(ref_area_type == input$selected_ref_area 
             & year == input$year
             & !is.na(scottish_neighbourhoods[input$x_variable])
             & !is.na(scottish_neighbourhoods[input$y_variable])) %>%
      select(`Local Authority`, x, y)
  })
  
  #Plotting data
  output$variable_comparison <- renderPlot({
    ggplot(filtered_data(),aes(x,y)) +
      geom_point(aes(color = `Local Authority`)) +
      xlab(input$x_variable) +
      ylab(input$y_variable)
  })
  
  observe({
    #Creating a list of variables for the selected ref area
    ref_area_variables_2 <- unique_variables %>%
      filter(ref_area_type == input$selected_ref_area_2) %>%
      select(variable) %>%
      arrange(variable)
    
    #Updating Y variables based on ref area selected
    updateSelectInput(session,"y_variable_2",
                      choices = ref_area_variables_2$variable)
    
  })
  
  observe({
    #Updating spatial units where Y variable is present
    selected_data_2 <- scottish_neighbourhoods %>%
      filter(ref_area_type == input$selected_ref_area_2 &
               !is.na(scottish_neighbourhoods[input$y_variable_2])) %>%
      select(ref_area_desc) %>%
      distinct %>%
      arrange(ref_area_desc)
    
    updateSelectizeInput(session,"spatial_units"
                         ,choices = selected_data_2$ref_area_desc
                         )
  })
  
  #Filtering data for use in plot
  filtered_data_2 <- reactive({
    scottish_neighbourhoods %>%
      mutate(Year = scottish_neighbourhoods[["year"]]
             ,y = scottish_neighbourhoods[[input$y_variable_2]]) %>%
      filter(ref_area_type == input$selected_ref_area_2
             & ref_area_desc %in% input$spatial_units
             & !is.na(scottish_neighbourhoods[input$y_variable_2])
      ) %>%
      select(`Spatial unit` = ref_area_desc, Year, y)
  })
  
  #Plotting data
  output$trend <- renderPlot({
    p <- ggplot(filtered_data_2(),aes(Year,y)) +
      geom_line(aes(color = `Spatial unit`)) +
      ylab(input$y_variable_2)
    
    p$labels$colour <- input$selected_ref_area_2
    p
  })
  
  #Creating a title for the data table
  output$table_title <- renderText({
    paste0(input$y_variable_2," by ",input$selected_ref_area_2)
  })
  
  #Spreading data for the data table
  trend_table <- reactive({
    spread(filtered_data_2(),`Spatial unit`,y)
  })
  
  #Rendering data table
  output$trend_data <- DT::renderDataTable(
    DT::datatable(trend_table()
                  ,options = list(searching = FALSE
                                  ,paging = FALSE
                                  ,info = FALSE)
                  ,rownames = FALSE)
  )
  
  #Creating a download button for the data table
  output$downloadData <- downloadHandler(
    filename = "download_data.csv",
    content = function(file) {
      write.csv(trend_table(), file, row.names = FALSE)
    }
  )
  #App must be run in browser so that the download filename is generated correctly
  #using runApp(launch.browser = TRUE)
  
  #Download markdown report
  output$downloadReportButton <- downloadHandler(
    filename = "report.docx",
    content = function(file) {
      render("report.Rmd", output_format="word_document",
             output_file=file,
             params=list(selected_ref_area_2=input$selected_ref_area_2
                         ,y_variable_2=input$y_variable_2
                         ,spatial_units=input$spatial_units))
    }
  )
  
})

