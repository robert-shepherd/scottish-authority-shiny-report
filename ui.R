#Script 4/5: UI for shiny app

fluidPage(
  
  titlePanel("Scottish Neighbourhood Data Explorer"),
  fluidRow(
    column(12
           ,h4("Introduction")
           ,p("This application aims to allow the user to explore data relating to
              Scottish neighbourhoods.")
           ,p("The user can select between different geographical units and each of the 
              subsequent options will be updated so that only information available can be 
              selected.")
           ,p("As updating options can cause some variables to reset, it is recommended
              that the options are set in the order provided."))
  ),
  fluidRow(
    hr(),
    column(12
           ,h4("Compare variables"))
  ),
  fluidRow(
    column(8
           ,plotOutput("variable_comparison")
    ),
    column(4,
           wellPanel(
             h4("Change plot options"),
             selectInput("selected_ref_area","1. Select geographical unit",ref_area_type),
             selectInput("x_variable","2. Select X variable","Crime"),
             selectInput("y_variable","3. Select Y variable","Crime"),
             selectInput("year","4. Select year","")
           ))
  ),
  fluidRow(
    hr()
    ,column(12
            ,h4("Trend data over time")
            ,p("Note: Plot initially appears blank, select spatial units to populate data.")
            ,downloadButton("downloadReportButton", "Download report"))
  ),
  fluidRow(
    column(8
           ,plotOutput("trend")
    ),
    column(4,
           wellPanel(
             h4("Change plot options"),
             selectInput("selected_ref_area_2","1. Select geographical unit",ref_area_type),
             selectInput("y_variable_2","2. Select Y variable","Crime"),
             selectizeInput("spatial_units","3. Select (up to 4) spatial units"
                            ,default_spatial$ref_area_desc
                            ,multiple = TRUE
                            ,options = list(maxItems = 4))
           ))
  ),
  fluidRow(
    column(12,
           h4(textOutput("table_title"))
           ,downloadButton("downloadData", "Download data")
           ,dataTableOutput("trend_data")
    )
  )
)