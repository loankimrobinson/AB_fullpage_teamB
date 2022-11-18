


output$location <- renderUI({
  selectInput("location", label = "Select Cities: ", choices = c("Washington",
  "Houston","New York City","El Paso","Dallas","Austin","San Antonio","Sacramento","Philadelphia","Miami"), multiple = F, selected = "New York City")
})

output$month<- renderUI({
  selectInput("month", label = "Select Months: ", choices = ab_data$month, multiple = F, selected = ab_data$month[11])
})

output$weather_type <- renderUI({
  req(input$location, input$month)
  choices_dt <- ab_data_team[ab_data_team$city %in% c(input$location) & ab_data_team$month_char %in% c(input$month), ]
  selectInput("weather_type", label = "Select Weather Type: ", 
              choices = unique(choices_dt$Weather_Type), 
              multiple = F, 
              selected = unique(choices_dt$Weather_Type)[1])
})


output$ana_ui <- renderUI({
  div(
    style = 'padding:0 10% 0 10%;',
    fluidRow(
      column(
        width = 3,
        wellPanel(
          style = 'background-color:rgba(250,250,250,0.9);text-align:left;color:black;padding-top:0px;',
          uiOutput("location"),
          uiOutput("month"),
          uiOutput("weather_type")
        # br(),
        # wellPanel(
        #   style = 'background-color:rgba(250,250,250,0.7);text-align:left;color:white;')
          #uiOutput("weather_sche")
        )
      )
    )
  )
})


observeEvent(input$location,{
  dt <- ab_data[ab_data$city %in% c(input$location), ]
  print(dt)
  print(unique(dt$city))
})


