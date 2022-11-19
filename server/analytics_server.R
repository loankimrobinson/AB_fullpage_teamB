source("global.R")


output$location <- renderUI({
  selectInput("location", label = "Select Cities: ", choices = c("Washington",
  "Houston","New York City","El Paso","Dallas","Austin","San Antonio","Sacramento","Philadelphia","Miami"), multiple = F, selected = "New York City")
})

output$month<- renderUI({
  selectInput("month", label = "Select Months: ", choices = unique(ab_data_team$month), multiple = F, selected = unique(ab_data_team$month)[11])
})

output$weather_type <- renderUI({
  req(input$location, input$month)
  ab_data_team$month_char <- as.character(ab_data_team$month_char)
  choices_dt <- ab_data_team[ab_data_team$city %in% c(input$location) & ab_data_team$month_char %in% c(input$month), ]
  selectInput("weather_type", label = "Select Weather Type: ", 
              choices = unique(choices_dt$Weather_Type), 
              multiple = F, 
              selected = unique(choices_dt$Weather_Type)[1])
})


output$ana_ui <- renderUI({
  div(
    style = 'padding:5% 5% 5% 5%;',
    wellPanel(
      style = 'background-color:rgba(250,250,250,0.9);text-align:left;color:black;padding-top:0px;',
    fluidRow(
      column(
        width = 3,
          uiOutput("location"),
          uiOutput("month"),
          uiOutput("weather_type")
        ),
      column(
        width = 9,
        uiOutput("tab2"))
      )
    )
  )
})


values <- reactiveValues(dt = ab_data_team,
                         dt_gender = ab_data_team,
                         dt_race = ab_data_team,
                         dt_income = ab_data_team,
                         dt_age = ab_data_team,
                         dt_map = ab_data_team)


observeEvent(c(input$location,input$weather_type),{
  
  sale_dt <- ab_data_team[ab_data_team$city %in% c(input$location) & 
                            ab_data_team$month_char %in% c(input$month) & 
                            ab_data_team$Weather_Type %in% input$weather_type , ]
  values$dt <- sale_dt 
  
  if(nrow(sale_dt )>= 1) {
    
    dt_gender <- get_plot_dt(sale_dt , "gender")
    values$dt_gender <- dt_gender
    
    dt_race <- get_plot_dt(sale_dt, "race")
    values$dt_race <- dt_race
    
    dt_income <- get_plot_dt(sale_dt, "income_text")
    values$dt_income <- dt_income
    
    dt_age <- get_plot_dt(sale_dt, "age_group")
    values$dt_age <- dt_age
    
    output$race_out <- plotly::renderPlotly({
      p <- bar_plot(plot_data = values$dt_race,
                    y_var = "count",
                    x_var = "race",
                    color_var = "race",
                    text = "count",
                    legend = "bottom",
                    type_var="bar",
                    color_fill_out = c("#e3af32","#636466"),
                    source = "summary_out",
                    y_var_label = "",
                    x_var_label = "",
                    title = "",
                    hovertext = "hovertext"
      )
      values$race_out <- p
    })

    output$gender_out <- plotly::renderPlotly({
      p <- pie_plot(dt = values$dt_gender, label = "gender", value = "count", hovertext = "hovertext",colors = c("#636466","#e3af32"))
      values$gender_out <- p

    })

    output$age_out <- plotly::renderPlotly({
      p <- bar_plot(plot_data = values$dt_age,
                    y_var = "count",
                    x_var = "age_group",
                    color_var = "age_group",
                    text = "count",
                    legend = "bottom",
                    type_var="bar",
                    color_fill_out = c("#e3af32","#636466"),
                    source = "summary_age_group",
                    y_var_label = "",
                    x_var_label = "",
                    title = "",
                    hovertext = "hovertext"
      )
      values$age_out <- p
    })

    output$income_out <- plotly::renderPlotly({
      p <- bar_plot(plot_data = values$dt_income,
                    y_var = "count",
                    x_var = "income_text",
                    color_var = "income_text",
                    text = "count",
                    legend = "bottom",
                    type_var="bar",
                    color_fill_out = c("#e3af32","#636466"),
                    source = "summary_income_text",
                    y_var_label = "",
                    x_var_label = "",
                    title = "",
                    hovertext = "hovertext"
      )
      values$income_out <- p
    })
    
    output$tab2 <- renderUI({
      div(
      style = 'background-color:rgba(250,250,250,1);text-align:center;color:black;padding-top:0px;padding-bottom:0px;',
      fluidRow(
        column(
          width = 6,""
        ),
        column(
          width = 6,
          fluidRow(
            column(6,plotly::plotlyOutput("race_out", width = "100%", height = "300px"),
                   plotly::plotlyOutput("age_out", width = "100%", height = "300px")),
            column(6,plotly::plotlyOutput("gender_out", width = "100%", height = "300px"),
                   plotly::plotlyOutput("income_out", width = "100%", height = "300px"))
          )
        )
      )
    )
    })
    
  }
 

  
  
  
  
})








