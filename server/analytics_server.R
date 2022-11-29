source("global.R")


output$location <- renderUI({
  selectInput("location", label = "Select Cities: ", choices = unique(dt_predict$city), multiple = F, selected = "New York City")
})


output$rev_ab <- renderUI({
  radioButtons("rev_ab", label = NULL, choices = c("Revenue", "Quantity"), selected = "Revenue", inline = TRUE)
})

output$rev_nonab <- renderUI({
  radioButtons("rev_nonab", label = NULL, choices = c("Revenue", "Quantity"), selected = "Revenue", inline = TRUE)
})



# output$month<- renderUI({
#   selectInput("month", label = "Select Months: ", choices = unique(ab_data_team$month), multiple = F, selected = unique(ab_data_team$month)[11])
# })

output$weather_type <- renderUI({
  req(input$location)
  choices_dt <- ab_data_team[ab_data_team$city %in% c(input$location), ]
  choices <- unique(choices_dt$Weather_Type)

  choices <- choices[choices %in% c("Moderately Cold", "Moderately Hot", "Rainy","Super Cold")]

  
  selectInput("weather_type", label = "Select Weather Type: ", 
              choices = choices, 
              multiple = F, 
              selected = choices[3])
})


output$ana_ui <- renderUI({
  div(
    style = 'padding:5% 5% 5% 5%;',
    wellPanel(
      style = 'background-color:rgba(250,250,250,0.9);text-align:left;color:black;padding-top:0px;',
    fluidRow(
      column(
        width = 6,
          uiOutput("location")),
      column(
        width = 6,
          uiOutput("weather_type")
        )),
    fluidRow(
      column(
        width = 6,
        h5("AB InBev's Beer December Prediction"),
        uiOutput("rev_ab"),
        plotly::plotlyOutput("ab_pred_out", width = "100%", height = "300px")
      ),
      column(
        width = 6,
        h5("Competitor's Beer December Prediction"),
        uiOutput("rev_nonab"),
        plotly::plotlyOutput("nonab_pred_out", width = "100%", height = "300px")
      )
      ),
    fluidRow(
      column(
        width = 12,
        uiOutput("tab2")
      )
      )
    )
  )
})


values <- reactiveValues(dt = ab_data_team,
                         dt_gender = ab_data_team,
                         dt_race = ab_data_team,
                         dt_income = ab_data_team,
                         dt_age = ab_data_team,
                         dt_map = ab_data_team,
                         dt_pre = dt_predict
                         )


observeEvent(c(input$location,input$weather_type),{
  
  sale_dt <- ab_data_team[ab_data_team$city %in% c(input$location) & 
                            #ab_data_team$month_char %in% c(input$month) & 
                            ab_data_team$Weather_Type %in% input$weather_type , ]
   values$dt <- sale_dt
   
   dt_pred <- dt_predict[dt_predict$city %in% c(input$location) & dt_predict$Weather_Type %in% input$weather_type,]
   values$dt_pred  <- dt_pred
   values$dt_pred_ab <- dt_pred[, c( "order_date","city","Weather_Type","AB_rev")]
   values$dt_pred_ab$pre_value <- values$dt_pred_ab$AB_rev
   values$dt_pred_nonab <- dt_pred[, c( "order_date","city","Weather_Type","nonAB_rev")]
   values$dt_pred_nonab$pre_value <- values$dt_pred_nonab$nonAB_rev
   
  if(nrow(sale_dt )>= 1) {
    
    dt_gender <- get_plot_dt(sale_dt , "gender")
    values$dt_gender <- dt_gender
    
    dt_race <- get_plot_dt(sale_dt, "race")
    values$dt_race <- dt_race
    
    dt_income <- get_plot_dt(sale_dt, "income_text", decreasing = FALSE)
    values$dt_income <- dt_income
    
    dt_age <- get_plot_dt(sale_dt, "age_group", decreasing = FALSE)
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
          width = 3,
          plotly::plotlyOutput("race_out", width = "100%", height = "300px")),
        column(
          width = 3,
          plotly::plotlyOutput("age_out", width = "100%", height = "300px")),
        column(
          width = 3,
          plotly::plotlyOutput("gender_out", width = "100%", height = "300px")),
        column(
          width = 3,
          plotly::plotlyOutput("income_out", width = "100%", height = "300px"))
          )
      )
    })
  }

})


observeEvent(input$rev_ab, {
  if(input$rev_ab == "Revenue"){
    dt <- values$dt_pred
    dt <- dt[, c( "order_date","city","Weather_Type","AB_rev")]
    dt$pre_value <- dt$AB_rev
    type = 'scatter'
  }else{
    dt <- values$dt_pred
    dt <- dt[, c( "order_date","city","Weather_Type","AB_qty")]
    dt$pre_value <- dt$AB_qty
    type = "bar"
  }
  
  values$dt_pred_ab <- dt
  output$ab_pred_out <- plotly::renderPlotly({
    p <- plotly::plot_ly(values$dt_pred_ab, x = ~order_date, y = ~pre_value, type =  type , mode = 'lines+markers')
    plot <- p %>% plotly::layout(title = '',
                                 showlegend = FALSE,
                                 separators = ',.',
                                 xaxis = list( title = ""),
                                 yaxis = list( title = input$rev_ab)) 
    plot
  })
  
})

observeEvent(input$rev_nonab, {
  if(input$rev_nonab == "Revenue"){
    dt <- values$dt_pred
    dt <- dt[, c( "order_date","city","Weather_Type","nonAB_rev")]
    dt$pre_value <- dt$nonAB_rev
    type = 'scatter'
  }else{
    dt <- values$dt_pred
    dt <- dt[, c( "order_date","city","Weather_Type","nonAB_qty")]
    dt$pre_value <- dt$nonAB_qty
    type = "bar"
  }

  values$dt_pred_nonab <- dt
  output$nonab_pred_out <- plotly::renderPlotly({
    p <- plotly::plot_ly(values$dt_pred_nonab , x = ~order_date, y = ~pre_value, type = type, mode = 'lines+markers')
    plot <- p %>% plotly::layout(title = '',
                                 showlegend = FALSE,
                                 separators = ',.',
                                 xaxis = list( title = ""),
                                 yaxis = list( title = input$rev_nonab)) 
    plot
  })
})


# output$pred <- renderUI({
#   div(
#     style = 'background-color:rgba(250,250,250,1);text-align:center;color:black;padding-top:0px;padding-bottom:0px;',
#     fluidRow(
#       column(
#         width = 6,
#         plotly::plotlyOutput("ab_pred_out", width = "100%", height = "300px")),
#       column(
#         width = 6,
#         plotly::plotlyOutput("ab_pred_out", width = "100%", height = "300px"))
#     )
#   )
# })









