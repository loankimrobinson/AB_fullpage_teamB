




output$visualization_ui <- renderUI({
  div(
    style = 'padding:5% 5% 5% 5%;',
    wellPanel(
      style = 'background-color:rgba(250,250,250,0.9);text-align:left;color:black;padding-top:0px;',
      fluidRow(column(width = 12,
      fluidRow(
        column(
          width = 6,h3("RFM Model"),
          plotly::plotlyOutput("rfm_model", width ="100%")
        ),
        column(
          width = 6,h3("Customer Segment"),
          selectInput("seg", label = "Segment", choice = c("Segments","Gender","Brands", "Weather","State"), selected ="Segments"),
          uiOutput("image")
        )
      )
    )
  ),
  fluidRow(column(width = 12,h3("Definition", style = "text-align:center;"),
                  
                  h5(HTML("Champion: Customers with Recency, Frequency, and Monetary value in the first quartile.<br><br>
                  Loyal: Purchase recency, frequency, and monetary are in 1st or 2nd quartile<br><br>
                  Potential loyal: Purchase recently but not very frequent and order value are relatively low.<br><br>
Risk of losing: Purchase recency, frequency, and monetary value are in 3rd or 4th quartile.<br><br>
Lost: Purchase recency, frequency, and monetary value are all in the 4th quartile.<br>"), style = "text-align:center;")
                  ))
    ))
})



output$rfm_model <- plotly::renderPlotly({
  bar_plot(plot_data = profile_ab,
           y_var = "value",
           x_var = "rfm_group",
           color_var = "name",
           text = "value",
           legend = "bottom",
           type_var="bar",
           color_fill_out = c("gray","#636466","#e3af32"),
           source = "summary_out",
           y_var_label = "",
           x_var_label = "",
           title = "",
           hovertext = "hovertext"
  )
  
})



output$image <- renderUI({
  req(input$seg)
  if(input$seg == "Segments"){
    tags$img(src = "RFM_1.png",width = "600px")
  }else if(input$seg == "Gender"){
    tags$img(src = "RFM_2.png",width = "600px")
  }else if(input$seg == "Brands"){
    tags$img(src = "RFM_3.png",width = "600px")
  }else if(input$seg == "Weather"){
    tags$img(src = "RFM_4.png",width = "600px")
  }else{
    tags$img(src = "RFM_5.png",width = "600px")
  }

})