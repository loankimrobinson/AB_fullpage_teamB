




output$visualization_ui <- renderUI({
  div(
    style = 'padding:5% 5% 5% 5%;',
    wellPanel(
      style = 'background-color:rgba(250,250,250,0.9);text-align:left;color:black;padding-top:0px;',
      fluidRow(column(width = 12,
      fluidRow(
        column(
          width = 6,h3("RFM Model")
        ),
        column(
          width = 6,h3("Customer Segment"),
          selectInput("seg", label = "Segment", choice = c("Segments","Gender","Brands", "Weather","State"), selected ="Segments"),
          uiOutput("image")
        )
      )
    )
  ),
  fluidRow(column(width = 12,h3("Definition")))
    ))
})



output$image <- renderUI({
  req(input$seg)
  if(input$seg == "Segments"){
    tags$img(src = "RFM_1.png",width = "650px")
  }else if(input$seg == "Gender"){
    tags$img(src = "RFM_2.png",width = "650px")
  }else if(input$seg == "Brands"){
    tags$img(src = "RFM_3.png",width = "650px")
  }else if(input$seg == "Weather"){
    tags$img(src = "RFM_4.png",width = "650px")
  }else{
    tags$img(src = "RFM_5.png",width = "650px")
  }

})