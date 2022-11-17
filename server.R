source("global.R")


server <- function(input, output, session) {
  
  values <- reactiveValues(plot_region  = NULL)
  # map tab
  output$map <- renderLeaflet({
    
    #factpal <- colorFactor(heat.colors(36),  sale_map_dt$prod_name)
    factpal <- colorFactor(color_set,  sale_map_dt$prod_name)
    sale_map_dt$radius <- sale_map_dt$unit_price * sale_map_dt$qty*1000
    
    fig_map  <-  leaflet::leaflet(sale_map_dt) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 5) %>% 
      addProviderTiles( "OpenStreetMap.DE") %>% # Esri.WorldStreetMap
      addCircles(lng = ~lng,
                 lat = ~lat,
                 layerId =  ~ city,
                 weight = 1,
                 radius = ~ radius,
                 fillOpacity = 1,
                 popup = ~labels,
                 color = ~factpal(prod_name),
                 #label = ~labels,
                 #fillColor = "red",
                 stroke = FALSE,
                 group = ~city
                 ) %>% addResetMapButton() %>%
      addLayersControl(overlayGroups = c("city")) %>%
      leaflet.extras::addSearchFeatures(
        targetGroups = "city",
        options = searchFeaturesOptions(
          zoom=7, 
          openPopup = TRUE, 
          firstTipSubmit = TRUE,
          autoCollapse = TRUE, 
          hideMarkerOnCollapse = TRUE,
        )) %>%  leaflet.extras::addStyleEditor(position = "bottomright")
  values$plot_region <- fig_map
  })
  
  
  observeEvent(input$map_shape_click,{
    print(input$map_shape_click)
    input$map_shape_click$id
    map_dt_location  <- sale_map_dt[sale_map_dt$city ==  input$map_shape_click$id, c("lat","lng","location","city")]
    values$map_dt_location <-  map_dt_location
    
    
    weather_data <- get_weather_api(map_dt_location$lat[1], map_dt_location$lng[1])
    if(nrow(weather_data )>= 1){
      values$weather_data <-  weather_data 
      get_temp_plot(weather_data)
    }
 
    values$weather_data$label <- paste0('
                             <div class="row" style="horizontal-align:middle;color:black;vertical-align:middle;" >
                             
                             <div class="col-md-4" style="text-align:left;padding-top:10px;">
                             <p style = "font-size:18px;">',weather_data$full_date,'</p>
                             </div>
                             
                             <div class="col-md-1" style="text-align:center;">
                             <img src="',weather_data$icon,'" width="50" height="50"/>
                             </div>
                             
                             <div class="col-md-1" style="text-align:center;padding-top:10px;">
                             <p style = "font-size:18px;">',weather_data$min,'<sup>o</sup>F</p>
                             </div>
                             
                             <div class="col-md-2" style="text-align:center;padding-top:5px;">
                             <img src="weather_plot/',weather_data$name,'.png" width= 100% height="50"/>
                             </div>
                             
                             <div class="col-md-1" style="text-align:center;padding-top:10px;">
                             <p style = "font-size:18px;">',weather_data$max,'<sup>o</sup>F</p>
                             </div>
                             
                             </div>
                             ')
    values$weather_data$id <- c(1:nrow(values$weather_data))
    
    print(values$weather_data)

      output$location_select <- renderText({
        HTML( paste0(map_dt_location$city[1],", ",  map_dt_location$location[1]))
      })
      
      output$temp <- renderText({
        HTML(paste0(values$weather_data$temp_now[1], "<sup>o</sup>F"))
      })
      
      output$forcast <- renderText({
        HTML(paste0(values$weather_data$shortForecast_now[1]))
      })
      
      output$weather_sche <- renderUI({
        lapply(1:nrow(values$weather_data), function(i){
          actionBttn(
            inputId = paste0("schedule_", values$weather_data$id[[i]]),
            label = HTML(values$weather_data$label[[i]]),
            style = "stretch",
            color = 'primary',
            block = TRUE,
            size = 'md'
          )
        })
    })
    
    output$weather_schedule <- renderUI(
      {
        div(
          style = 'padding:0 10% 0 10%;',
          fluidRow(
            column(
              width = 10,offset = 1,
              wellPanel(
                style = 'background-color:rgba(250,250,250,0.9);text-align:left;color:white;padding-top:0px;',
                h2(htmlOutput("location_select"), style = "color:black;text-align:center;"),
                h1(htmlOutput("temp"), style = "color:black;text-align:center;"),
                h4(htmlOutput("forcast"), style = "color:black;text-align:center;")),
                br(),
                wellPanel(
                  style = 'background-color:rgba(250,250,250,0.7);text-align:left;color:white;',
                uiOutput("weather_sche")
              )
            )
          )
        )
      })
    
  })
  


  
  
  
  output$plot_region <- renderUI({
    tagList(
        div(style="max-height:800px;position: relative",leafletOutput("map", width = "100%",height= "800px")),
        div(
          style = "position: absolute; left: 4em; bottom: 0.5em;",
          downloadButton("download_region", label = NULL,icon = icon("download"),class = "btn-m", title = "",
                         style = "position: absolute; right: 10px;bottom:10px;color:#636466;")
        )
    )

  })
  
  output$download_region <- downloadHandler(
    filename = function() {'sale_region.html'},
    content = function(file) {
      htmlwidgets::saveWidget(as_widget(values$plot_region), file)
    }
  )
  
  
  #==============================
  output$weather <- renderUI(
    {
      div(
        style = 'padding:0 10% 0 10%;',
        fluidRow(
          column(
            width = 6,
            wellPanel(
              style = 'background-color:rgba(250,250,250,0.85);',
              actionBttn(
                inputId = "monday",
                label = HTML('<div class="row"  style="horizontal-align:middle;padding-top:5px;" >
                                   <div class="col-md-3" style="text-align:left;horizontal-align:middle">
                                        <small>Monday</small>
                                   </div>
                                   <div class="col-md-2">
                                        <img src="sunny.png" width="40" height="40"/>
                                   </div>
                                   <div class="col-md-1">
                                        <small>35<sup>o</sup>F</small>
                                   </div>
                                   <div class="col-md-3">
                                        <img src="monday.png" width="120" height="50"/>
                                   </div>
                                   <div class="col-md-2">
                                        <small>70<sup>o</sup>F</small>
                                   </div>
                                </div>
                                   '),
                style = "stretch",
                color = 'primary',
                block = TRUE,
                size = 'lg'
              ),
              actionBttn(
                inputId = "tuesday",
                label = HTML('<div class="row"  style="horizontal-align:middle;padding-top:5px;" >
                                   <div class="col-md-3" style="text-align:left;horizontal-align:middle">
                                        <small>Tuesday</small>
                                   </div>
                                   <div class="col-md-2">
                                        <img src="bitmaps/snow-day.png" width="40" height="40"/>
                                   </div>
                                   <div class="col-md-1">
                                        <small>35<sup>o</sup>F</small>
                                   </div>
                                   <div class="col-md-3">
                                        <img src="monday.png" width="120" height="50"/>
                                   </div>
                                   <div class="col-md-2">
                                        <small>70<sup>o</sup>F</small>
                                   </div>
                                </div>
                                   '),
                style = "stretch",
                color = 'primary',
                block = TRUE,
                size = 'lg'
              )
             )
            )
          )
        )
    })
  
  # observeEvent(input$monday,{
  #   print(input$monday)
  # })


  
  
  
  #==============================
  
}