rm(list = ls())
library(shiny)
library(fullPage)
library(readxl)
library(usmap)
library(readr)
library(lubridate)
library(dplyr)
library(shinyBS)
library(shinyWidgets)
library(ggplot2)
library(RColorBrewer)



library(leaflet)
#devtools::install_github('bhaskarvk/leaflet.extras')
library(leaflet.extras)# to add addSearchFeatures #http://leaflet-extras.github.io/leaflet-providers/preview/index.html
library(sp)
library(raster)
library(rgeos)
library(sf)
library(maps)
library(tools)

library(rvest)
library(rjson)
library(jsonlite)
library(httr)



ab_data_team <- read.csv("data/AB_Data_20221117.csv", stringsAsFactors = F)
ab_data_team$month_char <- month(ymd(as.Date(ab_data_team$order_date, format = "%m/%d/%Y")), label=TRUE, abbr = F)
ab_data_team <- ab_data_team[ab_data_team$Weather_Type == "Outliers",]
ab_data_team$age <- trunc(as.numeric(difftime(Sys.Date() ,as.Date(ab_data_team$dob, format = "%m/%d/%Y"),units = "days")) / 365.25)
ab_data_team$age_group[ab_data_team$age <= 30 & ab_data_team$age >= 21] <- "22 - 30"
ab_data_team$age_group[ab_data_team$age <= 40 & ab_data_team$age >= 31] <- "31 - 40"
ab_data_team$age_group[ab_data_team$age <= 50 & ab_data_team$age >= 41] <- "41 - 50"
ab_data_team$age_group[ab_data_team$age <= 60 & ab_data_team$age >= 51] <- "51 - 60"
ab_data_team$age_group[ab_data_team$age >= 61] <- "Above 60"

gender_dt <- data.frame(table(ab_data_team$gender))
gender_dt$Var1 <- c("Female","Male")

p <- plotly::plot_ly(gender_dt, 
                     labels = ~Var1, 
                     values = ~Freq, 
                     type = 'pie',
                     textposition = ifelse(100*(gender_dt$Freq/sum(gender_dt$Freq))<8,"outside","inside"),
              
                     #textposition = 'inside',
                     sort = FALSE,
                     textinfo = 'label+value+percent',
                     #texttemplate = '<b>%{label}</br></br>%{percent}</b>', 
                     insidetextfont = list(color = '#FFFFFF'),
                     textfont = list(size = 20, color = "#4a1e15"),
                     hoverinfo = 'text',
                     text = ~Freq,
                     marker = list(colors = c("#636466","#e3af32"),
                                   line = list(color = '#FFFFFF', width = 1)),
                     #The 'pull' attribute can also be used to create space between the sectors
                     showlegend = FALSE)



bar_box_horizontal <- function(trimmer = 22, autorange = FALSE, plot_data,x_var,y_var,color_var,text,hovertext,title = "",x_var_label="",y_var_label="", colorRampPalette = TRUE,color_fill = c("#C2CFE7","#6787c4"), legend = "bottom", type_var="bar",alpha = 1,stack = TRUE,tickangle=0,mode_var="lines+markers", source = "summary_out") {
  legend_in <- legend
  if(legend_in  == "top"){
    legend <- list(orientation = "h",xanchor = "center",y = 1.0,x = 0.5)
    showlegend <- TRUE
  }else if(legend_in  == "bottom"){
    legend <- list(itemwidth = 29,orientation = "h",xanchor = "center",size = 12,y =-0.1,x = 0.5,
                   title=list(size = 12,color = "#485E89"),
                   font = list(size = 12,color = "#485E89"))
    showlegend <- TRUE
  }else if(legend_in  == "right"){
    legend <- list(orientation = "v",yanchor = "center",y = 1,x = 0.8)
    showlegend <- TRUE
  }else if(legend_in  == "left"){
    legend <- list(orientation = "v",yanchor = "center",y = 0.5,x = -0.15)
    showlegend <- TRUE
  }else{
    showlegend <- FALSE
  }
  if(isTRUE(colorRampPalette)){
    length_color <- length(unique(plot_data[[color_var]]))
    color_fill_out <-  colorRampPalette(color_fill)(length_color)
  }else{
    color_fill_out <- color_fill
  }
  
  if(isTRUE(stack)){
    barmode = 'stack'
  }else{
    barmode = 'group'
  }
  
  if(isFALSE(autorange)){
    autorange = "reversed"
  }else{
    autorange  = TRUE
  }
  
  if("total" %in% colnames(plot_data)){
    range <- c(ifelse(min(plot_data[["total"]]) >= 0, 0, min(plot_data[["total"]])*2), max(plot_data[["total"]])+(max(plot_data[["total"]])/7))
  }else{
    range <- c(ifelse(min(plot_data[,y_var]) >= 0, min(plot_data[,y_var]) - min(plot_data[,y_var])/5 , min(plot_data[,y_var])*2), max(plot_data[,y_var])+(max(plot_data[,y_var])/7))
  }
  label <- unique(as.character(plot_data[[x_var]]))
  tick.text <- trimmer(label, 22)
  
  
  p <- plotly::plot_ly(plot_data, 
                       x = ~ get(y_var), 
                       color = ~ get(color_var),
                       y = ~ get(x_var),
                       customdata = ~ get(color_var),
                       text = ~ formatC(get(text), format="f", big.mark=",", digits=0),
                       hoverinfo = "text",
                       hovertext = ~hovertext,
                       textposition = c('outside'),
                       textfont = list(size = 11, color = "black"),
                       type = type_var,
                       alpha = 1,
                       colors = color_fill_out,
                       orientation = 'h',
                       source = source)
  plot <- p %>% layout(
    barmode = "stack",
    bargap = 0.2, bargroupgap = 0.1,
    font = list(color = 'gray',size = 10),
    hoverlabel = list(font=list(size=11)),
    showlegend =  showlegend,
    title = list(text = title,font = list(size = 15,color = "#485E89")),
    margin =list( l=30,r=10,b=10,t=40),
    xaxis = list(
      # autorange = "reversed",
      range = range,
      fixedrange = TRUE,
      tickfont = list(
        
        size = 11,
        color = "#485E89"
      ),
      titlefont = list(
        
        size =  13,
        color = "#485E89"
      ),
      title = x_var_label,
      zeroline = FALSE,
      tickmode = "array",
      color = "#485E89"
      # tickvals=~ c(0, get(y_var)),
      # ticktext=~ c(0, get(y_var))
    ),
    yaxis = list(
      autorange =  autorange,
      tickfont = list(
        
        size = 11,
        color = "#485E89"),
      titlefont = list(
        
        size =  13,
        color = "#485E89"),
      title = y_var_label,
      zeroline = FALSE,
      # tickmode = "array",
      # tickvals = label,
      # ticktext = tick.text,
      # tickvals=~ c(0,get(x_var)),
      # ticktext=~ c(0,get(x_var)),
      color = "#485E89"),
    legend = legend)
  return(plot)
}









# str(ab_data_team)
# table(ab_data_team$gender)
# table(ab_data_team$age_group)
# table(ab_data_team$income)



#read data
product <- readxl::read_xlsx("data/AB_Data.xlsx", sheet = 1)
customer <- readxl::read_xlsx("data/AB_Data.xlsx", sheet = 2)
sale <- readxl::read_xlsx("data/AB_Data.xlsx", sheet = 3)

ab_data <- merge(sale, customer, all.x = TRUE, by = "cust_id")
ab_data <- merge(ab_data, product, all.x = TRUE, by = "sku_id")
ab_data$month <- month(ab_data$order_date, label=TRUE, abbr = F)

# table(unique(ab_data$city) %in% c("Washington",
#                        "Houston","New York City","El Paso","Dallas","Austin","San Antonio","Sacramento","Philadelphia","Miami"))


#Processing data
ab_data$age <- trunc(as.numeric(difftime(Sys.Date(), ab_data$dob, units = "days")) / 365.25)
ab_data$product_total_usd <- ab_data$unit_price * ab_data$qty
#[1] 45
  
#mapping
statepop$state <- statepop$abbr
statepop$location <- statepop$full
sale_map_dt <- merge(ab_data ,statepop[,c("state","location")], all.x = TRUE, by.y = "state", by.x = "st")

states <- read_rds("data/states.rds")
states$ID <- tools::toTitleCase(states$ID)
sale_map_dt <- sale_map_dt[!is.na(sale_map_dt$location), ]
sale_map_dt <- sale_map_dt[order(sale_map_dt$location,match(sale_map_dt$location,states$ID)),]
#https://rdrr.io/cran/leaflet/man/addLegend.html
sale_map_dt$labels <- sprintf("<strong style='color: red;font-size:14px;'>State: </strong><em style='font-size:14px;'>%s</em>
                               <br/><strong style='color: red;font-size:14px;'>City: </strong><em style='font-size:14px;'>%s</em>
                               <br/><strong style='color: red;font-size:14px;'>Customer ID: </strong><em style='font-size:14px;'>%g</em>
                               <br/><strong style='color: #3EACA8;font-size:14px;'>Product: </strong><em style='font-size:14px;'>%s</em>
                               <br/><strong style='color: #00d084;font-size:14px;'>Purchase: </strong><em style='font-size:14px;'>$%s</em>
                               ",
                              sale_map_dt$location,
                              sale_map_dt$city,
                              sale_map_dt$cust_id,
                              sale_map_dt$prod_name,
                              sale_map_dt$product_total_usd)%>% lapply(htmltools::HTML)

# https://www.weather.gov/documentation/services-web-api
get_weather_api <- function(lat, lng){
  
  url <- paste0("https://api.weather.gov/points/",lat,",",lng)
  print(url)
  url_forecast <- fromJSON(paste(readLines(url,warn=FALSE), collapse=""))
  
  forecast  <- url_forecast$properties$forecast
  print(forecast )

  dt_date  <- tryCatch(fromJSON(paste(readLines(forecast ,warn=FALSE), collapse="")), error = function(e) {return(NA)})
  print(all(is.na(dt_date)))
  
  while(all(is.na(dt_date))) {
    Sys.sleep(2) #Change as per requirement.
    dt_date <- tryCatch(fromJSON(paste(readLines(forecast ,warn=FALSE), collapse="")), error = function(e) {return(NA)})
  }

  periods <- dt_date$properties$periods[,c("name","startTime","temperature","icon", "shortForecast")]
  periods$temp_now <- periods$temperature[1]
  periods$shortForecast_now  <- periods$shortForecast[1]
  periods$date <- lubridate::ymd(as.Date(substr(periods$startTime, 1, 10)))
  periods$name <- gsub("Veterans Day","Friday", periods$name)

  
  if(periods$name[1] == "This Afternoon"| periods$name[1] == "Today"){
    periods$name[1] <- "Today"
    min <- periods$temperature[c(1:14)%%2==0]
    max <- periods$temperature[c(1:14)%%2!=0]
    periods <- periods[-(grep("Night|Tonight", periods$name)),]
    periods$min <- min
    periods$max <- max
  }else{
    max <- periods[-(grep("Night|Tonight|Overnight", periods$name)),]$temperature
    min <- periods[(grep("Night|Tonight|Overnight", periods$name)),]$temperature
    periods$name[1] <- "Today"
    periods <- periods[-(grep("Night|Tonight", periods$name)),]
    periods <- periods[-nrow(periods),]
    periods$min <- min
    periods$max <- max
  }
  
  periods$month <- month(periods$date, label=TRUE, abbr = F)
  periods$day <-  ifelse(day(periods$date)==1, "1<sup>st</sup>",
                         ifelse(day(periods$date)==2, "2<sup>nd</sup>",
                                ifelse(day(periods$date)==3, "3<sup>rd</sup>",paste0(day(periods$date), "<sup>th</sup>"))))
  periods$full_date <- paste0(periods$name,", ",periods$month, " ",periods$day)

  return(periods)
}

# url <- paste0("https://api.weather.gov/points/",40.8247,",",-96.6252)
# url_forecast <- fromJSON(paste(readLines(url,warn=FALSE), collapse=""))
# forecast  <- url_forecast$properties$forecast
# dt_date <- fromJSON(paste(readLines(forecast ,warn=FALSE), collapse=""))
# periods <- dt_date$properties$periods[,c("name","startTime","temperature","icon", "shortForecast")]
# weather_dt <- get_weather_api(33.8193,-118.2325)
# weather_dt <- get_weather_api(33.8193,-96.3829)
# print(weather_dt)
# dt_temp <- weather_dt
# i = 1

get_temp_plot <- function(dt_temp){
  for(i in 1:nrow(dt_temp)){
    dt <- dt_temp[i,]
    
    dt_plot = data.frame(name = dt$name, ave = seq(dt$min, dt$max, 1))
    dt_plot$color <- ifelse(dt_plot$ave > 100,"#D73027",
                        ifelse(dt_plot$ave > 90,"#F46D43",
                         ifelse(dt_plot$ave > 80,"#FDAE61",
                          ifelse(dt_plot$ave > 70,"#FEE090",
                          ifelse(dt_plot$ave > 60, "#E0F3F8",
                           ifelse(dt_plot$ave > 40, "#ABD9E9",
                            ifelse(dt_plot$ave > 30,"#74ADD1", 
                             ifelse(dt_plot$ave > 20,"#4575B4", 
                                    "#313695"))))))))
    
    out <- ggplot(dt_plot , aes(name,ave ,fill = ave )) + geom_bar(stat = "identity")+
      scale_fill_continuous(low = dt_plot$color[1], high = dt_plot$color[nrow(dt_plot)]) + #trans = 'reverse'#scale_fill_gradientn(colours = dt_plot$color) +
      coord_flip() +theme_minimal()+
      theme(
        panel.grid.minor = element_blank(), 
        panel.grid.major = element_blank(), 
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(), #remove x axis ticks
        axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank(),  #remove y axis ticks
        legend.position = "none"
      )
    ggsave(paste0("www/weather_plot/",dt$name, ".png"), width = 200, height = 80,units = "px")
  }

}

# url <- paste0("https://api.weather.gov/points/",sale_map_dt$lat[1],",",sale_map_dt$lng[1])
# url_forecast <- fromJSON(paste(readLines(url,warn=FALSE), collapse=""))
# forecast  <- url_forecast$properties$forecast
# dt_date <- fromJSON(paste(readLines(forecast ,warn=FALSE), collapse=""))


#dt_temp <- get_weather_api(sale_map_dt$lat[1], sale_map_dt$lng[1])


# display.brewer.pal(8,"RdYlBu")
# brewer.pal(8,"RdYlBu")
#get_temp_plot(dt_temp)


# brewer.pal(8,"RdYlBu")
# display.brewer.all()

color_set <- c(brewer.pal(8,"YlOrRd"), brewer.pal(10,"RdYlBu") ,brewer.pal(8,"Accent"),brewer.pal(9,"Set1"))













