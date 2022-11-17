
data <- data.frame(m = "monday", y = c(1:50))
out <- ggplot(data , aes(m,y,fill = y)) + geom_bar(stat = "identity") + scale_fill_continuous(low = "yellow", high = "blue") +coord_flip() +theme_minimal()+
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

out <- ggplot(data , aes(m,y,fill = y)) + geom_bar(stat = "identity")+
  scale_fill_gradientn(colours = c("yellow","red","blue")) +
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

ggsave("www/monday.png", width = 200, height = 70,units = "px")
