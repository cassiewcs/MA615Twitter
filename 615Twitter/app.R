
library(shiny)

##data input
#map
thor<-read.csv("Thor3_tweets.csv",header = TRUE, sep = ",")
thor<-data.frame(thor)
cap<-read.csv("Cap3_tweets.csv",header = TRUE, sep = ",")
cap<-data.frame(cap)
aven<-read.csv("Avengers3_tweets.csv",header = TRUE, sep = ",")
aven<-data.frame(aven)
mapdata<-rbind(thor,cap,aven)
#emoji
df.plot1<-read.csv("emoji1.csv",header = TRUE, sep = ",")
df.plot2<-read.csv("emoji2.csv",header = TRUE, sep = ",")
df.plot3<-read.csv("emoji3.csv",header = TRUE, sep = ",")
#wordcloud
wcdata<-read.csv("wcdata.csv", header = TRUE, sep = ",")

# Define UI 
 ui <- navbarPage("Marvel Movies Twitter Analysis",
                      
                       tabPanel("Mapping",
                                sidebarLayout(
                                  sidebarPanel(radioButtons("MovieInput1", "Movie",
                                                            choices = c("Thor:Ragnarok", "Captain America:Civil War","Avengers:Infinity War"),
                                                            selected = "Thor:Ragnarok")),
                                  mainPanel(
                                    h1("Locations of related twitters"),
                                    br(),
                                    leafletOutput("map",width="800px",height="400px")))),
                      tabPanel("Wordcloud",
                           sidebarLayout(
                             sidebarPanel(radioButtons("MovieInput2", "Movie",
                                                       choices = c("Thor:Ragnarok", "Captain America:Civil War","Avengers:Infinity War"),
                                                       selected = "Thor:Ragnarok"),
                                          sliderInput("maxInput","Maximum Number of Words:",min = 1,  max = 100,  value = 50)),
                                  mainPanel(
                                    h1("Wordclouds for the Marvel movies"),
                                    br(),
                                    h4("Words mentioned the most"),
                                    plotOutput("wordcloud",width="800px",height="800px")))))
 


# Define server
server <- function(input, output) {
   
   output$map <- renderLeaflet({
     
     filtered <-
       mapdata %>%
       filter( Movie == input$MovieInput1)
     color <- ifelse(input$MovieInput1 == "Thor:Ragnarok","red",
                     ifelse(input$MovieInput1 == "Captain America:Civil War","royalblue","thistle"))
     leaflet(filtered) %>% addTiles('http://{s}.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png')%>%
       setView(-95.7129, 42.358430, zoom = 4)%>% 
       addCircles(~place_lon, ~place_lat, popup=thor$place_lon, weight = 3, radius=40,
                  color=color, stroke = TRUE, fillOpacity = 0.8)
   })

   
   output$wordcloud <- renderPlot({
     
     filtered <- subset(wcdata, movie == input$MovieInput2)[,c(1,2)]
     
     wordcloud(filtered$word, filtered$freq, max.words = input$maxInput,colors=brewer.pal(n=8, "Dark2"),
               random.order=FALSE,rot.per=0.35)
     
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

