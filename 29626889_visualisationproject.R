
# load the required libraries
require(plotly)
require(dplyr)
require(ggplot2)
require(leaflet)
require(shinydashboard)
require(flexdashboard)
require(shinythemes)
require(shiny)

# load the data file
data <- read.csv('finaldata.csv')


# define the UI for the body using dashboardBody
body <- dashboardBody(
navbarPage("", id="nav",
# create a new tab for interactive map  
    tabPanel("Interactive map",
    fluidPage( theme= shinytheme("flatly"),
    fluidRow( # creates the space for displaying the interactive map
              column(8,
              title = "Interactive map of crime in London", leafletOutput("crimeMap",height = "550px")
            ),
              column(4,box(width=12,background = "blue",
              #creates a drop down filter to select crime type             
              selectInput("crime", "Select type of crime:",
                          c("Burglary"="Burglary","Criminal Damage"="Criminal Damage","Drugs"="Drugs","Other Notifiable Offences"="Other Notifiable Offences",                      
                            "Robbery"="Robbery","Theft and Handling"="Theft and Handling","Violence Against the Person"="Violence Against the Person"
                          )),
              box(background = "blue",width=12,plotOutput("plot1"))
)
)
))),
  
# create a new tab for prediction plots
tabPanel("Prediction",
         
           fluidPage( theme= shinytheme("flatly"),
           fluidRow(
                        column(8,
                          title = "Prediction chart for crime trend", plotlyOutput("prediction"))#creates space for prediction plot
                        ,
                        column(4,
                          
                          box( 
                            width = 12,background = 'blue',
                            #creates filter for selecting crime type, borough and smoother type
                           selectInput("predictCrime", "Select type of crime:",c("Burglary"="Burglary","Criminal Damage"="Criminal Damage","Drugs"="Drugs","Other Notifiable Offences"="Other Notifiable Offences","Robbery"="Robbery","Theft and Handling"="Theft and Handling","Violence Against the Person"="Violence Against the Person")),
                           selectInput("predictBorough", "Select borough:",c("Barking and Dagenham"="Barking and Dagenham",   "Barnet"="Barnet",  "Bexley"="Barnet" , "Brent" ="Barnet","Bromley" = "Bromley" ,  "Camden" ="Camden" ,   "City of London" = "City of London" ,  "Croydon" = "Croydon" ,"Ealing"= "Ealing",  "Enfield" = "Enfield" ,  "Greenwich"="Greenwich" ,  "Hackney" =  "Hackney"  ,            
                                                                             "Hammersmith and Fulham" ="Hammersmith and Fulham" ,"Haringey"="Haringey" , "Harrow"= "Harrow", "Havering" = "Havering" ,"Hillingdon"="Hillingdon",  "Hounslow"= "Hounslow" , "Islington"= "Islington",  "Kensington and Chelsea"= "Kensington and Chelsea","Kingston upon Thames"= "Kingston upon Thames",   "Lambeth"="Lambeth", "Lewisham" ="Lewisham","Merton" ="Merton" , "Newham" = "Newham" , "Redbridge"="Redbridge" , "Richmond upon Thames"="Richmond upon Thames" ,  "Southwark" = "Southwark" ,            
                                                                            "Sutton"="Sutton",  "Tower Hamlets"= "Tower Hamlets" ,  "Waltham Forest"= "Waltham Forest" ,"Wandsworth"="Wandsworth",  "Westminster" = "Westminster" )),
                           selectInput("predictModel","Select smoother type:",c("Generalized Linear Model" = "glm","Linear model" = "lm","Loess" = "loess")),
                           box(width=12,background='maroon',valueBoxOutput("predBox",width = 12,height = "100px"))
)
)
)
)
)
)
)

# We'll save it in a variable ui
ui <- dashboardPage(skin = "black",
  dashboardHeader(title = "Crime data analysis London"),
  dashboardSidebar(disable  = TRUE),
  body
)

#we now define server function and save it in a variable called server
server <-  function(input, output) {
  
  # for displaying the right tab
  output$tabSelected <- renderText({input$tabset1})
  
  # to store the mouse click for map settings
  data_of_click <- reactiveValues(clickedMarker=NULL)
  
  
  # function to render leaflet 
  output$crimeMap <- renderLeaflet({
  data_new<-data[ which((data$major_category==input$crime)),] # subset data based on crime type
  pal <- colorNumeric(palette = "Reds",domain = data_new$total_crime) # define color pallete
  leaflet(data = data_new) %>% addProviderTiles(providers$Esri.NatGeoWorldMap)  %>% # generate leaflet map with bubble
      addCircleMarkers(~Latitude, ~Longitude, 
                 radius=~total_crime/500 , 
                 color = ~pal(total_crime),
                 fillOpacity = 0.8, layerId=~borough,popup=~borough,stroke = TRUE
                
                                       )
      
    
    
  })
  
  # function to store mouse click to be provided to bar chart
  observeEvent(input$crimeMap_marker_click,{
    data_of_click$clickedMarker <- input$crimeMap_marker_click
    
  })
  
  
# bar chart which displays data based on the click on a borough from the map
  output$plot1 <- renderPlot({
    name<-data_of_click$clickedMarker$id
    if(is.null(name)){name="Bexley"}
    data_b<-data[ which(data$borough==name & data$major_category == input$crime),]
    ggplot(data_b,aes_string(x=data_b$year,y=data_b$total_crime))+
      geom_bar(stat="identity",aes(color="blue",fill=TRUE))+
      xlab("Year")+
      ylab("Crime rate")+
      scale_x_continuous(breaks = 2008:2016)+
      scale_fill_brewer('set1')+
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            legend.position = "none")
    
    
        
  })

  
  #function to create prediction plots and give color depending to crime type
  output$prediction <- renderPlotly({ 
    
    if (input$predictCrime =="Burglary")
    {
      val="black"
    }
    else if (input$predictCrime =="Criminal Damage")
    {
      val="navy"
    }
    else if (input$predictCrime =="Drugs")
    {
      val="#660000"
    }
    else if (input$predictCrime =="Other Notifiable Offence")
    {
      val="#660033"
    }
    else if (input$predictCrime =="Robbery")
    {
      val="#003300"
    }
    else if (input$predictCrime =="Theft and Handling")
    {
      val="#663399"
    }
    else
    {
      val="#CC3300"
    }
    
    data_new<-data[ which((data$major_category==input$predictCrime) & (data$borough == input$predictBorough)),]
    p<-ggplot(data_new,aes(x=year,y=total_crime))+ 
      geom_point(color=val)+
      geom_smooth(method=input$predictModel,formula = y~x) 
    ggplotly(p)
  
    })
 
  
  # function to generate predictions using linear model
  output$predBox<- renderValueBox({
    pred_data<-data[ which((data$major_category==input$predictCrime) & (data$borough == input$predictBorough)),]
    model = lm(total_crime ~ year, data = pred_data) #Create the linear regression
    crimePred <- predict(model)
    valueBox(
      value = sprintf("Predicted crime rate : %f",crimePred[1])
      ,color = "green"
      
    )
    })  
  
  
  
  }

# Preview the UI in the console
shinyApp(ui = ui, server = server)