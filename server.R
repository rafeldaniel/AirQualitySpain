#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(leaflet)
library(ggplot2)

mydata<-read.csv(file = "data/dataairqualityspain.csv",stringsAsFactors = F)
mydata<-mydata%>%filter(Validity!="Not valid")


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

  #create list of available pollutants
  output$pollutantslist<-renderUI({

    if (which.station()=="ALL"){
      pollutants<-unique(mydata$Pollutant)
      selectInput("pollutant","Select Pollutant",pollutants)
      
    }else{
      pollutants<-mydata%>%filter(StationLocalId==which.station())
      pollutants<-unique(pollutants$Pollutant)
      selectInput("pollutant","Select Pollutant",pollutants)
    }
    
    })
  
  output$aggType<-renderUI({

    aggType<-unique(mydata.pollutant()$AggregationType)
    selectInput("aggType","1, Select Aggregation Type",aggType)
    
  })
  
  output$units<-renderUI({

    units<-unique(mydata.aggType()$Unit)
    selectInput("units","2. Select Units",units)
    
  })
  
  
  #filters data for pollutant only
  mydata.pollutant<-reactive({
    mydata.pollutant<-mydata%>%filter(Pollutant==input$pollutant)
    mydata.pollutant
  })
  
  #filters data for aggType only
  mydata.aggType<-reactive({
    mydata.aggType<-mydata.pollutant()%>%filter(AggregationType==input$aggType)
    mydata.aggType
  })
  
  #filter data reactive according to selected options
  mydata.map<-reactive({
    
    mydata.map<-mydata%>%filter(ReportingYear<=max(input$year),ReportingYear>=min(input$year),Pollutant==input$pollutant,
                                Unit==input$units,AggregationType==input$aggType)%>%
      group_by(StationLocalId)%>%summarise(Unit=first(Unit), 
                                           AggType=first(AggregationType),
                                           lat=mean(SamplingPoint_Latitude),
                                           lng=mean(SamplingPoint_Longitude),
                                           mean=mean(AQValue))
    
    mydata.map
    
  })
  
  #filter data reactive according to selected options
  mydata.years<-reactive({
    
    mydata.years<-mydata%>%filter(Pollutant==input$pollutant,
                                Unit==input$units,AggregationType==input$aggType)
    
    mydata.years
    
  })
  
  #which station
  which.station<-reactive({
    
    if (!is.null(input$mapPlot_marker_click)&!is.null(input$mapPlot_marker_click[[1]])){
      input$mapPlot_marker_click[[1]]
    }else{
      "ALL"
    }
  })
  
  output$unit<-renderText({
    mydata.map<-mydata.map()
    paste("Unit:",mydata.map$Unit)
    })
  
  output$AggType<-renderText(paste("Aggregation Type:",mydata.map()$AggType))
  
  output$mapPlot <- renderLeaflet({
    
    mydata.map<-mydata.map()
    
    pal<-colorNumeric(
        palette = c("green","red"),
        domain = mydata.map$mean
        )    
    
    map<-leaflet(data = mydata.map)%>%addTiles()%>%
      addCircleMarkers(color=~pal(mean),stroke=F,fillOpacity = 0.6,radius=7,label=~StationLocalId,
                       layerId =~StationLocalId )%>%
      addLegend(pal = pal,values = ~mean)
      
    if(which.station()!="ALL"){
      mystation<-mydata.map%>%filter(StationLocalId==which.station())%>%
        group_by(StationLocalId)%>%summarise(lat=first(lat),lng=first(lng))
      
      map<-map%>%addMarkers(data=mystation)
    }
    
    map
    
  })
  
  output$histPlot<-renderPlot({
    mydata.map<-mydata.years()
    mydata.map<-mydata.map%>%filter(ReportingYear>=min(input$year),ReportingYear<=max(input$year))
    
    myplot<-ggplot(data = mydata.map,aes(x=mydata.map$AQValue))+geom_histogram()+
      geom_vline(linetype=2,size=2,aes(color="mean",xintercept = mean(mydata.map$AQValue,na.rm = T)))+ 
      geom_vline(linetype=2,size=2,aes(color="median",xintercept = median(mydata.map$AQValue,na.rm = T)))+
      ggtitle(input$pollutant)+
      xlab(paste(input$pollutant,"in",input$units))
    
    if(which.station()!="ALL"){
      stdata<-mydata.map%>%filter(StationLocalId==which.station())

      myplot<-myplot+geom_vline(size=2,aes(color="Selected Station",xintercept = mean(stdata$AQValue,na.rm = T)))
    }
    myplot
  })
  
  output$stationid<-renderText({
    which.station()
  })
  
  output$boxyearPlot<-renderPlot({
    mydata.map<-mydata.years()
    
    myplot<-ggplot(data = mydata.map,aes(y=mydata.map$AQValue,x=ReportingYear))+
      geom_boxplot(aes(group=ReportingYear))+
      geom_smooth(method="lm", aes(color="Average"),size=2)+
      ggtitle(paste(input$pollutant,"Box Plot Tendancy"))+
      ylab(paste(input$pollutant,"in",input$units))+xlab("Reporting Year")
    
    if (which.station()!="ALL"){
      mystat<-mydata.map%>%filter(StationLocalId==which.station())
      myplot<-myplot+geom_line(data=mystat,aes(y=mystat$AQValue,x=ReportingYear,color="Selected Station"),linetype=2,size=2)
    }
    
    myplot
  })
})
