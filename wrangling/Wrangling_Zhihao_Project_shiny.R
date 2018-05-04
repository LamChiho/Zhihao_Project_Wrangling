library(readr)
library(gapminder)
library(gutenbergr)
library(stringr)
library(ggplot2)
library(shiny)
library(tidyverse)
library(tidytext)
library(lubridate)
library(readxl)
library(maps)
library(rworldmap)
library(ggmap)
library(choroplethr)

load("~/shiny_environment.RData")
Age_bar<-c(10:50)
Area_list <- attacks1%>%
  mutate(n=1)%>%
  group_by(Area)%>%
  summarise(time=sum(n))%>%
  na.omit%>%arrange(desc(time))%>%
  .[1:20,]
Act_list <- attacks1%>%
  mutate(n=1)%>%
  group_by(Act)%>%
  summarise(time=sum(n))%>%
  na.omit()%>%
  arrange(desc(time))
# Define UI for application
ui <- fluidPage(
  
  titlePanel("Shark attacks"),
  
  # Sidebar Layout3
  sidebarLayout(
    
    # SelectInput for State and Type of data
    sidebarPanel(
      
      selectInput("Country", "Country",
                  choices = top_countries$Country,
                  selected = "USA"),
      selectInput("Specy", "Specy",
                  choices = specie_study$specy,
                  selected = "USA"),
      selectInput("Area", "Area",
                  choices = Area_list$Area,
                  selected = "California"),
      selectInput("Act", "Act",
                  choices = Act_list$Act,
                  selected = "swimming"),
      sliderInput(inputId = "bins",
                  label = "Slide to change the type of plot:",
                  min = 1,
                  max = 17,
                  value = 1),
      sliderInput(inputId = "Year",
                  label = "Year selection:",
                  min = 1800,
                  max = 2015,
                  value = c(1500,2015))
    ),
    
    
    mainPanel(plotOutput(outputId = "Plot"))
  )
)

# Define server logic required to draw a plot
server <- function(input, output) {
  
  reduced_df <- reactive({
    attacks%>%
      filter(input$Year[2]>=Year,Year>=input$Year[1])
  })
  
  output$Plot <- renderPlot({
    
    if(input$bins == 1){
      reduced_df()%>%
        filter(Country==input$Country)%>%
        group_by(Country,Year,International_Tourism)%>%
        mutate(n=1)%>%
        summarise(attack_of_country=sum(n))%>%
        na.omit()%>%
        mutate(rate=attack_of_country/International_Tourism)%>%
        ggplot(aes(Year, rate, colour = Country)) + geom_point() + geom_line() +
        ggtitle("Rate of attacked over years with tourists") +
        xlab("Year") + ylab("Number") }
    ##Attacks of different countries by months
    else if(input$bins == 2){
      reduced_df()%>%
        filter(Country==input$Country)%>%
        group_by(Month)%>%
        mutate(n=1)%>%
        summarise(attack_of_months=sum(n))%>%
        na.omit()%>%
        ggplot(aes(Month,attack_of_months,color = Month))+geom_density()+ggtitle("Shark attacks by months in different countries")
    }
    ##Attacks of different areas by months
    else if(input$bins == 3){
      reduced_df()%>%
        filter(Area==input$Area)%>%
        mutate(n=1)%>%
        group_by(Month)%>%
        summarise(attack_of_months=sum(n))%>%
        na.omit()%>%
        ggplot(aes(Month,attack_of_months,color = Month))+geom_density()+ggtitle("Shark attacks by months in different areas")
    }
    else if(input$bins == 4){
      temp1 <- geocode(input$Area)
      mp <- NULL
      mapWorld <- borders("world", colour="gray50", fill="gray50") 
      mp <- ggplot() +   mapWorld
      mp <- mp+ geom_point(aes(temp1$lon,temp1$lat,color=input$Area))+ggtitle("Location of this area")
      mp
    }
    ##Attacks of different areas by months
    else if(input$bins == 5){
      specie_study%>%
        .[1:5,]%>%
        ggplot(aes(specy,fatal_rate_specy,color = specy,size = attacks_of_specy))+geom_point()+ggtitle("The five deadliest sharks")  
    }
    else if(input$bins ==6){
      reduced_df()%>%
        filter(Year>=1900)%>%
        filter(Country==input$Country)%>%
        mutate(n=1)%>%
        group_by(Age)%>%
        filter(Age%in%Age_bar)%>%
        summarise(attack=sum(n))%>%
        ggplot(aes(Age,attack,color = Age))+geom_density()
    }
    else if(input$bins ==7){
      reduced_df()%>%
        filter(Area==input$Area)%>%
        group_by(Area,Year)%>%
        mutate(n=1)%>%
        summarise(death=sum(`Fatal (Y/N)`),time=sum(n))%>%
        na.omit()%>%
        mutate(rate=death/time)%>%
        ggplot(aes(Year, time, colour = Area)) + geom_point() + geom_line()  + ggtitle("Number of attacks over years in places") +
        xlab("Year") + ylab("Number of attack") +geom_smooth()
    }
    else if(input$bins ==8){
      reduced_df()%>%
        filter(Area==input$Area)%>%
        group_by(Area,Year)%>%
        mutate(n=1)%>%
        summarise(death=sum(`Fatal (Y/N)`),time=sum(n))%>%
        na.omit()%>%
        mutate(rate=death/time)%>%
        ggplot(aes(Year, death, colour = Area)) + geom_point() + geom_line() + ggtitle("Number of death over years in places") +
        xlab("Year") + ylab("Number of attack") +geom_smooth()
    }
    else if(input$bins ==9){
      reduced_df()%>%
        mutate(n=1)%>%
        group_by(Type)%>%
        summarise(Attack=sum(n))%>%
        na.omit()%>%
        ggplot(aes(Type,Attack,color=Type,size=Attack))+geom_point()+ggtitle("Type analysis")
    }
    else if(input$bins ==10){
      reduced_df()%>%
        filter(Country==input$Country)%>%
        group_by(Year)%>%
        mutate(n=`Fatal (Y/N)`)%>%
        mutate(t=1)%>%
        select(c(Year,n,t))%>%
        na.omit()%>%
        summarise(death=sum(n),attack=sum(t))%>%
        mutate(Fatal_Rate=death/attack)%>%
        ggplot(aes(Year,Fatal_Rate))+geom_point()+geom_line()+geom_smooth()+ggtitle("Death Rate since In this country by year")
    }
    else if(input$bins ==11){
      reduced_df()%>%
        filter(Area==input$Area)%>%
        group_by(Year)%>%
        mutate(n=`Fatal (Y/N)`)%>%
        mutate(t=1)%>%
        select(c(Year,n,t))%>%
        na.omit()%>%
        summarise(death=sum(n),attack=sum(t))%>%
        mutate(Fatal_Rate=death/attack)%>%
        ggplot(aes(Year,Fatal_Rate))+geom_point()+geom_line()+geom_smooth()+ggtitle("Death Rate In this Area")
    }
    else if(input$bins ==12){
      reduced_df()%>%
        filter(specy==input$Specy)%>%
        mutate(n=1)%>%
        group_by(Area)%>%
        summarise(attack_in_area=sum(n))%>%
        arrange(desc(attack_in_area))%>%
        na.omit()%>%
        .[1:5,]%>%
        na.omit()%>%
        ggplot(aes(Area,attack_in_area,color = Area, size = attack_in_area))+geom_point()+ggtitle("Areas attacked most frequently by this specy")
    }
    else if(input$bins ==13){
      reduced_df()%>%
        filter(Area==input$Area)%>%
        mutate(n=1)%>%
        group_by(specy)%>%
        summarise(attack_in_area=sum(n))%>%
        arrange(desc(attack_in_area))%>%
        na.omit()%>%
        .[1:5,]%>%
        na.omit()%>%
        ggplot(aes(specy,attack_in_area,color = specy, size = attack_in_area))+geom_point()+ggtitle("Sharks attack selected area most frequently")
    }
    else if(input$bins ==14){
      reduced_df()%>%
        filter(Act==input$Act)%>%
        mutate(n=1)%>%
        group_by(specy)%>%
        summarise(attack_this_act=sum(n))%>%
        arrange(desc(attack_this_act))%>%
        na.omit()%>%
        .[1:5,]%>%
        na.omit()%>%
        ggplot(aes(specy,attack_this_act,color = specy, size = attack_this_act))+geom_point()+ggtitle("Sharks attack selected as selected region")
    }
    else if(input$bins ==15){
      reduced_df()%>%
        filter(Act==input$Act)%>%
        mutate(n=1)%>%
        group_by(Area)%>%
        summarise(attack_in_area=sum(n))%>%
        arrange(desc(attack_in_area))%>%
        na.omit()%>%
        .[1:5,]%>%
        na.omit()%>%
        ggplot(aes(Area,attack_in_area,color = Area, size = attack_in_area))+geom_point()+ggtitle("Sharks attack selected area when act like this")
    }
    else if(input$bins ==16){
      reduced_df()%>%
        filter(Area==input$Area)%>%
        mutate(n=1)%>%
        group_by(Act)%>%
        summarise(attack_in_act=sum(n))%>%
        arrange(desc(attack_in_act))%>%
        na.omit()%>%
        ggplot(aes(Act,attack_in_act,color = Act, size = attack_in_act))+geom_point()+ggtitle("Sharks attack selected area when act like this")
    }
    else if(input$bins ==17){
      reduced_df()%>%
        filter(Country==input$Country)%>%
        select(Act,Country,`Fatal (Y/N)`)%>%
        mutate(n=1)%>%
        mutate(t=`Fatal (Y/N)`)%>%
        group_by(Act)%>%
        na.omit()%>%
        summarise(attack_in_act=sum(n),death_in_act=sum(t))%>%
        mutate(death_rate=death_in_act/attack_in_act)%>%
        ggplot(aes(Act,death_rate,color = Act,size = death_rate))+geom_point()+ggtitle("Sharks attack selected Country when act like this")
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

