
library(shiny)
library(shinydashboard)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)
library(googlesheets4)
library(lubridate)

#removes the authorization for the Google Sheet
sheets_deauth()

#using googlesheets4 to API data from Google Sheet into R
sources <- read_sheet("1crmXg4Rrth7xpxxgDY6cstRlZYIGa6EOPblp8XFbCGo", sheet = "sources")

#select just the columns are that needed
sources <- select(sources, status, category, author, date, publication, title, population_location)

#remove all sources that have not yet been extracted
sources <- filter(sources, status == "finished")

#convert date strings to dates
sources$date <- ymd(sources$date)

#remove all NA that are remaining... this should be zero if we input the data well
sources <- sources %>% 
    filter(!is.na(date)) %>% 
    filter(!is.na(population_location)) %>%
    filter(!is.na(title))

#this should just be country names after cleaning up the data as well
sources_location <- sources %>% 
    count(population_location)



theme_set(theme_bw())

ui<-dashboardPage(skin = "blue",
                  dashboardHeader(title = "LSHTM Rapid Review Group"),
                  
                  #### Sidebar Content
                  dashboardSidebar(
                      sidebarMenu(
                          menuItem("Sources Search", tabName = "sources", icon = icon("fa-file-text-o"),
                          menuItem("Severity", tabName = "Severity"),
                          menuItem("Health Services", tabName = "Health Services"))),
                  
                  #### Dashboard Content
                  
                  dashboardBody(
                      tabItem("Sources Search",
                              fluidRow(
                                  box(
                                      HTML("<br/> <h3><b>Super Spreading Events</b></h3> <br/>"),
                                      
                                  ))),
                      tabItem("Severity"),
                      tabItem("Health Services"))))



#### Server Code
server <- function(input, output){
    
    #output$SSE_T <- renderPlotly({
    #    ggplotly(
    #        ggplot(SuperSpreader, aes(x=event))+
    #            geom_bar(aes(fct_infreq(event)))+
    #            labs(title = "Figure 1. Frequency of Super Spreader Events by Type", y= "Count", x="Event Type")+
    #            theme(axis.text.x = element_text(angle = 45, hjust = 1)))})
    
    
    
}

shinyApp(ui = ui, server = server)
