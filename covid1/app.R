
library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(stringr)
library(googlesheets4)
library(lubridate)
library(rsconnect)

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

#list of categories
sources_cat <- sources %>%
    group_by(category) %>% 
    distinct(category)

#list of countries
sources_count<- sources %>%
    group_by(population_location) %>% 
    distinct(population_location)

#date_variable
input1 <- 0
input2 <- 0

theme_set(theme_bw())

ui<-dashboardPage(skin = "blue",
                  dashboardHeader(title = "LSHTM Rapid Review"),
                  
                  #### Sidebar Content
                  sidebar <- dashboardSidebar(
                      hr(),
                      sidebarMenu(id = "tabs",
                                  menuItem("Sources", tabName = "sources", icon = icon("file-text-o")),
                                  menuItem("Super Spreading Events", tabName = "super", icon = icon("calendar")),
                                  menuItem("Health Services", tabName = "Health Services", icon = icon("line-chart"))
                      ),
                      hr()
                  ),
                  
                  
                  #### Dashboard Content
                  
                  body <- dashboardBody(
                      tabItems(
                          tabItem(tabName = "sources",
                                  fluidPage(
                                      box(width = NULL,
                                          HTML("<br/> <h3><b>COVID-19 SOURCES</b></h3> <br/>

<h4><b>Search for Peer-reviwed, preprint and instituion publications</b> <br> </br>
This is a database of publications that have been reported and publicly released since early February. You 
can search the database by priority, date, and country. Our team has extracted the data contained
in these papers and a method for data sharing will be coming soon.</br> </br>
</h4>")
                                          
                                      ),
                                      box(selectInput("category", "Select Category", sources_cat, selected = "", multiple = TRUE),
                                          selectInput("country", "Select Country", sources_count, selected = "", multiple = TRUE)),
                                      box(dateInput(input1, label = "Earliest Date", value = "", min = NULL, max = NULL,
                                                    format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                    language = "en", width = NULL, autoclose = TRUE,
                                                    datesdisabled = NULL, daysofweekdisabled = NULL),
                                          dateInput(input2, label = "Latest Date", value = "", min = NULL, max = NULL,
                                                    format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                    language = "en", width = NULL, autoclose = TRUE,
                                                    datesdisabled = NULL, daysofweekdisabled = NULL))),
                                  box(width = NULL, dataTableOutput('table'))
                          ),
                          
                          tabItem(tabName = "super",
                                  fluidRow(
                                      box(
                                          HTML("<br/> <h3><b>Super Spreading Events</b></h3> <br/>

<h4><b>High-Level Results:</b> This rapid-review collected data on super spreading events (SSEs) of COVID-19. 
Overall data quality was low, however six high quality SSEs were identified. 
Half of these events were social gatherings where meals were shared. 
Across all sources, the most common event types include, social gatherings, medical settings, and workplace events. </br> </br>
</h4>")
                                          
                                      ))),
                          tabItem("Health Services")
                      )
                  ))




#### Server Code
server <- function(input, output){
    
    output$table <- DT::renderDataTable({
        
        cat <- input$category
        country <- input$country
        input1 <- input1
        input2 <- input2
        
        fsources <- sources %>% 
                filter(population_location == country[1] | population_location == country[2] |
                           population_location == country[3] | population_location == country[4])
        
        fsources <- fsources %>% 
            filter(category == cat[1] | category == cat[2] |
                       category == cat[3] | category == cat[4])
        
    
        
        fsources
        
    })
    
}

shinyApp(ui = ui, server = server)
