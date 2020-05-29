
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
library(shinyBS)

#removes the authorization for the Google Sheet
sheets_deauth()

#using googlesheets4 to API data from Google Sheet into R
sources <- read_sheet("1crmXg4Rrth7xpxxgDY6cstRlZYIGa6EOPblp8XFbCGo", sheet = "sources")


#remove all sources that have not yet been extracted
sources <- filter(sources, status == "finished")

#convert date strings to dates
sources$date <- ymd(sources$date)

#remove all NA that are remaining... this should be zero if we input the data well
sources <- sources %>% 
    filter(!is.na(date)) %>% 
    filter(!is.na(country)) %>%
    filter(!is.na(title))

#select just the columns are that needed
sources <- select(sources, category, country, author, date, publication, title, country, topics, age, url)

sources$country <- str_to_lower(sources$country) 
sources$topics <- str_to_lower(sources$topics)
sources$age <- str_to_lower(sources$age)

#this should just be country names after cleaning up the data as well
sources_location <- sources %>% 
    count(country)

#list of categories
sources_cat <- sources %>%
    group_by(topics) %>% 
    distinct(topics)

sources_cat <- unique(unlist(strsplit(sources_cat$topics, ",")))

sources_cat <- sources_cat %>% 
    trimws() %>% 
    str_to_lower() %>% 
    unique()

#list of ages
sources_age <- sources %>%
    group_by(age) %>% 
    distinct(age)

sources_age <- unique(unlist(strsplit(sources_age$age, ",")))

sources_age <- sources_age %>% 
    trimws() %>% 
    str_to_lower() %>% 
    unique()

#list of countries
sources_count<- sources %>%
    group_by(country) %>% 
    distinct(country)

sources_count <- unique(unlist(strsplit(sources_count$country, ",")))

sources_count <- sources_count %>% 
    trimws() %>% 
    str_to_lower() %>% 
    unique()

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

<h4><b>Search for peer-reviewed, preprint and instituion publications</b> <br> </br>
This is a database of publications that have been reported and publicly released since early February. You 
can search the database by topic, date, and country. Our team has extracted the data contained
in these papers and a method for data sharing will be coming soon.</br> </br>
</h4>")
                                          
                                      ),
                                      box(selectInput("category", "Select Category", sources_cat, selected = NULL, multiple = TRUE),
                                          selectInput("age", "Select Age Group", sources_age, selected = NULL, multiple = TRUE),
                                          selectInput("country", "Select Country", sources_count, selected = NULL, multiple = TRUE)),
                                      
                                      
                                      box(
                                          
                                          bsCollapse(id = "topic_definitions", open = "Panel 2",
                                                     bsCollapsePanel("Transmission", "This topic includes information on transmission dynamics including infectious period, 
                                                                     incubation period, serial interval, basic reproduction number, asymptomatic and prodromal transmission, 
                                                                     duration of hospital admission, time from onset to hospital admission, time from onset to quarantine,
                                                                     time from onset to isolation and time from onset to death.", style = "info"),
                                                     
                                                     bsCollapsePanel("Severity", "There is a wide range of illness severity. This topic includes data on critical care, 
                                                                     including mechanical ventilation and ICU admission, as well as data about the severity of the disease 
                                                                     stratified by characteristics such as age or comorbidities.", style = "info"),
                                                     
                                                     bsCollapsePanel("Risk Groups", "It is important to understand risk factors for COVID-19 infection and severe infection.
                                                                     We want to understand the dynamics of COVID-19 in groups with particular characteristics. Data reported
                                                                     is stratified by patient characteristics, age, and comorbidities. Currently, we are focusing on vulnerable 
                                                                     groups—outbreaks in long-term care or retirement homes, outbreaks in prisons, and outcomes in children..", style = "info"),
                                                     
                                                     bsCollapsePanel("Symptoms", "We’re interested in understanding what symptoms COVID-19 causes, from the most common symptoms to
                                                                     the uncommon ones. We report on the number of patients in each study with the symptom and the percentage of
                                                                     patients with that symptoms.", style = "info"),
                                                     
                                                     bsCollapsePanel("Diagnostics", "This topic includes data on the sensitivity and specificity of different diagnostic tools,
                                                                     including RT-PCR from nasopharyngeal, throat and stool swabs, chest CT findings, and serology for IgM 
                                                                     antibodies. ", style = "info"),
                                                     
                                                     bsCollapsePanel("Viral Load", "This topic includes reverse CT values and viral RNA copies.", style = "info"),
                                                     
                                                     bsCollapsePanel("Health Services", "Social distancing has been implemented to reduce the burden on health services. This 
                                                                     topic incorporates data on health capacity and health services utilization, including surge capacity, ICU capacity,
                                                                     ICU occupancy, and health workforce.", style = "info"),
                                                     
                                                     bsCollapsePanel("Super-Spreading", "Super-spreading, defined here as one primary case infecting four or more others during a clearly defined
                                                                     event, has been documented globally. The date, location and duration of event, type of event, setting, number
                                                                     of attendees, number of secondary cases, and the secondary attack rate are reported.", style = "info")
                                                     )),
                                      
                                    
                                      ),
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
        country_input <- input$country
        age_select <- input$age
        input1 <- input1
        input2 <- input2
        
        if(all(!is.null(country_input))) {
        
        sources <- sources %>% 
            filter(
                grepl(country_input[1], country) |
                    grepl(country_input[2], country) | 
                    grepl(country_input[3], country) |
                    grepl(country_input[4], country)
            )
        }
        
        if(all(!is.null(age_select))) {
            
        sources <- sources %>% 
            filter(
                grepl(age_select[1], age) |
                    grepl(age_select[2], age) | 
                    grepl(age_select[3], age)
            )
        }
        
        if(all(!is.null(cat))) {
            
        sources <- sources %>% 
            filter(
                grepl(cat[1], topics) |
                    grepl(cat[2], topics) | 
                    grepl(cat[3], topics) |
                    grepl(cat[4], topics)
            )
        }
        
        #this output table only appears if there is a country and a topic selected. If not, it has an error. One must chose a country or a topic
        datatable(sources, extensions = 'Buttons', options = list(
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ))
        
        
    })
    
}

shinyApp(ui = ui, server = server)
