#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(rio)
library(RColorBrewer)
#library(zipcodeR)
library(rpivotTable)
library(shinythemes)
library(leaflet)
#library(leaflet.extras)
#library(leaflet.extras2)
#library(sf)
#library(USAboundaries)
library(htmltools)


# Import data and manipulatin

mental_data <- read.csv("H:/Denis/Data_Analysis_Training/R - Studio/New/Mental_Health/survey_mental_health.csv")

mental_data %>% 
  summary()

mental_data <- df %>% 
  replace(.,is.na(.),"No Answer")  # Replaces null values with No answer.


df <- select(mental_data,
                      Gender_clean,Country,self_employed,family_history,Received_treatment,work_interfere,
                      remote_work,tech_company,benefits_provided,wellness_program,seek_help,anonymity,
                      coworkers_are_aware,supervisor_is_aware,mental_health_interview,phys_health_interview)

df1 <- df %>% 
  drop_na()   # Remove rows that contain missing values
  #filter(self_employed == "No")  # Only use data of respondent who are employed


df1 %>% 
  summary()

df %>% 
  summary()


  
 # Select values for Pivot table

pivotData <- select(df,
                    Gender_clean,
                    Country,
                    self_employed,
                    Received_treatment,
                    work_interfere,
                    remote_work,
                    tech_company)

pivotData1 <- select(df1,
                     Gender_clean,
                     Country,
                     self_employed,
                     Received_treatment,
                     work_interfere,
                     remote_work,
                     tech_company)

# Palettes for bar charts (remember to do them alphabetically)
# Yes, No, Don't know
seek_help = c("#0015BC", "#6F0B5E", "#DE0100")
# Female, Male, Missing, Queer, Transgender   
Gender_clean = c("#20377D", "#DD1A35", "#00674E", "#A80C30", "#77253A")

remote_work = c("#79001F", "#E8261F")
# Yes, No

tech_company = c("#D870AD","#4B89DC")
 
# Yes, No

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Mental Health Dashboard",
  theme = shinytheme("darkly"),
  
  tabPanel("Bar Charts",
           sidebarPanel(
             selectInput("select", label = h5("Please select your question"),
                         choices = list("benefits_provided","wellness_program","anonymity","coworkers_are_aware",
                                        "supervisor_is_aware","mental_health_interview","phys_health_interview"),
                         selected = "benefits_provided"),
             radioButtons("radio", label = h3("Select Variables"),
                          choices = list("Gender" = "Gender_clean", "Sought after Help" = "seek_help", "Work From Home" = "remote_work"),
                          selected = "Gender_clean"),
             
             
             
           ),
           
           mainPanel(
             plotOutput("plot1")
           )),
  
  
  tabPanel("Cross Tabulation",
           navlistPanel(
             tabPanel("Table with missing values", rpivotTableOutput("OverallPivot")),
             tabPanel("Table without missing Values", rpivotTableOutput("OverallPivot1"))
             
           )),
  

  
  
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
  output$plot1 <- renderPlot({
    ggplot(data = df, aes(get(input$select), fill = get(input$radio)))+
      geom_bar(
        stat = "count",
        aes(y=..count..),)+
      scale_fill_manual(name = input$radio,
                        values = get(input$radio))}+
      coord_flip()+
      xlab(input$select)
      )
  
  
  output$OverallPivot <- renderRpivotTable({
    rpivotTable(data = pivotData, cols = "wellness_program", rows = c("Gender_clean","Received_treatment"))
  })
  
 
   output$OverallPivot1 <- renderRpivotTable({
     rpivotTable(data = pivotData1, cols = "wellness_program", rows = c("Gender_clean","Received_treatment"))
   })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
