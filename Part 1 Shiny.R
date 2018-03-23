#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(tidyverse)
library(stringr)
options(warn = -1)
##########################################################################
#Get ready for the cleaned dataset Noon_Data which we did in Part 1 R file
##########################################################################

url1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url2 <- ".txt.gz&dir=data/historical/stdmet/"

years <- c(1988:2017)

# We don't have the data for 2013, so we delete 2013 from the years list
years <- years[-26]

urls <- str_c(url1, years, url2, sep = "")
filenames <- str_c("mr", years, sep = "")

N <- length(urls)

for (i in 1:N){
  suppressMessages(assign(filenames[i], read_table(urls[i], col_names = TRUE)))
  file <- get(filenames[i])
  colnames(file)[1] <-"YYYY"
  
  # before 2005 the data don't have munite(mm) column, we add them back
  if (i <= 17) {file <- file %>% mutate(mm = "00")}
  
  # since 2007, the data has a redundant first row about unit, we delet them 
  if (i >= 20) {file <- file[-1,]}
  
  # we only need column about time and air & sea temperature
  file <- file %>% select(YYYY, MM, DD, hh, mm, ATMP, WTMP)
  
  # put '19' in front of 2 digit years
  if (i <= 11) {file[1] <- i + 1987}
  
  if(i == 1){Whole_Set <- file}
  else{Whole_Set <- rbind.data.frame(Whole_Set, file)}
  
}

# Before 2009, every time of record took place at mm = 00
# Since 2009, every time of record took place at mm = 50
# So the following is how we get the data that only recorded at noon from the whole data set

Noon_Data <- Whole_Set %>% filter((hh == "11" & mm == "50") | (hh == "12" & mm == "00"))

# Change sea temp and air temp to numeric form
Noon_Data$ATMP <- as.numeric(Noon_Data$ATMP)
Noon_Data$WTMP <- as.numeric(Noon_Data$WTMP)

# Convert all missing data (which is 999 or 99) into NA form
Noon_Data$ATMP <- ifelse(Noon_Data$ATMP > 90, NA, Noon_Data$ATMP)
Noon_Data$WTMP <- ifelse(Noon_Data$WTMP > 90, NA, Noon_Data$WTMP)

# Combine year, month, and day to one Data column
Noon_Data <- unite(Noon_Data, Date, YYYY, MM, DD, sep = "-")
Noon_Data$Date <-as.Date(Noon_Data$Date)

ui <- dashboardPage(
  dashboardHeader(title = "The Air & Sea Temperature detected by Buoy 46035"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Time Series", tabName = "time_series"),
      menuItem("Air & Sea Temp. Correlation", tabName = "correlation")
    )
  ),
  
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "time_series",
              fluidRow(
                box(selectInput("time_series_mode",
                                "Mode:",
                                choices = list("Air Temperature", "Sea Temperature",
                                               "Air and Sea Temperatures")), 
                    plotOutput("plot1"), width = 12)
              )
      ),
      
      tabItem(tabName = "correlation",
              fluidRow(
                box(selectInput("cor_mode",
                                "Mode:",
                                choices = list("Scatter Plot", "Smooth Line")), 
                    plotOutput("plot2"), width = 12)
              )
      )
      
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    
    if (input$time_series_mode == "Air Temperature") {
      
      graph <- ggplot(Noon_Data, aes(Date, ATMP)) + geom_line() +
        geom_line(col = "red") +
        labs(x = "Date (Shown Year Here)", y = "Temperature (Celcius Degree)",
             title = "Time Series of Air Temperature (Daily Noon Data)")
      print(graph)
    }
    
    if (input$time_series_mode == "Sea Temperature") {
      
      graph <- ggplot(Noon_Data, aes(Date, WTMP)) + geom_line() +
        geom_line(col = "blue") +
        labs(x = "Date (Shown Year Here)", y = "Temperature (Celcius Degree)",
             title = "Time Series of Sea Temperature (Daily Noon Data)")
      print(graph)
    }     
    
    if (input$time_series_mode == "Air and Sea Temperatures") {
      
      graph <- ggplot(Noon_Data, aes(Date)) + 
        geom_line(aes(y = ATMP, col = "ATMP")) + 
        geom_line(aes(y = WTMP, col = "WTMP")) +
        scale_colour_manual(values=c("red", "blue")) +
        labs(x = "Date (Shown Year Here)", y = "Temperature (Celcius Degree)",
             title = "Time Series of Air & Sea Temperature (Daily Noon Data)")
      print(graph)
    }     
    
  })
  
  output$plot2 <- renderPlot({
    if (input$cor_mode == "Scatter Plot") {
      
      graph <- ggplot(Noon_Data) + 
        geom_point(mapping = aes(x = ATMP, y = WTMP)) +
        labs(x = "Daily Noon Air Temp (Celcius)", 
             y = "Daily Noon Sea Temp (Celcius)",
             title = "Scatter Plot to See the Correlation between ATMP and WTMP")
      print(graph)
    }
    
    if (input$cor_mode == "Smooth Line") {
      
      graph <- ggplot(Noon_Data) + 
        geom_smooth(mapping = aes(x = ATMP, y = WTMP)) +
        labs(x = "Daily Noon Air Temp (Celcius)", 
             y = "Daily Noon Sea Temp (Celcius)",
             title = "Smooth Line to See the Correlation between ATMP and WTMP")
      print(graph)
    }
    
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

