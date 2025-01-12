library(shiny)
library(haven)
library(sjlabelled)
library(rsconnect)
library(shinythemes)
library(knitr)
library(scales)
library(stargazer)
library(ggrepel)
library(tidyverse)

# Use the first read command with filepath when troubleshooting
#Qbs <- read_rds("~/Desktop/Data/Final/QBs/QBs.rds")
Qbs <- read_rds("QBs.rds")



ui <- fluidPage(theme = shinytheme("flatly"),
                
                navbarPage("NFL Quarterback Analysis",
                           
                           tabPanel("Graph",
                                    # Sidebar layout with input and output definitions
                                    sidebarLayout(
                                      # Sidebar panel for inputs
                                      sidebarPanel(
                                        # input choices for x-axis variables
                                        selectInput("x_axis",
                                                    "OLine Statistic",
                                                    choices = c("Sacks" = "Sacks", 
                                                                "Hits" = "Hits")), 
                                        tags$h6(helpText("These statistics are considered to be a measure of good OLine play. The less sacks/hits an OLine gives up the better they are.")),
                                        
                                        br(),
                                        
                                        selectInput("y_axis",
                                                    "Quarterback Statistic",
                                                    choices = c("Completion Percentage" = "comp_pct", 
                                                                "Touchdowns" = "touchdowns", 
                                                                "Interceptions" = "interceptions", 
                                                                "Passing Yards" = "Passing_Yards")), 
                                        tags$h6(helpText("These statistics are considered to be a measure for good QB play. The more touchdowns, yards, and completions a QB has the better he is.")), 
                                        
                                        br(), 
                                        
                                        selectInput("year", 
                                                    "Year", 
                                                    choices = c("All","2009", "2010", "2011", "2012", 
                                                                "2013", "2014", "2015", "2016", 
                                                                "2017"), 
                                                    selected = "All")
                                      ),
                                        
                                      mainPanel(
                                        plotOutput("plot"),
                                         htmlOutput("summary")
                                        )
                                      )
                                    )
                           )
                )
                
                
                # Define server logic for random distribution app ----
                server <- function(input, output) {
                  
                  # create function to reactively change x-axis label
                  x_label <- reactive({
                    req(input$x_axis)
                    if(input$x_axis == "Sacks"){
                      x_label <- "Sacks"
                    } else if(input$x_axis == "Hits"){
                      x_label <- "Hits"
                    }})
                  
                  # create function to reactively change y-axis label
                  y_label <- reactive({
                    req(input$y_axis)
                    if(input$y_axis == "comp_pct"){
                      y_label <- "Completion Percentage"
                    } else if(input$y_axis == "touchdowns"){
                      y_label <- "Touchdowns"
                    } else if(input$y_axis == "interceptions"){
                      y_label <- "Interceptions"
                    } else if(input$y_axis == "Passing_Yards"){
                      y_label <- "Passing Yards"
                    }
                    })
                  
                  output$plot <- renderPlot({
                    if (input$year != "All"){Qbs <- Qbs %>% filter(year == input$year)} #Want default to be all years but to have option of individual year
                      Qbs %>%
                        ggplot(aes_string(x = input$x_axis, y = input$y_axis, color = "passer_player_name")) +
                        geom_point() +
                        geom_smooth(method = "lm", se = FALSE) + 
                        geom_label_repel(aes(label = year)) +
                        labs(x = x_label(),
                             y = y_label(),
                             title = "QB Performance in Relation to OLine Play",
                             subtitle = "Some metrics show stronger relationships than others ",
                             caption = "Data taken from Kaggle")
                    
                  })
                  
                  }

# Create Shiny app
shinyApp(ui, server)
