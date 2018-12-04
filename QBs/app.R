library(shiny)
library(haven)
library(sjlabelled)
library(rsconnect)
library(shinythemes)
#library(plotly)
library(knitr)
library(scales)
library(stargazer)
library(ggrepel)
library(tidyverse)

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
                                        
                                        br() 
                                        
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
                    } else if(imput$y_axis == "interceptions"){
                      y_label <- "Interceptions"
                    } else if(imput$y_axis == "Passing_Yards"){
                      y_label <- "Passing Yards"
                    }
                    })
                  
                  output$plot <- renderPlot({
                    
                      Qbs %>%
                        ggplot(aes_string(x = input$x_axis, y = input$y_axis, color = "passer_player_name")) +
                        geom_point() +
                        geom_smooth(method = "lm", se = FALSE) + 
                        geom_label_repel(aes(label = year)) +
                        labs(x = x_label(),
                             y = y_label(),
                             title = "QB Preformance in Relation to OLine Play",
                             subtitle = " ",
                             caption = "Data taken from Kaggle*")
                    
                  })
                  
                  }

# Create Shiny app
shinyApp(ui, server)
