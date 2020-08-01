# Import packages
library(shiny)

# Define UI for our application
shinyUI(navbarPage(
  
  # Application title
  title="European Union States",
  
  # Application id
  id = "mynav",
  
  # Tab layout for different components
  tabPanel("Introduction",
           
           # Introduction to the application
           fluidRow(
             column(8, offset = 2,
                    h1("EUROPEAN UNION STATES"),
                    hr(),
                    uiOutput("intro"),
                    hr()
             )
           ),
           
           # Display options
           fluidRow(
             column(8, offset = 2,
                    h2("Options"),
                    hr()
             ),
             column(7, offset = 2,
                    h4("Do you want to look at the summary:")
             ),
             column(3,
                    actionButton("show_summary", "Show")
             )
           ),
           fluidRow(
             column(7, offset = 2,
                    h4("Do you want to look at the details of a single state:")
             ),
             column(3,
                    actionButton("show_state_details", "Show")
             )
           ),
           fluidRow(
             column(7, offset = 2,
                    h4("Do you want to look at the plot(s) of the states:")
             ),
             column(3,
                    actionButton("show_plots", "Show")
             ),
             column(8, offset = 2,
                    hr()
             )
           )
  )
))