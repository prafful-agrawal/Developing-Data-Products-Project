# Import packages
library(shiny)
library(plotly)
library(DT)

# Import the dataset
eu_df <- read.csv("eu_states.csv")

# Define server for our application
shinyServer(function(input, output, session) {
  
  # Set the active tabs
  tab_list <- c("Introduction")
  
  # Ouput intro
  intro <- includeMarkdown("intro.Rmd")
  output$intro <- renderUI({intro})
  
  # Toggle on the summary tab
  observeEvent(input$show_summary, {
    
    # Insert the tab only if not present in the list of active tabs
    if(!("Summary" %in% tab_list)) {
      insertTab("mynav",
                tabPanel("Summary",
                         fluidRow(
                           column(2, offset = 9,
                                  actionButton("hide_summary", "Hide Summary"))
                         ),
                         fluidRow(
                           column(8, offset = 2,
                                  hr(),
                                  h3("European Union States Summary"),
                                  dataTableOutput("summary")
                           )
                         )
                ),
                target = "Introduction",
                position = "after"
      )
      
      # Update the list of active tabs
      tab_list <<- c(tab_list, "Summary")
    }
  })
  
  # Switch to summary tab
  observeEvent(input$show_summary, {
    updateNavbarPage(session, "mynav",
                      selected = "Summary")
  })
  
  # Toggle off the summary tab
  observeEvent(input$hide_summary, {
    removeTab("mynav", "Summary")
    tab_list <<- tab_list[tab_list != "Summary"]
  })
  
  # Output summary table
  output$summary <- renderDataTable({
    
    # Format the links
    eu_df$Further_Reading <- sapply(eu_df$Links, function(x) {
      as.character(tags$a(href = x, "Click Here!"))
    })
    
    # Assign  the datatable
    datatable(eu_df[, c("State", "Code", "Capital", "Further_Reading")],  
              options = list(lengthMenu = c(10, 30), pageLength = 10),
              escape = FALSE,
              selection = "none")
  })
  
  # Toggle on the state details tab
  observeEvent(input$show_state_details, {
    
    # Insert the tab only if not present in the list of active tabs
    if(!("State Details" %in% tab_list)) {
      insertTab("mynav",
                tabPanel("State Details",
                         fluidRow(
                           column(2, offset = 9,
                                  actionButton("hide_state_details", "Hide State Details"))
                         ),
                         fluidRow(
                           column(8, offset = 2,
                                  hr(),
                                  selectInput("state", "Please select a State:", eu_df$State),
                                  hr(),
                                  h3("State Details"),
                                  dataTableOutput("state_details")
                           )
                         )
                         
                ),
                target = ifelse(any(tab_list == "Summary"), "Summary", "Introduction"),
                position = "after"
      )
      
      # Update the list of active tabs
      tab_list <<- c(tab_list, "State Details")
    }
  })
  
  # Switch to state details tab
  observeEvent(input$show_state_details, {
    updateNavbarPage(session, "mynav",
                     selected = "State Details")
  })
  
  # Toggle off the state details tab
  observeEvent(input$hide_state_details, {
    removeTab("mynav", "State Details")
    tab_list <<- tab_list[tab_list != "State Details"]
  })
  
  # Output state details table
  output$state_details <- renderDataTable({
    
    # Format the flag
    eu_df$Flag <- sapply(eu_df$Flag, function(x) {
      as.character(tags$img(src = x, width="40%"))
    })
    
    # Format the population 
    eu_df$Population <- paste0(round(eu_df$Population*1e-6, 2), " Million")
    
    # Format the area
    eu_df$Area <- paste0(round(eu_df$Area*1e-3, 2), " Thousand km&#178;")
    
    # Format the population density
    eu_df$Population_Density <- paste0(eu_df$Population_Density, " per km&#178;")
    
    # Format the Location
    eu_df$Location <- paste0(round(abs(eu_df$Latitude), 2),
                             ifelse(eu_df$Latitude > 0, " &#176;N, ", " &#176;S, "),
                             round(abs(eu_df$Longitude), 2),
                             ifelse(eu_df$Longitude > 0, " &#176;E", " &#176;W"))
    
    # Format the links
    eu_df$Further_Reading <- sapply(eu_df$Links, function(x) as.character(tags$a(href = x, "Click Here!")))
    
    # Assign the datatable
    datatable(t(eu_df[eu_df$State == input$state, c(10, 1:3, 11, 5:7, 12)]),
              rownames = TRUE,
              colnames = "",
              options = list(lengthMenu = 10, pageLength = 10),
              escape = FALSE,
              selection = "none")
  })
  
  # Toggle on the plots tab
  observeEvent(input$show_plots, {
    
    # Insert the tab only if not present in the list of active tabs
    if(!("Plots" %in% tab_list)) {
      appendTab("mynav",
                tabPanel("Plots",
                         fluidRow(
                           column(2, offset = 9,
                                  actionButton("hide_plots", "Hide Plots"))
                         ),
                         fluidRow(
                           column(8, offset = 2,
                                  hr(),
                                  checkboxGroupInput("plotvar", "Please select the Plots to display:",
                                                     c("w.r.t. Population" = "p",
                                                       "w.r.t. Area" = "a",
                                                       "w.r.t. Population Density" = "pd")),
                                  actionButton("update", "Update plots", icon = icon("refresh")),
                                  helpText("Please press the 'Update plots' button above to see the changes"),
                                  helpText("Please wait for a moment to let the plots render"),
                                  hr(),
                                  uiOutput("eu_plots")
                           )
                         )
                )
      )
      
      # Update the list of active tabs
      tab_list <<- c(tab_list, "Plots")
    }
  })
  
  # Switch to plots tab
  observeEvent(input$show_plots, {
    updateNavbarPage(session, "mynav",
                     selected = "Plots")
  })
  
  # Toggle off the plots tab
  observeEvent(input$hide_plots, {
    removeTab("mynav", "Plots")
    tab_list <<- tab_list[tab_list != "Plots"]
  })
  
  # Render UI for plots update
  output$eu_plots <- renderUI({
    plot_output_list()
  })
  
  
  plot_output_list <- eventReactive(input$update, {
    list(
      
      # Check if to show plot w.r.t. population
      if(any(input$plotvar == "p")) {
        list(plotlyOutput("plot_p"),
             hr()
        )
      },
      
      # Check if to show plot w.r.t. area
      if(any(input$plotvar == "a")) {
        list(plotlyOutput("plot_a"),
             hr()
        )
      },
      
      # Check if to show plot w.r.t. population density
      if(any(input$plotvar == "pd")) {
        list(plotlyOutput("plot_pd"),
             hr()
        )
      }
    )
  })
  
  # Output plot w.r.t population
  output$plot_p <- renderPlotly({
    
    # Format the popup
    eu_df$popup <- paste0("State: ", eu_df$State,
                          "<br>Capital: ", eu_df$Capital,
                          "<br>Population: ", round(eu_df$Population * 1e-6, 2),
                          " Millions</span></a>")
    
    # Initialize the plotly object
    plt_p <- plot_geo(data = eu_df,
                      type = "choropleth",
                      locations = ~Code,
                      z = sqrt(eu_df$Population),
                      text = ~popup,
                      hoverinfo = "text",
                      colorscale = "Blues",
                      reversescale = TRUE,
                      showscale = FALSE,
                      marker = list(line = list(color = toRGB("white")))
    )
    
    # Define layout options
    plt_p <-  plt_p %>% layout(title = "Europen Union States with respect to Population",
                               geo = list(resolution = 50,
                                          scope = "europe",
                                          projection = list(type = "conic conformal", scale = 1.5))
    )
    
    # Assign the plot
    expr = plt_p
  })
  
  # Plot w.r.t area
  output$plot_a <- renderPlotly({
    
    # Format the popup
    eu_df$popup <- paste0("State: ", eu_df$State,
                          "<br>Capital: ", eu_df$Capital,
                          "<br>Area: ", round(eu_df$Area * 1e-3, 2),
                          " Thousand km&#178;</span></a>")
    
    # Initialize the plotly object
    plt_a <- plot_geo(data = eu_df,
                      type = "choropleth",
                      locations = ~Code,
                      z = sqrt(eu_df$Area),
                      text = ~popup,
                      hoverinfo = "text",
                      colorscale = "Greens",
                      reversescale = TRUE,
                      showscale = FALSE,
                      marker = list(line = list(color = toRGB("white")))
    )
    
    # Define layout options
    plt_a <-  plt_a %>% layout(title = "Europen Union States with respect to Area",
                               geo = list(resolution = 50,
                                          scope = "europe",
                                          projection = list(type = "conic conformal", scale = 1.5))
    )
    
    # Assign the plot
    expr = plt_a
  })
  
  # Plot w.r.t population density
  output$plot_pd <- renderPlotly({
    
    # Format the popup
    eu_df$popup <- paste0("State: ", eu_df$State,
                          "<br>Capital: ", eu_df$Capital,
                          "<br>Population Density: ", eu_df$Population_Density,
                          " per km&#178;</span></a>")
    
    # Initialize the plotly object
    plt_pd <- plot_geo(data = eu_df,
                       type = "choropleth",
                       locations = ~Code,
                       z = log(eu_df$Population_Density),
                       text = ~popup,
                       hoverinfo = "text",
                       colorscale = "YlOrRd",
                       reversescale = TRUE,
                       showscale = FALSE,
                       marker = list(line = list(color = toRGB("white")))
    )
    
    # Define layout options
    plt_pd <-  plt_pd %>% layout(title = "Europen Union States with respect to Population Density",
                                 geo = list(resolution = 50,
                                            scope = "europe",
                                            projection = list(type = "conic conformal", scale = 1.5))
    )
    
    # Assign the plot
    expr = plt_pd
  })
})