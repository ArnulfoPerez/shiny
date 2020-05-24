## app.R ##
library(shinydashboard)
library(shiny)
library(leaflet)
library(mxmaps)

data("df_mxstate")
df_mxstate$value <- df_mxstate$pop

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- dashboardPage(
    dashboardHeader(title = "Mexico Info Board"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Widgets", tabName = "widgets", icon = icon("th")),
            menuItem("leaflet", tabName = "leaflet", icon = icon("th")),
            menuItem("México", tabName = "México", icon = icon("th"))
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(plotOutput("plot1", height = 250)),
                        
                        box(
                            title = "Controls",
                            sliderInput("slider", "Number of observations:", 1, 100, 50)
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "widgets",
                    fluidRow(
                        
                        column(3, wellPanel(
                            selectInput("input_type", "Input type",
                                        c("slider", "text", "numeric", "checkbox",
                                          "checkboxGroup", "radioButtons", "selectInput",
                                          "selectInput (multi)", "date", "daterange"
                                        )
                            )
                        )),
                        
                        column(3, wellPanel(
                            # This outputs the dynamic UI component
                            uiOutput("ui")
                        )),
                        
                        column(3,
                               tags$p("Input type:"),
                               verbatimTextOutput("input_type_text"),
                               tags$p("Dynamic input value:"),
                               verbatimTextOutput("dynamic_value")
                        )
                    )
            ),
            
            # third tab content
            tabItem(tabName = "leaflet",
                    fluidPage(
                        leafletOutput("mymap"),
                        p(),
                        actionButton("recalc", "New points")
                    )
            ),
            
            # fourth tab content
            tabItem(tabName = "México",
                    fluidPage(
                        plotOutput("mexicoPop")
                    )
            )
        )
    )
)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    output$ui <- renderUI({
        if (is.null(input$input_type))
            return()
        
        # Depending on input$input_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$input_type,
               "slider" = sliderInput("dynamic", "Dynamic",
                                      min = 1, max = 20, value = 10),
               "text" = textInput("dynamic", "Dynamic",
                                  value = "starting value"),
               "numeric" =  numericInput("dynamic", "Dynamic",
                                         value = 12),
               "checkbox" = checkboxInput("dynamic", "Dynamic",
                                          value = TRUE),
               "checkboxGroup" = checkboxGroupInput("dynamic", "Dynamic",
                                                    choices = c("Option 1" = "option1",
                                                                "Option 2" = "option2"),
                                                    selected = "option2"
               ),
               "radioButtons" = radioButtons("dynamic", "Dynamic",
                                             choices = c("Option 1" = "option1",
                                                         "Option 2" = "option2"),
                                             selected = "option2"
               ),
               "selectInput" = selectInput("dynamic", "Dynamic",
                                           choices = c("Option 1" = "option1",
                                                       "Option 2" = "option2"),
                                           selected = "option2"
               ),
               "selectInput (multi)" = selectInput("dynamic", "Dynamic",
                                                   choices = c("Option 1" = "option1",
                                                               "Option 2" = "option2"),
                                                   selected = c("option1", "option2"),
                                                   multiple = TRUE
               ),
               "date" = dateInput("dynamic", "Dynamic"),
               "daterange" = dateRangeInput("dynamic", "Dynamic")
        )
    })
    
    output$input_type_text <- renderText({
        input$input_type
    })
    
    output$dynamic_value <- renderPrint({
        str(input$dynamic)
    })
    points <- eventReactive(input$recalc, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(data = points())
    })
    
    output$mexicoPop <- renderPlot({
        mxstate_choropleth(df_mxstate, title = "Total population, by state") 
    })
}

shinyApp(ui, server)
