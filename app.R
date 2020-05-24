## app.R ##
library(shinydashboard)
library(shiny)
library(leaflet)
library(mxmaps)

## Auto-Install the following packages
.packs <- c("tidyverse", "lubridate", "ggrepel", "viridis", "scales")
.success <- suppressWarnings(sapply(.packs, require, character.only = TRUE))
if (length(names(.success)[!.success])) {
    install.packages(names(.success)[!.success])
    sapply(names(.success)[!.success], require, character.only = TRUE)
}
if (!require(mxmaps))
    devtools::install_github("diegovalle/mxmaps")
library(mxmaps)
options(stringsAsFactors = FALSE)

temp <- tempfile()
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",temp)
files <- unzip(temp, list = TRUE)
df <- read_csv(unz(temp, arrange(files, Length)[1,]$Name))
unlink(temp)
date_cap <- as.Date(df$FECHA_ACTUALIZACION[1])

pos <- filter(df, RESULTADO == 1)

pos$region <- str_mxmunicipio(pos$ENTIDAD_RES, pos$MUNICIPIO_RES)
muns <- pos %>%
    group_by(region) %>%
    tally()
muns <- left_join(muns, df_mxmunicipio, by = "region")
muns$value2 <-  muns$n / muns$pop * 10^5
muns$value <- if_else(muns$value2 > 400, 400, muns$value2)

muns$name <- paste(muns$state_name, muns$municipio_name)
cities <- filter(muns, name %in% c("Coahuila Monclova",
                                   "Baja California Sur Los Cabos",
                                   "Ciudad de México Cuajimalpa de Morelos",
                                   "Quintana Roo Benito Juárez",
                                   "Sinaloa Culiacán",
                                   "Tabasco Centro",
                                   "Baja California Mexicali",
                                   "Yucatán Mérida",
                                   "Puebla Puebla",
                                   "Nuevo León Monterrey",
                                   "Chihuahua Juárez",
                                   "Michoacán Lázaro Cárdenas",
                                   "Oaxaca Santa María Huatulco",
                                   "Colima Manzanillo",
                                   "Jalisco Guadalajara"))
cities$municipio_name <- str_replace(cities$municipio_name, 
                                     "Cuajimalpa de Morelos", 
                                     "Cuajimalpa")
cities$municipio_name <- str_replace(cities$municipio_name, 
                                     "Santa María Huatulco", 
                                     "Huatulco")
cities$municipio_name <- str_replace(cities$municipio_name, 
                                     "Benito Juárez", 
                                     "Cancún")
cities$municipio_name <- str_replace(cities$municipio_name, 
                                     "Juárez", 
                                     "Ciudad Juárez")
cities$municipio_name <- str_replace(cities$municipio_name, 
                                     "Centro", 
                                     "Villahermosa")
cities$group <- NA

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
            menuItem("México Pop.", tabName = "MéxicoPop", icon = icon("th")),
            menuItem("México Covid", tabName = "MéxicoCovid", icon = icon("th")),
            menuItem("Center Covid", tabName = "CenterCovid", icon = icon("th")),
            menuItem("Nuevo León Covid", tabName = "NLCovid", icon = icon("th"))
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
            tabItem(tabName = "MéxicoPop",
                    fluidPage(
                        plotOutput("mexicoPop")
                    )
            ),
            
            # fith tab content
            tabItem(tabName = "MéxicoCovid",
                    fluidPage(
                        plotOutput("mexicoCovid")
                    )
            ),
            
            # fith tab content
            tabItem(tabName = "CenterCovid",
                    fluidPage(
                        plotOutput("centerCovid")
                    )
            ),
            
            # sixth tab content
            tabItem(tabName = "NLCovid",
                    fluidPage(
                        plotOutput("nlCovid")
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
    
    output$mexicoCovid <- renderPlot({
        mxmunicipio_choropleth(muns, 
                               num_colors = 1,
                               title = paste("COVID-19 per county",date_cap),
                               legend = "tasa por\n100 mil\nhabitantes") +
            scale_fill_viridis("tasa por\n100 mil\nhabitantes",
                               trans = scales::pseudo_log_trans(sigma = 0.001)) + 
            geom_label_repel(data = cities, aes(long, lat, label = municipio_name), 
                             size = 3,
                             force = .1, alpha = .8,
                             box.padding = 3.3, label.padding = 0.18) +
            theme(legend.key.size = unit(2, "cm")) +
            theme(plot.title = element_text(size=32))
        #ggsave("graphs/map_covid.png", dpi = 100, width = 19, height = 14)
        
    })
    
    output$centerCovid <- renderPlot({
        mxmunicipio_choropleth(muns, 
                               num_colors = 1,
                               title = paste("COVID-19 central México",date_cap),
                               legend = "tasa por\n100 mil\nhabitantes",
                               zoom = subset(df_mxmunicipio, state_name %in% c("Ciudad de México",
                                                                               "Puebla",
                                                                               "Morelos",
                                                                               "México",
                                                                               "Hidalgo",
                                                                               "Tlaxcala"))$region) +
            scale_fill_viridis("tasa por\n100 mil\nhabitantes",
                               trans = scales::pseudo_log_trans(sigma = 0.001)) +
            theme(legend.key.size = unit(2, "cm")) +
            theme(plot.title = element_text(size=30))
        #ggsave("graphs/map_centro_covid.png", dpi = 100, width = 16, height = 11)
        
    })
    
    output$nlCovid <- renderPlot({
        mxmunicipio_choropleth(muns, 
                               num_colors = 1,
                               title = paste("COVID Nuevo León",date_cap),
                               legend = "tasa por\n100 mil\nhabitantes",
                               zoom = subset(df_mxmunicipio, state_name %in% c("Nuevo León"))$region) +
            scale_fill_viridis("tasa por\n100 mil\nhabitantes",
                               trans = scales::pseudo_log_trans(sigma = 0.001)) +
            theme(legend.key.size = unit(2, "cm")) +
            theme(plot.title = element_text(size=30))
        #ggsave("graphs/map_centro_covid.png", dpi = 100, width = 16, height = 11)
        
    })
}

shinyApp(ui, server)
