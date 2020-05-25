## app.R ##
library(shinydashboard)

library(mxmaps)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(viridis)
library(scales)
options(stringsAsFactors = FALSE)

temp <- tempfile()
download.file("http://187.191.75.115/gobmx/salud/datos_abiertos/datos_abiertos_covid19.zip",temp)
files <- unzip(temp, list = TRUE)
df <- read_csv(unz(temp, arrange(files, Length)[1,]$Name))
unlink(temp)
date_cap <- as.Date(df$FECHA_ACTUALIZACION[1])
#ENTIDAD_FEDERATIVA
entidades <- c("Aguascalientes",
               "Baja California",
               "Baja California Sur",
               "Campeche",
               "Coahuila",
               "Colima",
               "Chiapas",
               "Chihuahua",
               "Ciudad de México",
               "Durango",
               "Guanajuato",
               "Guerrero",
               "Hidalgo",
               "Jalisco",
               "México",
               "Michoacán",
               "Morelos",
               "Nayarit",
               "Nuevo León",
               "Oaxaca",
               "Puebla",
               "Querétaro",
               "Quintana Roo",
               "San Luis Potosí",
               "Sinaloa",
               "Sonora",
               "Tabasco",
               "Tamaulipas",
               "Tlaxcala",
               "Veracruz",
               "Yucatán",
               "Zacatecas")

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
    dashboardHeader(title = "Mexico COVID-19"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            menuItem("Histogramas", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Read me", tabName = "widgets", icon = icon("th")),
            menuItem("México Population", tabName = "MéxicoPop", icon = icon("th")),
            menuItem("México Covid", tabName = "MéxicoCovid", icon = icon("th")),
            menuItem("Center Covid", tabName = "CenterCovid", icon = icon("th")),
            menuItem("Nuevo León Covid", tabName = "NLCovid", icon = icon("th")),
            selectInput("entity", "Entity",entidades),
            menuItem("Covid by federal entity", tabName = "EntityCovid", icon = icon("th"))
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
                            plotOutput("plot2", height = 250))
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "widgets",
                    includeMarkdown("README.md")
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
            ),
            
            # seventh tab content
            tabItem(tabName = "EntityCovid",
                    fluidPage(
                        plotOutput("entityCovid")
                    )
            )
        )
    )
)

server <- function(input, output) {
    
    output$plot1 <- renderPlot({
        hist(df$EDAD,main = "Histograma por edad",xlab = "Edad",ylab = "Frecuencia")
    })
    
    output$plot2 <- renderPlot({
        hist(df$SEXO,main = "Histograma por sexo",xlab = "Sexo",ylab = "Frecuencia")
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
    
    output$entityCovid <- renderPlot({
        mxmunicipio_choropleth(muns, 
                               num_colors = 1,
                               title = paste("COVID",input$entity,date_cap),
                               legend = "tasa por\n100 mil\nhabitantes",
                               zoom = subset(df_mxmunicipio, state_name ==input$entity)$region) +
            scale_fill_viridis("tasa por\n100 mil\nhabitantes",
                               trans = scales::pseudo_log_trans(sigma = 0.001)) +
            theme(legend.key.size = unit(2, "cm")) +
            theme(plot.title = element_text(size=30))
        #ggsave("graphs/map_centro_covid.png", dpi = 100, width = 16, height = 11)
        
    })
}

shinyApp(ui, server)
