library(shiny)
library(maptools)
library(ggplot2)
library(rgdal)
library(broom)
library(raster)
library(readxl)
library(viridis)
library(reshape)

options(stringsAsFactors = F)

# Apro lo shpaefile
shp <- readOGR('shp')
shp@data$id <- rownames(shp@data)
colnames(shp@data) <- tolower(colnames(shp@data))
#shp2 <- shp

# Apro il file excel e unisco i dati allo shapefile
ds <- as.data.frame(read_excel(path = 'comuni.xlsx'))
colnames(ds) <- tolower(colnames(ds))
shp <- merge(shp, ds)

# Elimino le geometrie che non fanno parte del dataset 'ds'
shp <- subset(shp, !(is.na(shp@data$ds_prog)))

# Converto shp in un dataframe contenente tutti i dati disaggregati per comune
comuni <- fortify(shp)
comuni <- merge(comuni, shp)

# Dissolvo shp per circondario, distretto e provincia
# Creo una lista, 'lsp', che conterrÃ  i Large Spatial Polygons e una lista, 'df', che conterrÃ  i dataframe
lsp <- list()
df <- list()

lsp$province <- raster::aggregate(shp, 'ds_provincia')
lsp$province@data$id <- rownames(lsp$province@data)
df$province <- fortify(lsp$province)
df$province <- merge(df$province, lsp$province)

lsp$distretti <- raster::aggregate(shp, 'ds_distretto')
lsp$distretti@data$id <- rownames(lsp$distretti@data)
df$distretti <- fortify(lsp$distretti)
df$distretti <- merge(df$distretti, lsp$distretti)


#ui <- shinyUI(navbarPage("Navbar!", #theme =  'bootstrap.css',
ui <- tagList(
  
  
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Open+Sans');
      body {
        font-family: 'Open Sans';
      }
    "))
  ),
  
  navbarPage("Navbar!", #theme =  'bootstrap.css',
    tabPanel("Plot",
      sidebarLayout(
        sidebarPanel(width = 3,
          selectInput(
            inputId = "distretto",
            label = "Seleziona il distretto: ", 
            choices = unique(ds$ds_distretto),
            selected = ""
          ),
          uiOutput('comune'),
          plotOutput("uiMap")
        ),
        mainPanel(width = 9,
          plotOutput("map")
        )#, width="1000px", height="600px"))
      )
    ),
    tabPanel("Summary",
      verbatimTextOutput("summary")
    )
  )
)



server <- (function(input, output) {
  
  data <- reactive({
    data <- comuni[comuni$ds_distretto %in% (input$distretto),]
  })
  
  output$comune <- renderUI({
    selectInput(
      inputId = "comune",
      label = "Seleziona il comune: ", choices = c("", unique(data()[, "ds_den_mod"])),
      selected = ""
    )
  })

  output$uiMap <- renderPlot({
    ggplot() +
      theme_void() +
      coord_equal() +
      geom_polygon(data = df$province, mapping = aes(x=long, y=lat, group=group), fill=NA, color='black', size=0.05) +
      #geom_polygon(data = data(), mapping = aes(x=long, y=lat, group=group, fill=data()$ds_distretto), color='black', size=0.01) +
      geom_polygon(data = df$distretti[df$distretti$ds_distretto %in% input$distretto,], 
        mapping = aes(x=long, y=lat, group=group), fill=alpha('blue', 0.5)) +
      scale_fill_viridis(discrete = T) +
      theme(legend.position = 'none')
  })

  output$map <- renderPlot({
    ggplot() +
      theme_void() +
      coord_equal() +
      geom_polygon(data = data(), mapping = aes(x=long, y=lat, group=group, fill=ds_circondario), color='black', size=1) +
      geom_polygon(data = data()[data()$ds_den_mod %in% input$comune,], mapping = aes(x=long, y=lat, group=group), color='red', size=1, fill=NA) +
      scale_fill_viridis(discrete = T)
  })

})
shinyApp(ui = ui, server = server)

