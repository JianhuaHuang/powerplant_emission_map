shinyUI(fluidPage(

    tags$head(
      includeCSS("styles.css"),
      includeScript("gomap.js")
      ),
    
    absolutePanel(id = "absPanel", class = "modal", fixed = TRUE, draggable = TRUE,
      top = 20, left = 'auto', right = 40, bottom = "auto",
      width = 220, height = "auto",
      
      h3("U.S. Powerplants"),
      selectInput('state', '', choices = c(US = 'US', var.states)),
      selectInput("emission", '', var.em, selected = "PLCO2AN")
    ),
    
    leafletMap("map", width="100%", height = 500,
      ## map id is from mapbox: https://www.mapbox.com/editor/?id=jianhua1122.j5f28jda#project
      initialTileLayer = "http://{s}.tiles.mapbox.com/v3/jianhua1122.j5f28jda/{z}/{x}/{y}.png",
      options=list(
        center = c(37.45, -93.85),
        zoom = 4, 
        opacity = 1)
    ),
  
  br(),br(),
  
  fluidRow(
    column(8, dataTableOutput("Table"),  style="overflow-y:scroll;height:400px;"),
    column(4, plotOutput('Plot1')),
    tags$style(type="text/css", '#Table tfoot {display:none;}')  ## remove the foot of "Table", and the horizontal scrollbar will disappear
  ),
  
  conditionalPanel("false", icon("globe"))
))
