# powMapClicked is the powerplant being clicked on map, and the powTableClicked is the powerplant being clicked (goMap button) in the table
# Their value will be passed to the reactive binding variable powSelected, which is used to control the emission plot and map popup

shinyServer(function(input, output, session) {
  dispCol <- reactive(c(phy.info, input$emission, gsub('AN', 'RTA', input$emission), 'Go'))
    
  makeReactiveBinding('powSelected')
  makeReactiveBinding('em.plot1')
  
  # Create the map; this is not the "real" map, but rather a proxy
  # object that lets us control the leaflet map on the page.
  map <- createLeafletMap(session, 'map')
    
  observe({
    if (is.null(input$map_click)) return()
    powSelected <<- NULL
  })
  
  observe({
    map$clearShapes()
    
    map$addCircle(
      pows$LAT, pows$LON,
      radius = ((pows[[input$emission]] - min(pows[[input$emission]])) / max(pows[[input$emission]]) + .05) * 10^6.5 / max(5, input$map_zoom)^2,
      layerId= row.names(pows),
      list(weight= 1, fill=TRUE, color= power.categ$COLORS))
  })
  
  state.selected <-  reactive(states[states$Abbreviation == input$state, ])
  observe(if (nrow(state.selected()) == 0) {
    map$setView(lat = 37.45, lng = -93.85, zoom = 4)} else
      map$setView(lat = state.selected()$latitude, lng = state.selected()$longitude, zoom = 7))  
  
  pows.state <- reactive(if(input$state == 'US') NULL else pows[pows$PSTATABB == input$state, ])

  ## observe the activities on dataTable
  observe({
    map$clearPopups()
    
    powTableClicked <- pows[pows$ORISPL == input$mydata, ]

    isolate({
      content <- as.character(tagList(
        tags$strong(paste(powTableClicked$PNAME)),
        br(),
        paste0(powTableClicked$CNTYNAME,', ', powTableClicked$PSTATABB),
        br(),
        paste('Primary fuel:', power.categ[as.integer(row.names(powTableClicked)), 'FUELPOPUP']),
        br(),
        paste("Annual Generation:", powTableClicked[['PLNGENAN']], '(MWh)'),
        br(),
        paste("Estimated", gsub('PL|AN', '', input$emission), ":", round(powTableClicked[[input$emission]]), '(tons)')
      ))
      
      if (nrow(powTableClicked) > 0) map$setView(powTableClicked$LAT, powTableClicked$LON, 8)
      if (nrow(powTableClicked) > 0) map$showPopup(powTableClicked$LAT, powTableClicked$LON, content)      
    })   
   ## at the initialization, powTableClicked is of length zero. So don't give its value to powSelected at the beginning
#     if(nrow(powTableClicked) > 0) powSelected <<- powTableClicked
    
  })
  
    ## observe the activities on map
    observe({
    event <- input$map_shape_click
    if (is.null(event)) return()
      
    map$clearPopups()

    powMapClicked <- pows[row.names(pows) == event$id, ]
    
    powSelected <<- powMapClicked
    
    isolate({
      content <- as.character(tagList(
        tags$strong(paste(powMapClicked$PNAME)),
        br(),
        paste0(powMapClicked$CNTYNAME,', ', powMapClicked$PSTATABB),
        br(),
        paste('Primary fuel:', power.categ[as.integer(row.names(powMapClicked)), 'FUELPOPUP']),
        br(),
        paste("Annual Generation:", powMapClicked[['PLNGENAN']], '(MWh)'),
        br(),
        paste("Estimated", gsub('PL|AN', '', input$emission), ":", round(powMapClicked[[input$emission]]), '(tons)')
      ))
      
      map$showPopup(powMapClicked$LAT, powMapClicked$LON, content)      
    })

  })
  
  
  output$Table <- renderDataTable(
    if (!is.null(powSelected)) {powSelected[, dispCol()]} else {
      if(input$state == 'US') {pows[, dispCol()]} else {      pows.state()[, dispCol()]
      } 
    }, options = list(iDisplayLength = 50))
  
    
  ## observe the change of powSelected and then update the emission plot
  observe({    
    em.plot1.temp <- (
      c('U.S.' = mean(pows[,input$emission], na.rm = T),
        'State' = if(input$state == 'US' && is.null(powSelected)) NULL else{
          if(is.null(powSelected)) mean(pows.state()[,input$emission], na.rm = T) else {
            mean(pows[pows$PSTATABB == powSelected$PSTATABB,input$emission], na.rm = T)}
        }, 
        'Selected' = powSelected[, input$emission])
    )    
    ## change the names of em.plot1.temp according to the input$state and powSelected
    names(em.plot1.temp) <- c(
      'U.S.',  
      if(input$state == 'US' && is.null(powSelected)) NULL else {
        if(is.null(powSelected)) input$state else {
          as.character(powSelected$PSTATABB)
        }
      }, 
      if(is.null(powSelected)) NULL else 'Selected')[1:length(em.plot1.temp)]
    
    em.plot1 <<- em.plot1.temp
  })


  output$Plot1 <- renderPlot(barplot(em.plot1, main = paste('Average', gsub('PL|AN', '', input$emission))))

  
})  

















