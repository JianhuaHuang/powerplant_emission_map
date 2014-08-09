
shinyServer(function(input, output, session) {
  dispCol <- reactive(c(phy.info, input$emission, gsub('AN', 'RTA', input$emission)))
  
  makeReactiveBinding('pow.clicked')
  makeReactiveBinding('em.plot1')
  
  # Create the map; this is not the "real" map, but rather a proxy
  # object that lets us control the leaflet map on the page.
  map <- createLeafletMap(session, 'map')
  
  observe({
    if (is.null(input$map_click))
      return()
    pow.clicked <<- NULL
  })
  
  observe({
    map$clearShapes()
    
    if (nrow(pows) == 0)
      return()
    
    map$addCircle(
      pows$LAT,
      pows$LON,
      ((pows[[input$emission]] - min(pows[[input$emission]])) / max(pows[[input$emission]]) + .05) * 10^6.5 / max(5, input$map_zoom)^2,
      row.names(pows),
      list(
        weight= 1,
        fill=TRUE,
        color= power.categ$COLORS
      )
    )
  })
  
  state.selected <-  reactive(states[states$Abbreviation == input$state, ])
  observe(if (nrow(state.selected()) != 0) {
    map$setView(lat = state.selected()$latitude, lng = state.selected()$longitude, zoom = 7)} else
      map$setView(lat = 37.45, lng = -93.85, zoom = 4))  
    
  observe({
    event <- input$map_shape_click
    if (is.null(event))
      return()
    map$clearPopups()

    pow <- pows[row.names(pows) == event$id, ]  ## powerplant clicked on
    pow.clicked <<- pow
    
    isolate({
      content <- as.character(tagList(
        tags$strong(paste(pow$PNAME)),
        br(),
        paste0(pow$CNTYNAME,', ', pow$PSTATABB),
        br(),
        paste('Primary fuel:', power.categ[as.integer(event$id), 'FUELPOPUP']),
        br(),
        paste("Annual Generation:", pow[['PLNGENAN']], '(MWh)'),
        br(),
        paste("Estimated", gsub('PL|AN', '', input$emission), ":", round(pow[[input$emission]]), '(tons)')
      ))
      map$showPopup(event$lat, event$lng, content)
    })
    
  })
  
  pows.state <- reactive(if(input$state == 'US') NULL else pows[pows$PSTATABB == input$state, dispCol()])
  
  output$Table <- renderDataTable(
    if (!is.null(pow.clicked)) cbind(pow.clicked[, dispCol()], Go = paste0('<a><i class="fa fa-globe"></i></a>')) else {
      if(input$state == 'US') 
        cbind(pows[, dispCol()], Go = paste('<a class="go-map" href="" data-lat="', pows.state()$LAT, '" data-long="', pows.state()$LON, '"><i class="fa fa-crosshairs"></i></a>', sep="")) else          
          cbind(pows.state(), Go = paste('<a class="go-map" href="" data-lat="', pows.state()$LAT, '" data-long="', pows.state()$LON, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    }, options = list(iDisplayLength = 50))
  
  observe({    
    em.plot1.temp <- (
      c('U.S.' = mean(pows[,input$emission], na.rm = T),
        'State' = if(input$state == 'US' && is.null(pow.clicked)) NULL else{
          if(is.null(pow.clicked)) mean(pows.state()[,input$emission], na.rm = T) else {
            mean(pows[pows$PSTATABB == pow.clicked$PSTATABB,input$emission], na.rm = T)}
        }, 
        'Selected' = pow.clicked[, input$emission])
    )    
    ## change the names of em.plot1.temp according to the input$state and pow.clicked
    names(em.plot1.temp) <- c(
      'U.S.',  
      if(input$state == 'US' && is.null(pow.clicked)) NULL else {
        if(is.null(pow.clicked)) input$state else {
          as.character(pow.clicked$PSTATABB)
        }
      }, 
      if(is.null(pow.clicked)) NULL else 'Selected')[1:length(em.plot1.temp)]
    
    em.plot1 <<- em.plot1.temp
  })

  output$Plot1 <- renderPlot(barplot(em.plot1, main = paste('Average', gsub('PL|AN', '', input$emission))))
  
#   output$Plot2 <- renderPrint(em.plot1)
  
})  


















