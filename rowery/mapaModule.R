library(leaflet)

# Module UI function
mapaOutput <- function(id, label = "mapa") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    tags$p(),
    div(id = "mapPlotDiv", 
       style = "position:relative",
       alt = "mapa liczników",
       leafletOutput(ns("mapa_leaflet"), height=500)
    )
  )
}

# Module server function
mapa <- function(input, output, session, indeksy, lokacje, koloryLicznikow) {
  indeksy_mapa <-reactive({
    indeksy_rob = c()
    for(indeks in indeksy()){
      if (is.na(lokacje$lat[indeks])){indeksy_rob = c(indeksy_rob, indeks+1, indeks+2)}
      else {indeksy_rob = c(indeksy_rob,indeks)}
    }
    indeksy_rob}
  )
  
  output$mapa_leaflet <- renderLeaflet({
    shiny::validate(need(indeksy_mapa(), 'Wybierz przynajmniej jedno miejsce!'))
    
    kolory<-unname(koloryLicznikow[lokacje[indeksy_mapa(),]$Miejsce])
    
    leaflet(lokacje[indeksy_mapa(),], options = leafletOptions(maxZoom = 18)) %>% 
      addTiles() %>% 
      addCircleMarkers(lng = ~lon, lat = ~lat, label = ~Miejsce, 
                       radius = 10, color = kolory, opacity=1, weight = 8,
                       layerId=~Miejsce)
  })
  
 clickedId<-eventReactive(input$mapa_leaflet_marker_click,{
    input$mapa_leaflet_marker_click$id
  })
  
  return(clickedId)
  
}