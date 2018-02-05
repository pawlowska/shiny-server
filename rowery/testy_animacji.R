library(shiny)
library(xts)
library(leaflet)
library(dplyr)


library(data.table)
source('ladowanie_danych.R', encoding = 'UTF-8')

godz_kierunki_2017<-
  zaladuj_dane_godzinowe('dane/2017_08_05-14_06.csv', format="%y-%m-%d %H:%M", ziw=F, bez_kierunkow = F) %>% 
  select ('Czas', Ban_in = 'Banacha IN', Ban_out = 'Banacha OUT','Banacha') %>%
  mutate (perc_in = Ban_in/max(Ban_in+Ban_out),perc_out = Ban_out/max(Ban_in+Ban_out), lat = 52.21058, lng =	20.986546,
          name = 'Banacha')%>%
  mutate (rad_in = 8 + 50*perc_in, rad_out = 8+50*perc_out) %>%
  mutate (perc_ruch =  Ban_in+Ban_out/max(Ban_in+Ban_out), perc_kier = Ban_in/(Ban_in+Ban_out+1))

ui <- fluidPage(
    sliderInput("time", "date",min(godz_kierunki_2017$Czas), 
                max(godz_kierunki_2017$Czas),
                value = min(godz_kierunki_2017$Czas),
                step=3600,
                animate=animationOptions(interval = 333),
                timezone ='+0100'),
    radioButtons("kind", "Rodzaj animacji:",c("pierwszy","drugi")),
    leafletOutput("mymap")
  )



server <- function(input, output, session) {
  points <- reactive({
    godz_kierunki_2017 %>% 
      filter(Czas==input$time)
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%  setView(20.986, 52.2241,  zoom = 12) %>%
      addTiles() 
    })
  
  observe({
    proxy <- leafletProxy("mymap") 
    if (input$kind == "pierwszy") {
      proxy %>% clearMarkers() %>%
      addCircleMarkers(data = points(), popup =~name, radius = ~rad_in, color = 'red', opacity=0.8, stroke = F) %>%
      addCircleMarkers(data = points(), popup =~name, radius = ~rad_out, color = 'blue', opacity=0.8, stroke = F)      
      }
  if (input$kind == "drugi"){
    proxy %>% clearMarkers() %>%
    addCircleMarkers(data = points(), popup =~name, radius = ~8 + 40*perc_in, fill=TRUE, fillColor = ~rgb(perc_kier, 0, 1-perc_kier, maxColorValue = 1), fillOpacity=1, stroke = F) 
    }
  })
}

shinyApp(ui, server)
?observe
