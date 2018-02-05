library(shiny)
library(xts)
library(leaflet)
library(dplyr)
library(tidyr)
library(stringr)
library(leaflet.minicharts)


library(data.table)
source('ladowanie_danych.R', encoding = 'UTF-8')
?stri_sub

#reading locations
lokacje <- read.csv("pliki/czujniki_rowerowe.csv",dec=",", encoding='UTF-8') 
sapply(lokacje,"class")

View(lokacje)


godz_kierunki_2017<-
  zaladuj_dane_godzinowe('dane/2017_08_05-14_06.csv', format="%y-%m-%d %H:%M", ziw=F, bez_kierunkow = F) 

#wektor uzgadniający różnice między nazwami czujników w danych lokacją i danych godzinowych
translate <-c("Al. USA - południe" = "Al. USA płd.", 
              "Al. USA - północ" = "Al. USA płn.",
              "Dworzec Wileński \\(Al. Solidarności\\)" = "Dworzec Wileński (al. Solidarności)",
              "Dworzec Wileński Nowy" = "Dworzec Wileński (Targowa)",
              "Świętokrzyska - Emilii Plater, płd" = "Świętokrzyska/Emilii Plater płd.",
              "Świętokrzyska - Emilii Plater, płn" = "Świętokrzyska/Emilii Plater płn.")

tylko_do <- c("Al. USA płn.","Świętokrzyska/Emilii Plater płd.","Żwirki i Wigury/Trojdena wsch.")
tylko_od <- c("Al. USA płd.","Świętokrzyska/Emilii Plater płn.","Żwirki i Wigury/Trojdena zach.")

kierunki <-  gather(godz_kierunki_2017, key = 'Miejsce', value = 'Liczba',
          names(godz_kierunki_2017)[names(godz_kierunki_2017) %like% '^.+IN|OUT$']) %>%
  select("Czas","Liczba","Miejsce")%>%
  mutate (in_out = sub('[ _]','',str_sub(Miejsce,start=-3)),  Miejsce = sub('[ _]IN|[ _]OUT$', '', Miejsce))%>%
  spread (in_out,Liczba) %>% mutate (Miejsce = str_replace_all(Miejsce,translate)) %>% 
  mutate(opis = Miejsce) %>%
  merge(lokacje,by = 'Miejsce', all.x=T) %>%
  mutate(IN = ifelse(Miejsce %in% tylko_do, IN+OUT, IN)) %>%
  mutate(OUT = ifelse(Miejsce %in% tylko_od, IN+OUT, OUT)) %>% 
  mutate(OUT = ifelse(Miejsce %in% tylko_do, 0, OUT)) %>%
  mutate(IN = ifelse(Miejsce %in% tylko_od, 0, IN)) %>%
  mutate (perc_in = IN/max(IN+OUT),perc_out = OUT/max(IN+OUT))%>%
  mutate (rad_in = 8 + 50*perc_in, rad_out = 8+50*perc_out) %>%
  mutate (perc_ruch =  (IN+OUT)/max(IN+OUT), perc_kier = IN/(IN+OUT+1))

lokacje2 <- group_by(kierunki,Miejsce,lat,lon) %>% summarize()
  



View(kierunki)
View(lokacje2)
translate
?str_replace_all
?str_sub
"Dworzec Wileński (Al. Solidarności)" ==  "Dworzec Wileński (Al. Solidarności)"


ui <- fluidPage(
    # sliderInput("time", "date",min(kierunki$Czas),
    #             max(kierunki$Czas),
    #             value = min(kierunki$Czas),
    #             step=3600,
    #             animate=animationOptions(interval = 333),
    #             timezone ='+0100'),
   radioButtons("kind", "Rodzaj animacji:",c("batoniko-wykresy","ciastko-wykresy")),
    leafletOutput("mymap")
  )



server <- function(input, output, session) {
  # points <- reactive({
  #   kierunki %>% 
  #     filter(Czas==input$time)
  # })
  # 
  output$mymap <- renderLeaflet({
    leaflet() %>%  setView(20.986, 52.2241,  zoom = 12) %>%
      addTiles() %>% addMinicharts(lokacje2$lon, lokacje2$lat,
                                layerId = lokacje2$Miejsce,
                                width = 45, height = 45)
    })
  
  observe({
  #   proxy <- leafletProxy("mymap", session) 
  #   if (input$kind == "pierwszy") {
  #     proxy %>% clearMarkers() %>% 
  #     addCircleMarkers(data = points(), popup =~Miejsce, radius = ~rad_in, color = 'red', opacity=0.8, stroke = F) %>%
  #     addCircleMarkers(data = points(), popup =~Miejsce, radius = ~rad_out, color = 'blue', opacity=0.8, stroke = F)      
  #     }
  #   if (input$kind == "drugi"){
  #     proxy %>% clearMarkers() %>% 
  #     addCircleMarkers(data = points(), popup =~Miejsce, radius = ~8 + 40*perc_in, fill=TRUE, fillColor = ~rgb(perc_kier, 0, 1-perc_kier, maxColorValue = 1), fillOpacity=1, stroke = F) 
  # }
    if (input$kind == "ciastko-wykresy"){
      leafletProxy('mymap', session) %>% 
        updateMinicharts(
          kierunki$Miejsce,
          chartdata = kierunki[,c('IN','OUT')],
          maxValues = max(as.matrix(kierunki[,c('IN','OUT')])),
          time = kierunki$Czas,
          type = 'pie',
          width = 10+60*sqrt(kierunki$perc_ruch),
          transitionTime = 10
        )
    }
    if (input$kind == "batoniko-wykresy"){
      leafletProxy('mymap', session) %>% 
        updateMinicharts(
          kierunki$Miejsce,
          chartdata = kierunki[,c('IN','OUT')],
          maxValues = max(as.matrix(kierunki[,c('IN','OUT')])),
          time = kierunki$Czas,
          type = 'bar',
          width = 20,
          heigh = 80,
          transitionTime = 10
        )
    }    
  })
}


??addMiniCharts
shinyApp(ui, server)
?observe
