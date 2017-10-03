library(shiny)

library(Cairo) #for nice looks of the graph
options(shiny.usecairo=T)

library(lubridate)
library(leaflet) #for maps

source('ladowanie_danych.R', encoding = 'UTF-8')
source('obsluga_sumowania.R', encoding = 'UTF-8')
source('wykresy.R', encoding = 'UTF-8')
source('read_from_api.R', encoding = 'UTF-8')

#reading locations
lokacje <- read.csv("pliki/czujniki_rowerowe.csv",dec=",", encoding='UTF-8')
sapply(lokacje,"class")

#reading colors etc
listy_stylow<-data.table(read.csv(file = "pliki/listy_stylow.csv", fileEncoding = 'UTF-8', colClasses = "character"))

#reading data
dane_long<-wczytaj_dane("pliki/dane_long.csv")
nazwy<-unique(dane_long[,Miejsce])

#dane_tyg<-podsumuj.tygodnie(dane_long)
#dane_m<-podsumuj.miesiace(dane_long)

zakresOd=  min(dane_long[,Data])
zakresDo = max(dane_long[,Data]) 
zakresDoPogoda= '2017-06-30'

okresy = c('dobowo', 'tygodniowo', 'miesięcznie')
wykresyPogody=c('temperatury', 'daty')

#dane godzinowe
godzinowe<-wczytaj_dane_godzinowe("pliki/dane_godzinowe_long.csv")
cat(file=stderr(), "przygotowania zakonczone", "\n")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
                    h1 {
                      margin-top:0px;
                      margin-bottom: 5px;
                    }
                    ")),
    tags$style(HTML("
                    h5 {
                      padding-left: 16px;
                      margin-top: 3px;
                    };
                    "))),
  h5('Autorka: Monika Pawłowska', align = 'right'),
  headerPanel('Liczba rowerów'),
  h5('Dane z liczników rowerowych w Warszawie'),
  sidebarLayout(
    sidebarPanel(
      lapply(1:length(nazwy), function(x) {
        #n <- length(nazwy)
        css_col <- paste0("#liczniki div.checkbox:nth-child(",x,
                          ") span{color: ", listy_stylow$kolory[x],"; font-weight : ",listy_stylow[[x,3]],"}")
        tags$style(type="text/css", css_col)
      }),
      checkboxGroupInput('liczniki', 'Wybierz miejsca', nazwy, 
                         selected = nazwy[c(1,7,12)], inline = FALSE, width = NULL),
      style= "padding: 10px 0px 0px 20px;"
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Wykres",
                 #wybor zakresu i grupowania daty
                 wellPanel(fluidRow(
                   column(5, #daty
                          dateRangeInput('zakres', 'Wybierz zakres dat',
                                         start=as.character(Sys.Date()-100), end=as.character(Sys.Date()-1), 
                                         min=zakresOd, max=as.character(Sys.Date()-1),
                                         separator = 'do', weekstart = 0, language = "pl")
                   ),
                   column(7, #dobowo/tygodniowo/miesiecznie
                          radioButtons('okres', 'Podsumuj', okresy, selected = okresy[1], 
                                       inline = TRUE, width = NULL)        
                   )
                 ), style= "padding: 10px 0px 0px 20px;"), #end wellPanel
                 div(id = "plotDiv", #wykres
                     style = "position:relative",
                     alt = "Ile rowerów jeździ w Warszawie",
                   plotOutput('plotLiczba', height=480, hover = hoverOpts(id = "plot_hover", delay = 100)),
                   uiOutput("bike_date_tooltip")
                 )
        ),
        tabPanel("Pogoda",
                 #wybor zakresu i grupowania daty
                 wellPanel(fluidRow(

                   column(4, #dobowo/tygodniowo/miesiecznie
                          radioButtons('rodzajPogody', 'Zależność od', wykresyPogody, selected = wykresyPogody[2], 
                                       inline = TRUE, width = NULL)        
                   ),
                   column(6, #daty
                          dateRangeInput('zakresPogoda', 'Wybierz zakres dat',
                                         start=as.character(as.Date(zakresDoPogoda)-120), end=zakresDoPogoda,
                                         min=zakresOd, max=zakresDoPogoda,
                                         separator = 'do', weekstart = 0, language = "pl")
                   )
                 ), style= "padding: 10px 0px 0px 20px;"), #end wellPanel
                 div(id = "weatherPlotDiv", 
                     style = "position:relative",
                     alt = "Ile rowerów w zależności od pogody",
                     plotOutput('plotPogoda', height=500, hover = hoverOpts(id = "plot_hover", delay = 100)),
                     uiOutput("bike_weather_tooltip")
                 )
        ),
        # tabPanel("Dane godzinowe",
        #          tags$p(), 
        #          div(id = "hoursPlotDiv", 
        #              style = "position:relative",
        #              #alt = "Ile rowerów w zależności od pogody",
        #              plotOutput('plotHours', height=500)
        #          )
        # ),
        tabPanel("Położenie liczników",
                 tags$p(), 
                 div(id = "mapPlotDiv", 
                     style = "position:relative",
                     alt = "mapa liczników",
                     leafletOutput("mymap", height=500)
                 )
        ),
        tabPanel("O aplikacji",
                 tags$p(),
                 tags$p('Aplikacja', tags$b('Rowery'),' przedstawia dane z automatycznych liczników rowerów w Warszawie
                        od początku ich funkcjonowania, czyli od ', 
                        zakresOd, ', do ', zakresDo, 
                        '. Źródłem danych jest: ',
                   tags$a(href='https://zdm.waw.pl', "Zarząd Dróg Miejskich w Warszawie"),
                   '(otrzymane mailem).'),
                 tags$p(
                   'Średnią dobową temperaturę w Warszawie (a dokładniej - na stacji meteorologicznej na Lotnisku Chopina) wzięłam ze strony ',
                   tags$a(href='https://dane.imgw.pl', 'https://dane.imgw.pl.'),
                   'Źródłem pochodzenia danych jest Instytut Meteorologii i Gospodarki Wodnej – Państwowy Instytut Badawczy.'
                 ),
                 tags$p(
                   'Autorka aplikacji: Monika Pawłowska (kontakt:',
                   tags$a(href='rowery@greenelephant.pl', "rowery@greenelephant.pl"),
                   '), mapa: Adam Kolipiński. Kod i dane źródłowe dostępne są',
                   tags$a(href='https://github.com/pawlowska/shiny-server/tree/master/rowery', 'tu.'))
        ) #end of "O..."
        
      ) #end tabsetPanel
    ) #end mainPanel
  )
) #end ui

server <- function(input, output, session) {
  #dane zaladowane od ostatniego git commit
  cat(file=stderr(), "probuje wczytac nowe_long", "\n")
  ostatnie_nowe_long<-wczytaj_dane("pliki/nowe_long.csv")
  ostatnia_data<-max(ostatnie_nowe_long[,Data])
  cat(file=stderr(), "ostatnia data w pliku nowe_long", as.character(ostatnia_data), "\n")
  
  #czy są nowsze dane niż w "nowe_long.csv"?  
  if (ostatnia_data<Sys.Date()-1) {
    updateDateRangeInput(session, 'zakres', 
                         end=as.character(Sys.Date()-1), max=as.character(Sys.Date()-1))
    
    ids<-read_counterids()
    nowe_dane<-zaladuj_dane_api(ids=ids, od=ostatnia_data)
    nowe_dane<-suma_licznikow(numery_dat(nowe_dane))
    #nowe_z_pogoda<-dodaj_pogode(nowe_dane)
    nowe_long<-wide_to_long(dodaj_pogode(nowe_dane))
    ostatnie_nowe_long<-rbind(ostatnie_nowe_long[Data<ostatnia_data], nowe_long)
    
    setorder(ostatnie_nowe_long, "Data")
    #uaktualnij "nowe" dane
    write.csv(ostatnie_nowe_long[Data>zakresDo], file = "pliki/nowe_long.csv", fileEncoding = 'UTF-8')
  }
  
  cat(file=stderr(), "ostatnia uaktualniona data", as.character(max(ostatnie_nowe_long[,Data])), "\n")
  
  #polacz ze "starymi" danymi
  dane_long<-rbind(dane_long, ostatnie_nowe_long[Data>zakresDo])
  #setorder(dane_long, Miejsce)
  
  dane_tyg<-podsumuj.tygodnie(dane_long)
  #setorder(dane_tyg, Miejsce)
  
  dane_m<-podsumuj.miesiace(dane_long)
  #setorder(dane_m, Miejsce)
  
  zakresDo<-as.character(Sys.Date()-1)
  
  indeksy<-reactive({ #ktore kolory beda uzyte
    shiny::validate(
      need(input$liczniki, 'Wybierz przynajmniej jedno miejsce!'))
    match(unique(data()$Miejsce), nazwy)
  })
  
  data <- reactive({
    zakres_dat=interval(input$zakres[1], input$zakres[2])
    #pick daily, weekly or monthly data
    if (input$okres==okresy[2])       {wybor<-dane_tyg}
    else if (input$okres==okresy[3])  {wybor<-dane_m}
    else {wybor<-dane_long}
    wybor[Data %within% zakres_dat & Miejsce %in% input$liczniki]
  })
  
  data_with_weather <- reactive({
    #zakres_dat=interval(zakresOd, zakresDoPogoda)
    zakres_dat=interval(input$zakresPogoda[1], input$zakresPogoda[2])
    
    dane_long[Data %within% zakres_dat & Miejsce %in% input$liczniki]
  })
  
  data_hourly <- reactive({
    godzinowe[Miejsce %in% input$liczniki]
  })
  
  uzyte_kolory<-reactive({
    listy_stylow$kolory[indeksy()]
  })
  
  uzyte_linie<-reactive({
    listy_stylow$linie[indeksy()]
  })
  
  output$plotLiczba <- renderPlot({
    shiny::validate(
      need((input$zakres[1]>=zakresOd)&(input$zakres[2]>=zakresOd), 
           paste("Data spoza zakresu - dostępne dane od", zakresOd)),
      need((input$zakres[1]<=zakresDo)&(input$zakres[2]<=zakresDo), 
           paste("Data spoza zakresu - dostępne dane do", zakresDo)),
      need(input$liczniki, 'Wybierz przynajmniej jedno miejsce!')
    )
    wykres_kilka(data(), 
                 start=input$zakres[1], stop=input$zakres[2], paleta=uzyte_kolory(), linie = uzyte_linie())
  })
  
  output$plotPogoda <- renderPlot({
    shiny::validate(
      need(input$liczniki, 'Wybierz przynajmniej jedno miejsce!')
    )
    if(input$rodzajPogody==wykresyPogody[1]) {
      pogoda_basic(data_with_weather(), paleta=uzyte_kolory())
    } else {
      #zakres_dat=interval(input$zakresPogoda[1], input$zakresPogoda[2])
      
      #wykres_pogoda_liczba(data_with_weather()[Data %within% zakres_dat],
      wykres_pogoda_liczba(data_with_weather(),
                           start=input$zakresPogoda[1], stop=input$zakresPogoda[2], 
                           paleta=uzyte_kolory(), linie = uzyte_linie())
    }
  })

  output$plotHours <- renderPlot({
    shiny::validate(
      need(input$liczniki, 'Wybierz przynajmniej jedno miejsce!'))
    wykres_godzinowy(data_hourly(), paleta=uzyte_kolory(), linie = uzyte_linie())
  })
  
  output$bike_date_tooltip <- renderUI({
    #based on: http://stackoverflow.com/questions/38992270/r-shiny-tooltip-in-ggplot
    
    hover <- input$plot_hover
    #is mouse close to a point?
    point <- nearPoints(data(), hover, threshold = 8, maxpoints = 1)[ ,c("Data", "Liczba_rowerow")]
    if (nrow(point) == 0) return(NULL) #jesli nie ma punktu w poblizu

    #else add to UI
    wellPanel(
      style = tooltip_html(tooltip_position(hover)),
      p(HTML(paste0( point$Data,": ", point$Liczba_rowerow)))
    )
  })
  
  output$bike_weather_tooltip <- renderUI(
    if (input$rodzajPogody==wykresyPogody[2]) {return(NULL)}
    else {
    
    hover <- input$plot_hover
    #is mouse close to a point?
    point <- nearPoints(data_with_weather(), hover, threshold = 8, maxpoints = 1)[ ,c("Data","temp_avg","deszcz","snieg", "Liczba_rowerow")]
    if (nrow(point) == 0) return(NULL) #jesli nie ma punktu w poblizu
    opad=point$deszcz+point$snieg
    
    #else add to UI
    wellPanel(
      style = tooltip_html(tooltip_position(hover, w=180)),
      p(HTML(paste0( point$Data,": ",point$temp_avg, '&degC, ',opad,' mm, ', point$Liczba_rowerow)))
    )
  })

  output$mymap <- renderLeaflet({
    leaflet(lokacje[indeksy(),], options = leafletOptions(maxZoom = 18)) %>% 
    addTiles() %>% 
    addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~Miejsce, radius = 10, color = uzyte_kolory(), opacity=1, weight = 8)
  })
  
}

tooltip_position<-function(hover, w=130) {   #calculate the position of the tooltip
  #in relative units
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  #in pixel units
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  if(hover$range$right-left_px<w) left_px<-left_px-(w+5) #tooltips too far right were hidden
  
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
  
  c(left_px, top_px)
}

tooltip_html<-function(pos_px) {
  left_px<-pos_px[1]
  top_px<-pos_px[2]
  paste0("position:absolute; z-index:100; padding: 0 6px 0 6px; height: 22px; overflow: hidden;
                    background-color: rgba(245, 245, 245, 0.85); ",
  "left:", left_px + 2, "px; top:", top_px + 2, "px;")
}

shinyApp(ui = ui, server = server)