library(shiny)

library(Cairo) #for nice looks of the graph
options(shiny.usecairo=T)

library(lubridate)
library(leaflet) #for maps
library(shinyWidgets)
#library(shinyjs)

source('ladowanie_danych.R', encoding = 'UTF-8')
source('obsluga_sumowania.R', encoding = 'UTF-8')
source('wykresy.R', encoding = 'UTF-8')
source('tooltip.R', encoding = 'UTF-8')
source('text.R', encoding = 'UTF-8')

#reading locations
lokacje <- read.csv("pliki/polozenie_licznikow.csv",dec=",", encoding='UTF-8')
sapply(lokacje,"class")
lokacje<-data.table(lokacje)

#reading colors etc
listy_stylow<-data.table(read.csv(file = "pliki/listy_stylow.csv", fileEncoding = 'UTF-8', colClasses = "character"))

#reading data
dane_long<-wczytaj_dane("pliki/dane_long.csv")
nazwy<-unique(dane_long[,Miejsce])
zakresOd=  min(dane_long[,Data])
zakresDo = max(dane_long[,Data]) 
zakresDoPogoda= '2018-02-28'
plik_pogoda="pliki/IMGW_pogoda_20180228.csv"

okresy = c('dobowo', 'tygodniowo', 'miesięcznie','rocznie')
wartosci = c('bezwzględne', 'procentowe') #PZ
wykresyPogody=c('temperatury', 'daty')

#dane godzinowe chwilowo nieużywane
#godzinowe<-wczytaj_dane_godzinowe("pliki/dane_godzinowe_long.csv")
cat(file=stderr(), "jest", as.character(Sys.Date()), "\n")


ui <- fluidPage(
  #useShinyjs(),
  tags$head(tags$script('var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),
  # tags$head(tags$script('var top_window=(window.top == window.self);
  #                       Shiny.onInputChange("top_window", top_window);
  #                       ')),
  tags$head(
    tags$style(HTML('.shiny-split-layout>div  {overflow: visible;}')),
    tags$style(HTML("body {font-size:12px;}")),
    tags$style(HTML("h1 {font-size:28px; margin-top:10px; margin-bottom: 5px;}"))#,
  ),
  
  headerPanel('Liczniki rowerów w Warszawie'),
  sidebarLayout(
    sidebarPanel(
      lapply(1:length(nazwy), function(x) {
        tags$style(type="text/css", 
                   css_list(what="#liczniki div.checkbox:nth-child(",listy_stylow, x))
      }),
      uiOutput('wyborLicznikow'),
      style= "padding: 10px 10px 0px 15px;" #top right bottom left; grey bckgrnd around selections
    ),
    mainPanel(
      tabsetPanel(id = "zakladki",
        tabPanel("Wykres",
                 #wybor zakresu i grupowania daty
                 wellPanel(fluidRow(
                   column(5, #daty
                          dateRangeInput('zakres', 'Wybierz zakres dat',
                                         start=as.character(Sys.Date()-100), end=as.character(Sys.Date()-1), 
                                         min=zakresOd, max=as.character(Sys.Date()-1),
                                         separator = 'do', weekstart = 0, language = "pl")
                   ),
                   column(7,
                    splitLayout(
                      selectInput('okres', 'Podsumuj', okresy, selected = okresy[1]),
                      selectInput('wartosc', 'Wartości', wartosci, selected = wartosci[1])
                    )
                   )
                 ), style= "padding: 10px 10px 0px 15px;"), #end wellPanel
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
                    ), style= "padding: 5px 0px 0px 15px;"
                 ), #end wellPanel
                 div(id = "weatherPlotDiv", 
                     style = "position:relative",
                     alt = "Ile rowerów w zależności od pogody",
                     plotOutput('plotPogoda', height=500, hover = hoverOpts(id = "plot_hover", delay = 100)),
                     uiOutput("bike_weather_tooltip")
                 )
        ),
        tabPanel("Położenie liczników",
                 tags$p(), 
                 div(id = "mapPlotDiv", 
                     style = "position:relative",
                     alt = "mapa liczników",
                     leafletOutput("mymap", height=500)
                 )
        ),
        tabPanel("O aplikacji", value="o_aplikacji",
                 #title= ifelse(top_window, "O aplikacji", "O licznikach"),
                 tags$p(),
                 conditionalPanel(
                   condition = "window.top == window.self",
                   tekst1, tekst2, tekst3
                 ),
                conditionalPanel(
                   condition = "window.top != window.self",
                   tekst_zdm1, tekst_zdm2, tekst_zdm3
                 )
        ) #end of "O..."
        
      )#end tabsetPanel
      
    ) #end mainPanel
  )
) #end ui

server <- function(input, output, session) {
  values<-reactiveValues(first_run=TRUE)
  indeksy<-reactive(
    if(values$first_run) {
      values$first_run<-FALSE
      query <- parseQueryString(session$clientData$url_search)
      m<-lokacje[id==query$licznik]$Miejsce
      match(m, nazwy)
    } else {
      match(unique(data()$Miejsce), nazwy)
    }
  )
  
  output$wyborLicznikow <- renderUI({
    req(input$dimension)
    
    init_selected<-isolate(nazwy[indeksy()])
    
    if (input$dimension[1]<750) {
      pickerInput('liczniki', label=NULL,#'Wybierz miejsca', 
                  nazwy, selected = init_selected, 
                  options = list(`actions-box` = TRUE, 
                                 `selected-text-format` = "count > 5",
                                 `select-all-text`="Zaznacz wszystkie",
                                 `deselect-all-text`="Odznacz wszystkie",
                                 `none-selected-text`="Wybierz miejsca"), 
                  multiple = T)
    } else {
      checkboxGroupInput('liczniki', 'Wybierz miejsca', 
                         choices=nazwy, selected = init_selected, 
                         inline = FALSE, width = NULL)
    }
  })
  
  #dane zaladowane od ostatniego git commit
  p<-"pliki/nowe_long.csv"
  if (file.exists(p)) {
    cat(file=stderr(), "probuje wczytac nowe_long", "\n")
    ostatnie_nowe_long<-wczytaj_dane(p)
    ostatnia_data<-max(ostatnie_nowe_long[,Data])
    cat(file=stderr(), "ostatnia data w pliku nowe_long", as.character(ostatnia_data), "\n")
  }
  else {
    cat(file=stderr(), "brak pliku nowe_long", "\n")
    ostatnie_nowe_long<-dane_long[0,]
    ostatnia_data<-max(dane_long[,Data])
    cat(file=stderr(), "ostatnia data w pliku dane_long", as.character(ostatnia_data), "\n")
  }
  
  #czy są nowsze dane?  
  if (ostatnia_data<Sys.Date()-1) {
    updateDateRangeInput(session, 'zakres', 
                         end=as.character(Sys.Date()-1), max=as.character(Sys.Date()-1))
    #zaladuj
    nowe_long<-zaladuj_nowe_z_api(ostatnia_data, plik_pogoda, lokacje)
    
    #polacz
    ostatnie_nowe_long<-rbind(ostatnie_nowe_long[Data<ostatnia_data], nowe_long)
    setorder(ostatnie_nowe_long, "Data")
    #uaktualnij "nowe" dane
    write.csv(ostatnie_nowe_long[Data>zakresDo], file = "pliki/nowe_long.csv", fileEncoding = 'UTF-8')
  }
  
  cat(file=stderr(), "ostatnia uaktualniona data", as.character(max(ostatnie_nowe_long[,Data])), "\n")
  
  #polacz ze "starymi" danymi
  dane_long<-rbind(dane_long, ostatnie_nowe_long[Data>zakresDo])
  
  #podsumuj
  dane_tyg<-podsumuj.tygodnie(dane_long)
  dane_m<-podsumuj.miesiace(dane_long)
  dane_y<-podsumuj.lata(dane_long)
  
  zakresDo<-as.character(Sys.Date()-1)
  

  
  data <- reactive({
    zakres_dat=interval(input$zakres[1], input$zakres[2])
    #pick daily, weekly or monthly data
    if (input$okres==okresy[2])       {wybor<-dane_tyg}
    else if (input$okres==okresy[3])  {wybor<-dane_m}
    else if (input$okres==okresy[4])  {
      zakres_dat=interval(as.Date("2014-01-01"), input$zakres[2])
      wybor<-dane_y
      }
    else {wybor<-dane_long}

    #PZ
    if(input$wartosc == wartosci[1])
      wybor[Data %within% zakres_dat & Miejsce %in% input$liczniki]
    else
      podsumuj.procentowo(wybor[Data %within% zakres_dat & Miejsce %in% input$liczniki])
  })
  
  krok<-reactive({
    if (input$okres==okresy[2])       {k<-7}
    else if (input$okres==okresy[3])  {k<-31}
    else if (input$okres==okresy[4])  {k<-366}
    else {k<-1}
    k  
  })
  
  data_with_weather <- reactive({
    zakres_dat=interval(input$zakresPogoda[1], input$zakresPogoda[2])
    
    dane_long[Data %within% zakres_dat & Miejsce %in% input$liczniki]
  })
  
  #data_hourly <- reactive({
  #  godzinowe[Miejsce %in% input$liczniki]
  #})

  uzyte_style<-reactive({
    listy_stylow[indeksy()]
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
                 start=input$zakres[1], stop=input$zakres[2], 
                 paleta=uzyte_style()$kolory, linie = uzyte_style()$linie, alfy=as.numeric(uzyte_style()$alfy),
                 krok=krok(), wartosc = input$wartosc)
  })
  
  output$plotPogoda <- renderPlot({
    shiny::validate(
      need(input$liczniki, 'Wybierz przynajmniej jedno miejsce!')
    )
    if(input$rodzajPogody==wykresyPogody[1]) {
      pogoda_basic(data_with_weather(), paleta=uzyte_style()$kolory)
    } else {
      wykres_pogoda_liczba(data_with_weather(),
                           start=input$zakresPogoda[1], stop=input$zakresPogoda[2], 
                           paleta=uzyte_style()$kolory, linie = uzyte_style()$linie)
    }
  })

  #output$plotHours <- renderPlot({
  #  shiny::validate(
  #    need(input$liczniki, 'Wybierz przynajmniej jedno miejsce!'))
  #  wykres_godzinowy(data_hourly(), paleta=uzyte_kolory(), linie = uzyte_linie())
  #})
  
  output$bike_date_tooltip <- renderUI({
    #based on: http://stackoverflow.com/questions/38992270/r-shiny-tooltip-in-ggplot
    
    hover <- input$plot_hover
    #is mouse close to a point?
    point <- nearPoints(data(), hover, threshold = 8, maxpoints = 1)[ ,c("Data", "Liczba_rowerow")]
    if (nrow(point) == 0) return(NULL) #jesli nie ma punktu w poblizu

    #else add to UI
    wellPanel(
      style = tooltip_html(tooltip_position(hover)),
      p(HTML(paste0( point$Data,": ", round(point$Liczba_rowerow, digits=1))))
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
    shiny::validate(
      need(input$liczniki, 'Wybierz przynajmniej jedno miejsce!')
    )
    leaflet(lokacje[indeksy(),], options = leafletOptions(maxZoom = 18)) %>% 
    addTiles() %>% 
    addCircleMarkers(lng = ~lon, lat = ~lat, popup = ~Miejsce, 
                     radius = 10, color = uzyte_style()$kolory, opacity=1, weight = 8)
  })
  
}

shinyApp(ui = ui, server = server)