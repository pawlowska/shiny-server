library(shiny)

library(Cairo) #for nice looks of the graph
options(shiny.usecairo=T)

library(lubridate)
library(shinyWidgets)
library(shinyBS)

source('ladowanie_danych.R', encoding = 'UTF-8')
source('obsluga_sumowania.R', encoding = 'UTF-8')
source('wykresy.R', encoding = 'UTF-8')
source('tooltip.R', encoding = 'UTF-8')
source('text.R', encoding = 'UTF-8')
source('mapaModule.R', encoding = 'UTF-8')
source('bikeCountPlotModule.R', encoding = 'UTF-8')

Sys.setlocale("LC_ALL", "Polish")

katalog="pliki"

#reading locations
lokacje <- read.csv(paste(katalog, "polozenie_licznikow.csv", sep="/"),dec=".", encoding='UTF-8')
lokacje<-data.table(lokacje)

#reading colors etc
style<-wczytaj_style(katalog)

#reading data
dane_long<-wczytaj_dane(paste(katalog, "dane_long.csv", sep="/"))
nazwy<-unique(dane_long[,Miejsce])
zakresOd=  min(dane_long[,Data])
zakresDo = max(dane_long[,Data]) 
plik_pogoda=paste(katalog, "IMGW_pogoda_20180930.csv", sep="/")
temp<-fread(plik_pogoda)
zakresDoPogoda=as.character(max(temp[,Data]))

#okresy = c('dobowo', 'tygodniowo', 'miesięcznie','rocznie')
wartosci = c('bezwzględne', 'procentowe') #PZ
wykresyPogody=c('temperatury', 'daty')

okresy=list('dobowo'=1, 'tygodniowo'=7, 'miesięcznie'=31, 'rocznie'=366)

#dane godzinowe chwilowo nieużywane
#godzinowe<-wczytaj_dane_godzinowe("pliki/dane_godzinowe_long.csv")
cat(file=stderr(), "jest", as.character(Sys.Date()), "\n")

ui <- fluidPage(
  tags$head(tags$script(src="rozmiar.js"),
            tags$script(src="iframe_css.js")),
  tags$head(
    tags$style(HTML('.shiny-split-layout>div  {overflow: visible;}')),
    tags$style(HTML("#caly, #eksport {height:34px;}")),
    tags$style(HTML(".input-sm {height:34px;}")),
    tags$style(HTML("h1 {font-size:28px; margin-top:10px; margin-bottom: 5px;}"))#,
  ),
  
  headerPanel('Liczniki rowerów w Warszawie'),
  sidebarLayout(
    sidebarPanel(
      lapply(1:length(nazwy), function(x) {
        tags$style(type="text/css", 
                   css_list(what="#liczniki div.checkbox:nth-child(",style, x))
      }),
      uiOutput('wyborLicznikow'),
      style= "padding: 10px 10px 0px 15px;" #top right bottom left; grey bckgrnd around selections
      
    ),
    mainPanel(
      tabsetPanel(id = "zakladki",
        tabPanel("Wykres", value = "wykres",
                 #wybor zakresu i grupowania daty
                 wellPanel(fluidRow(
                   column(6,  #daty 
                          splitLayout(cellWidths = c("82%","9%", "9%"),
                          #splitLayout(cellWidths = c("90%","10%"),
                                      cellArgs = list(style = " display: inline-block; vertical-align: bottom;"),
                          dateRangeInput('zakres', 'Wybierz zakres dat', 
                                         start=as.character(Sys.Date()-100), end=as.character(Sys.Date()-1), 
                                         min=zakresOd, max=as.character(Sys.Date()-1),
                                         separator = 'do', weekstart = 0, language = "pl"),
                          actionButton(inputId = "caly", "∞", style = "margin-bottom: 15px;"),
                          #allDatesInput("caly", label="∞"),
                          downloadButton(outputId = "eksport", label="", style = "margin-bottom: 15px;" ),
                          bsTooltip(id = "caly", title = "Pokaż cały zakres dat", placement = "left", trigger = "hover")
                          )
                   ),
                   column(6,
                    splitLayout(
                      selectInput('okres', 'Podsumuj', names(okresy), selected = names(okresy)[1]),
                      selectInput('wartosc', 'Wartości', wartosci, selected = wartosci[1])
                    )
                   )
                 ), style= "padding: 10px 10px 0px 15px;"), #end wellPanel
                 # div(id = "plotDiv", #wykres
                 #     style = "position:relative",
                 #     alt = "Ile rowerów jeździ w Warszawie",
                 #   plotOutput('plotLiczba', height=500, hover = hoverOpts(id = "plot_hover", delay = 100)),
                 #   uiOutput("bike_date_tooltip")
                 # )
                 bikeCountPlotOutput('plotLiczba')
        ),
        tabPanel("Pogoda", value="pogoda",
                 #wybor zakresu i grupowania daty
                 wellPanel(fluidRow(
                       column(4, #dobowo/tygodniowo/miesiecznie
                              radioButtons('rodzajPogody', 'Zależność od', wykresyPogody, selected = wykresyPogody[2], 
                                           inline = TRUE, width = NULL)        
                       ),
                       column(6, #daty
                              splitLayout(cellWidths = c("90%", "10%"),
                                          cellArgs = list(style = " display: inline-block; vertical-align: bottom;"),
                              dateRangeInput('zakresPogoda', 'Wybierz zakres dat',
                                             start=as.character(as.Date(zakresDoPogoda)-120), end=zakresDoPogoda,
                                             min=zakresOd, max=zakresDoPogoda,
                                             separator = 'do', weekstart = 0, language = "pl"),
                              actionButton(inputId = "calyPogoda", "∞", style = "margin-bottom: 15px;"),
                              bsTooltip(id = "calyPogoda", title = "Pokaż cały zakres dat", 
                                        placement = "left", trigger = "hover"))
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
        tabPanel("Mapa", value='mapa',
                 mapaOutput(id='mapa_licznikow')
        ),
        tabPanel("O aplikacji", value="o_aplikacji",
                 #title= ifelse(top_window, "O aplikacji", "O licznikach"),
                 tags$p(),
                 conditionalPanel(
                   condition = "window.top == window.self",
                   tekst1, tekst2, tekst3, tekst_zdm4
                 ),
                conditionalPanel(
                   condition = "window.top != window.self",
                   tekst_zdm1, tekst_zdm2, tekst_zdm3, tekst_zdm4
                 )
        ) #end of "O..."
      )#end tabsetPanel
    ) #end mainPanel
  )
) #end ui

server <- function(input, output, session) {
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if(length(query)>0&&!is.null(query$tab)){
      updateTabsetPanel(session, inputId = "zakladki", selected = query$tab)
    }
  })
  
  
  values<-reactiveValues(first_run=TRUE)
  indeksy<-reactive(
    if(values$first_run) { #first run => sprawdz czy nr licznika podany w url
      values$first_run<-FALSE
      query <- parseQueryString(session$clientData$url_search)
      if (length(query)==0) { #none
        NULL
      }
      else if ((!is.null(query$licznik))&&query$licznik=='all') { #all
          c(1:length(nazwy))
      } else { #one
          m<-lokacje[id==query$licznik]$Miejsce
          match(m, nazwy)
      }
    } else {
      match(unique(data()$Miejsce), nazwy)
    }
  )
  
  output$wyborLicznikow <- renderUI({
    req(input$dimension)
    init_selected<-isolate(nazwy[indeksy()])
    
    if (input$dimension[1]<780) {
      pickerInput('liczniki', label=NULL,#'Wybierz miejsca', 
                  nazwy, selected = init_selected, 
                  options = list(`actions-box` = TRUE, 
                                 `selected-text-format` = "count > 5",
                                 `count-selected-text` = "Wybrano {0}",
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
  
  #callModule(allDates, "caly", reactive(data), reactive(input$zakres),
  #           as.character(min(dane_long[Miejsce %in% input$liczniki]$Data)),
  #           as.character(max(dane_long[Miejsce %in% input$liczniki]$Data)))
  observeEvent(input$caly, { #caly zakres dat
     if (!is.null(data())) {
       updateDateRangeInput(session, 'zakres', 
                          start=as.character(min(dane_long[Miejsce %in% input$liczniki]$Data)), 
                          end=(as.character(max(dane_long[Miejsce %in% input$liczniki]$Data))))
     }
  })
  
  
  observeEvent(input$calyPogoda, { #caly zakres dat pogoda
    if (!is.null(data())) {
      updateDateRangeInput(session, 'zakresPogoda', 
                           start=as.character(min(dane_long[Miejsce %in% input$liczniki]$Data)), 
                           end=(zakresDoPogoda))
    }
  })
  
  #aktualizacja danych
  dane_long<-dodaj_nowe_dane(stare=dane_long, p=(paste(katalog, "nowe_long.csv", sep="/")), 
                             plik_pogoda=plik_pogoda, lokacje=lokacje, zakresDo=zakresDo)
  
  #aktualizacja daty
  zakresDo<-as.character(Sys.Date()-1)
  cat(file=stderr(), "aktualizuje date w UI", "\n")
  updateDateRangeInput(session, 'zakres', end=as.character(zakresDo), max=as.character(zakresDo))
  
  
  #podsumuj
  dane_tyg<-podsumuj.okresy(dane_long, "startTyg")
  dane_m<-podsumuj.miesiace(dane_long)
  dane_y<-podsumuj.lata(dane_long)
  
  data <- reactive({
    zakres_dat=interval(input$zakres[1], input$zakres[2])
    #pick daily, weekly or monthly data
    if (input$okres==names(okresy)[2])       {wybor<-dane_tyg}
    else if (input$okres==names(okresy)[3])  {wybor<-dane_m}
    else if (input$okres==names(okresy)[4])  {
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
    okresy[[input$okres]]
    # if (input$okres==okresy[2])       {k<-7}
    # else if (input$okres==okresy[3])  {k<-31}
    # else if (input$okres==okresy[4])  {k<-366}
    # else {k<-1}
    # k  
  })
  
  data_with_weather <- reactive({
    zakres_dat=interval(input$zakresPogoda[1], input$zakresPogoda[2])
    
    dane_long[Data %within% zakres_dat & Miejsce %in% input$liczniki]
  })
  
  #data_hourly <- reactive({
  #  godzinowe[Miejsce %in% input$liczniki]
  #})

  callModule(bikeCountPlot, 'plotLiczba', zakres=reactive({input$zakres}), zakresOd, zakresDo, 
             liczniki=reactive({input$liczniki}), style, data=data, krok, wartosc=reactive({input$wartosc}))
  
  output$plotPogoda <- renderPlot({
    shiny::validate(
      need((input$zakresPogoda[1]>=zakresOd)&(input$zakresPogoda[2]>=zakresOd), 
           paste("Data spoza zakresu - dostępne dane od", zakresOd)),
      need((input$zakresPogoda[1]<=zakresDoPogoda)&(input$zakresPogoda[2]<=zakresDoPogoda), 
           paste("Data spoza zakresu - dostępne dane do", zakresDo)),
      need(input$zakresPogoda[1]<input$zakresPogoda[2], "Błędny zakres dat"), 
      need(input$liczniki, 'Wybierz przynajmniej jedno miejsce!')
    )
    if(input$rodzajPogody==wykresyPogody[1]) {
      pogoda_basic(data_with_weather(), paleta=style$kolory)
    } else {
      wykres_pogoda_liczba(data_with_weather(),
                           start=input$zakresPogoda[1], stop=input$zakresPogoda[2], 
                           paleta=style$kolory, linie = style$linie)
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
      
      #else add to UI
      opad=point$deszcz+point$snieg
      wellPanel(
        style = tooltip_html(tooltip_position(hover, w=180)),
        p(HTML(paste0( point$Data,": ",point$temp_avg, '&degC, ',opad,' mm, ', point$Liczba_rowerow)))
      )
  })
  
  klik<-callModule(mapa, 'mapa_licznikow', indeksy=indeksy, lokacje, style$kolory)
  
  observe({
    #print(klik())
    #updatePickerInput(session, inputId = "liczniki", selected = input$mapa_leaflet_marker_click$id)
    updatePickerInput(session, inputId = "liczniki", selected = klik())
    updateTabsetPanel(session, inputId = "zakladki", selected = "wykres")
  })
  
  #long to wide, encoding
  output$eksport <- downloadHandler(
   filename = function() { 
     paste("dane_z_licznikow_", Sys.Date(), ".csv", sep="")
   },
   content = function(file) {
     dane_do_zapisu<-dcast(data(), Data ~Miejsce, value.var="Liczba_rowerow")
     write.csv(dane_do_zapisu, file, fileEncoding = "UTF-8")
  })

}

shinyApp(ui = ui, server = server)