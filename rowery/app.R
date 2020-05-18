library(shiny)

library(Cairo) #for nice looks of the graph
options(shiny.usecairo=T)

library(lubridate)
library(shinyWidgets)
library(shinyBS)

source('ladowanie_danych.R', encoding = 'UTF-8')
source('obsluga_sumowania.R', encoding = 'UTF-8')
source('text.R', encoding = 'UTF-8')
source('mapaModule.R', encoding = 'UTF-8')
source('bikeCountPlotModule.R', encoding = 'UTF-8')
source('dateWithButtonModule.R', encoding = 'UTF-8')
source('weatherPanelModule.R', encoding = 'UTF-8')

Sys.setlocale("LC_ALL", "Polish")

katalog="pliki"
miasto="Warszawa"
tytul='Liczniki rowerów w Warszawie'

#reading locations
lokacje <- read.csv(paste(katalog, "polozenie_licznikow.csv", sep="/"),dec=".", encoding='UTF-8')
lokacje<-data.table(lokacje)

#reading colors etc
style<-wczytaj_style(katalog)

#dane dostępne do: 10.05.2020, pogoda do kwietnia
#reading data
dane_long<-wczytaj_dane(paste(katalog, "dane_long.csv", sep="/"))
nazwy<-unique(dane_long[,Miejsce])
zakresOd=  min(dane_long[,Data])
zakresDo = max(dane_long[,Data])
plik_pogoda=paste(katalog, "IMGW_2014_do2020_04.csv", sep="/")
temp<-fread(plik_pogoda)
zakresDoPogoda=as.character(max(temp[,Data]))

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

  headerPanel(tytul),
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
                    splitLayout(cellWidths = c("90%","10%"),
                                cellArgs = list(style = " display: inline-block; vertical-align: bottom;"),
                        dateWithButtonInput('zakresW'),
                        downloadButton(outputId = "eksport", label="", style = "margin-bottom: 15px;" )
                    )
                ),
                column(6,
                    splitLayout(
                        selectInput('okres', 'Podsumuj', names(okresy), selected = names(okresy)[1]),
                        selectInput('wartosc', 'Wartości', wartosci, selected = wartosci[1])
                    )
                ) #end fluidRow
                ), style= "padding: 10px 10px 0px 15px;"), #end wellPanel
                bikeCountPlotOutput('plotLiczba')
        ),
        tabPanel("Pogoda", value="pogoda",
                 weatherPanelOutput('pogoda_panel')
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
      else if (!is.null(query$licznik)) {
        if (query$licznik=='all') { #all
          c(1:length(nazwy))
        } else { #one
          m<-lokacje[id==query$licznik]$Miejsce
          match(m, nazwy)
        }
      }
    }
     else match(unique(data()$Miejsce), nazwy)
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

  zakres<-callModule(dateWithButton, 'zakresW', dane=dane_long, liczniki=reactive(input$liczniki),
                     zakresMax=c(od=as.Date(zakresOd), do=Sys.Date()-1))
  
  callModule(weatherPanel, 'pogoda_panel', dane=dane_long, liczniki=reactive(input$liczniki),
             zakresMax=c(od=as.Date(zakresOd), do=as.Date(zakresDoPogoda)), style)
  
  #aktualizacja danych
  #dane_long<-dodaj_nowe_dane(stare=dane_long, p=(paste(katalog, "nowe_long.csv", sep="/")),
  #                           plik_pogoda=plik_pogoda, lokacje=lokacje, zakresDo=zakresDo, miasto)

  #aktualizacja daty 
  zakresDo<-as.character(Sys.Date()-1)

  #podsumuj
  dane_tyg<-podsumuj.okresy(dane_long, "startTyg")
  dane_m<-podsumuj.miesiace(dane_long)
  dane_y<-podsumuj.lata(dane_long)

  data <- reactive({
    req(zakres())
    zakres_dat=interval(zakres()[1], zakres()[2])
    #pick daily, weekly or monthly data
    if (input$okres==names(okresy)[2])       {wybor<-dane_tyg}
    else if (input$okres==names(okresy)[3])  {wybor<-dane_m}
    else if (input$okres==names(okresy)[4])  {
      zakres_dat=interval(as.Date("2014-01-01"), zakres()[2])
      wybor<-dane_y
    } else {wybor<-dane_long}

    #PZ
    if(input$wartosc == wartosci[1])
      wybor[Data %within% zakres_dat & Miejsce %in% input$liczniki]
    else
      podsumuj.procentowo(wybor[Data %within% zakres_dat & Miejsce %in% input$liczniki])
  })

  callModule(bikeCountPlot, 'plotLiczba',
             zakres=zakres, zakresOd, zakresDo,
             liczniki=reactive({input$liczniki}), style, data=data,
             krok=reactive({okresy[[input$okres]]}), wartosc=reactive({input$wartosc}))

  #data_hourly <- reactive({
  #  godzinowe[Miejsce %in% input$liczniki]
  #})

  #output$plotHours <- renderPlot({
  #  shiny::validate(
  #    need(input$liczniki, 'Wybierz przynajmniej jedno miejsce!'))
  #  wykres_godzinowy(data_hourly(), paleta=uzyte_kolory(), linie = uzyte_linie())
  #})

  klik<-callModule(mapa, 'mapa_licznikow', indeksy=indeksy, lokacje, style$kolory)

  observe({
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