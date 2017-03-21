library(shiny)

library(Cairo) #for nice looks of the graph
options(shiny.usecairo=T)

library(lubridate)

source('ladowanie_danych.R', encoding = 'UTF-8')
source('wykresy.R', encoding = 'UTF-8')

dane_polaczone<-wczytaj_dane() #wczytuje wstepnie obrobione dane z csv

dane_polaczone<-suma_licznikow(dane_polaczone)

nazwy<-names(dane_polaczone)[4:ncol(dane_polaczone)]

dane_long<-wide_to_long(dane_polaczone)
dane_tyg<-podsumuj.tygodnie(dane_long)
dane_m<-podsumuj.miesiace(dane_long)

zakresOd=  '2014-08-01'
zakresOdPokaz='2017-01-01'
#zakresDo = '2017-01-26'
zakresDo = '2017-03-19'


okresy = c('dobowo', 'tygodniowo', 'miesięcznie')

ile_licznikow<-19
ile_sum<-3
ile<-ile_licznikow-ile_sum

nazwy_licznikow<-nazwy[1:ile_licznikow]
nazwy_sum<-nazwy[(ile_licznikow+1):(ile_licznikow+ile_sum)]

ui <- fluidPage(
  headerPanel('Liczba rowerów'),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('zakres', 'Wybierz zakres dat', 
                     start=zakresOdPokaz, end=zakresDo, min=zakresOd, max=zakresDo,
                     separator = 'do', weekstart = 0, language = "pl"),
      selectInput('okres', 'Podsumuj', okresy, selected = okresy[2], multiple = FALSE,
                  selectize = TRUE, width = NULL, size = NULL),
      checkboxGroupInput('liczniki', 'Wybierz miejsca', nazwy, 
                         selected = nazwy[sample(1:ile, 3)], inline = FALSE, width = NULL)
  ),
    mainPanel(
      tags$p(paste('Dane z automatycznych liczników rowerów ZDM z okresu od ', zakresOd, ' do ', zakresDo)),
      tags$p(""),
      div(
        id = "plotDiv",
        style = "position:relative",

        plotOutput('plot1', height=500, hover = hoverOpts(id = "plot_hover", delay = 100)),
        uiOutput("my_tooltip")
      ),
      
      hr(),
      tags$p(
        'Dane: ',
        tags$a(href='https://zdm.waw.pl', "Zarząd Dróg Miejskich w Warszawie"),
        '(otrzymane mailem). Aplikacja: Monika Pawłowska',
        tags$a(href='rowery@greenelephant.pl', "rowery@greenelephant.pl")
        )
    ) #end mainPanel
  )
) #end ui

server <- function(input, output) {
  indeksy<-reactive({ #ktore kolory beda uzyte
    match(input$liczniki, nazwy)
    })
  
  data <- reactive({
    zakres_dat=interval(input$zakres[1], input$zakres[2])
    if (input$okres==okresy[2]) {
      dane_tyg[Data %within% zakres_dat & Miejsce %in% input$liczniki]
      }
    else if (input$okres==okresy[3]) {
      dane_m[Data %within% zakres_dat & Miejsce %in% input$liczniki]
      }
    else {
      dane_long[Data %within% zakres_dat & Miejsce %in% input$liczniki]
      }
  })
  
  output$plot1 <- renderPlot({
    uzyte_kolory<-kolory[indeksy()]
    uzyte_linie <-lista_linii[indeksy()]
    wykres_kilka(data(), 
                 start=input$zakres[1], stop=input$zakres[2], paleta=uzyte_kolory, linie = uzyte_linie)
  })
  
  output$my_tooltip <- renderUI({
    #based on: http://stackoverflow.com/questions/38992270/r-shiny-tooltip-in-ggplot
    
    hover <- input$plot_hover 
    point <- nearPoints(data(), hover, threshold = 8, maxpoints = 1)[ ,c("Data", "Liczba_rowerow")]
    if (nrow(point) == 0) return(NULL) #jesli nie ma punktu w poblizu

    #calculate the position of the tooltip
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)   
    
    style <- paste0("position:absolute; z-index:100; padding: 0 6px 0 6px; height: 22px; overflow: hidden;
                    background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    wellPanel(
      style = style,
      p(HTML(paste0( point$Data,": ", point$Liczba_rowerow)))
    )
  })
}

shinyApp(ui = ui, server = server)