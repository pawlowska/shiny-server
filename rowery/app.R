library(shiny)

library(Cairo) #for nice looks of the graph
options(shiny.usecairo=T)

source('ladowanie_danych.R', encoding = 'UTF-8')
source('wykresy.R', encoding = 'UTF-8')

dane_polaczone<-wczytaj_dane()
nazwy<-names(dane_polaczone)[2:20]
dane_long<-raw_to_long(dane_polaczone)

zakresOd=  '2014-08-01'
zakresOdPokaz='2016-01-01'
zakresDo = '2017-01-26'

ui <- fluidPage(
  headerPanel('Liczba rowerów'),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput('zakres', 'Wybierz zakres dat', 
                     start=zakresOdPokaz, end=zakresDo, min=zakresOd, max=zakresDo,
                     separator = 'do', weekstart = 0, language = "pl"),
      checkboxGroupInput('liczniki', 'Wybierz miejsca', nazwy, 
                         selected = nazwy[sample(1:length(nazwy),5)], inline = FALSE, width = NULL)
  ),
    mainPanel(
      tags$p(paste('Dane z automatycznych liczników rowerów ZDM z okresu od ', zakresOd, ' do ', zakresDo)),
      tags$p(""),
      plotOutput('plot1', height=500, hover = "plot_hover"),
      # verbatimTextOutput("info"),
      
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
  output$plot1 <- renderPlot({
    uzyte_kolory<-kolory[indeksy()]
    wykres_kilka(dane_long, input$liczniki, 
                 start=input$zakres[1], stop=input$zakres[2], paleta=uzyte_kolory)
  })
  
  # output$info <- renderText({
  #   xy_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
  #   }
  #   xy_range_str <- function(e) {
  #     if(is.null(e)) return("NULL\n")
  #     paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
  #            " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
  #   }
  #   
  #   paste0(
  #     "hover: ", xy_str(input$plot_hover)
  #   )
  # })
}

shinyApp(ui = ui, server = server)