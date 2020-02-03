library(ggplot2)
library(data.table)

source('wykresy.R', encoding = 'UTF-8')
source('tooltip.R', encoding = 'UTF-8')
source('validators.R', encoding = 'UTF-8')


# Module UI function
  weatherPlotOutput <- function(id, label = "wykresPogodowy") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    tabsetPanel(id=ns("rodzajWykresu"),
      tabPanel("Temperatura", value = "T",
        div(id = "plotDiv", #wykres
            style = "position:relative",
            alt = "Ile rowerów w zależności od pogody",
            plotOutput(ns('plotPogodaTemperatura'), height=500, hover = hoverOpts(id = ns("plot_hover"), delay = 75)),
            uiOutput(ns("bike_weather_tooltip"))
        )
      ),
      tabPanel("Data", value = "Data",
            plotOutput(ns('plotPogodaData'), height=500)
      )
    )
  )
}

# Module server function
weatherPlot <- function(input, output, session, dane, zakresPogoda, zakresMax, liczniki, style) {
  
  data_with_weather <- reactive({
    req(zakresPogoda())
    validateLiczniki(liczniki())
    validateZakres(zakresPogoda(), zakresMax[1], zakresMax[2])
    
    zakres_dat=interval(zakresPogoda()[1], zakresPogoda()[2])
    dane[Data %within% zakres_dat & Miejsce %in% liczniki()]
  })
  
  output$plotPogodaTemperatura <- renderPlot({
    pogoda_basic(data_with_weather(), paleta=style$kolory)
  })

  output$plotPogodaData<- renderPlot({
    wykres_pogoda_liczba(data_with_weather(),
                         start=zakresPogoda()[1], stop=zakresPogoda()[2], 
                         style=style)
  })
      
  output$bike_weather_tooltip <- renderUI({
    #is mouse close to a point?
    point <- nearPoints(data_with_weather(), input$plot_hover, threshold = 8, maxpoints = 1)[ ,c("Data","temp_avg","deszcz","snieg", "Liczba_rowerow")]
    if (nrow(point) == 0) return(NULL) #jesli nie ma punktu w poblizu
    else {#else add to UI
      string<-paste0(point$Data,": ",
                     point$temp_avg, '&degC, ',
                     point$deszcz+point$snieg,' mm, ', 
                     point$Liczba_rowerow)
      tooltipWellPanel(input$plot_hover, string)
    }
  })
}
