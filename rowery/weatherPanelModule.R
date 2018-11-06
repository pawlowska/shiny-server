source('dateWithButtonModule.R', encoding = 'UTF-8')
source('weatherPlotModule.R', encoding = 'UTF-8')

weatherPanelOutput<-function(id, label="panel_pogody") {
  ns <- NS(id)
  tagList(
    wellPanel(dateWithButtonInput(ns('zakres')), style= "padding: 5px 0px 0px 15px;"), #end wellPanel
    weatherPlotOutput(ns('plot'))
  )
}


weatherPanel <- function(input, output, session, 
                         dane, liczniki, zakresMax, style) {

  zakresPogoda<-callModule(dateWithButton, 'zakres', 
                           dane, liczniki, zakresMax)

  callModule(weatherPlot, 'plot', 
             dane=dane, zakresPogoda=zakresPogoda, zakresMax, liczniki, style)
}
  