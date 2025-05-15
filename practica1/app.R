library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)

df<-read.csv("C:/Users/aritz/Downloads/Global_Mobility_Report.csv")
df$date <- ymd(df$date)

ui <- fluidPage(
  titlePanel('Movilidad del sector minorista y de recreacion en 2020'),
  selectInput(inputId = 'pais', label = 'selecciona un pais:', choices = unique(df$country_region)),
  plotOutput(outputId = 'grafico_evolucion')
)

server <- function(input,output) {
  output$grafico_evolucion <- renderPlot({
    df2 <- filter(df, country_region == input$pais)
    ggplot(df2, aes(date, retail_and_recreation_percent_change_from_baseline)) +
      geom_line(color = 'royalblue') + theme_bw()
  })
}

shinyApp(ui = ui, server = server)
