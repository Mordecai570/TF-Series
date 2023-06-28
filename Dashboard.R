library(bslib)
library(shinythemes)
library(shinydashboard)
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(readxl)
library(plotly)
library(hrbrthemes)
library(thematic)
library(formattable)
library(forecast)
library(yfR)

path=file.path("C:/Users/Windows/Desktop/Estatística/Series Temporais/")

precos <- read_excel(file.path(path,"precos_orig"))


ui <-  navbarPage(selected = "precos",theme = shinytheme("cerulean"),
          "ME607 - Séries",
          tabPanel("Introdução", "Sequoia Ltda é uma empresa de distribuição logistica brasileira fundada em 2010. Nesse trabalho iremos investigar as ações da empresa no período em que se aprensentou um declínio para elas.",br(),br(),"A motivação por trás desse estudo vem tanto da análise direta sobre os valores da ação para investimento e também sobre o impacto das ações politicas em um aspecto econômico.",br(),br(),"Um dos maiores sites de vendas online hoje no Brasil é a",
                   strong("Shopee"),
                   "criação da empresa chinesa Sea Group, que durante a pandemia se tornou uma febre , tanto por conta do fechamento das lojas físicas quanto seus preços baixos. A",strong("Shopee"),"se tornou um dos grandes",
               em("ecomerces"),
               "do Brasil.",br(),br(),"A Sequoia sendo uma das empresas parceiras logistica da Shopee teve seu auje junto da mesma durante a pandemia e por conta dessa mudança de paradigima das compras feitas no Brasil como descrito no artigo de Pignati Giovana a Shopee teve um grande aumento em suas ações."),
          navbarMenu("Gráficos",
                     tabPanel("Gráfico 1", plotOutput("plot1")),
                     tabPanel("Gráfico 2", "three-b",plotOutput("plot2")),
                     tabPanel("Gráfico 3", "3a",plotOutput("plot3")),
                     tabPanel("Gráfico 4", "three-b",plotOutput("plot4")),
                     tabPanel("Gráfico 5", "2a",plotOutput("plot5")),
                     tabPanel("Gráfico 6", "three-b",plotOutput("plot6")),
                     tabPanel("Estacionáridade", "O gráfico nos mostra para diferente valores de lag, o p-valor resultante",plotOutput("plot7"))
          
      ),
          navbarMenu("Modelagem", 
                     tabPanel("Modelo",
                     sidebarPanel(
                       sliderInput(
                         "MA_1", label = "Escolha do primeiro parâmetro MA:",
                         min = -3.8025, max = -1.8025, value = -2.8025 , step = 0.2
                       ),
                       sliderInput(
                         "MA_2", label = "Escolha do segundo parâmetro MA:",
                         min =  -1.8114, max = 0.8114, value =  2.6143 , step = 0.2
                       ),
                       sliderInput(
                         "MA_3", label = "Escolha do terceiro parâmetro MA:",
                         min = 0.2, max = 2, value = -0.8114, step = 0.2
                       ),
                       
                       
                       sliderInput(
                         "AR_1", label = "Escolha do primeiro parâmetro AR:",
                         min =  -0.0170, max =  1.0170, value = 0.0170, step = 0.2
                       ),
                       sliderInput(
                         "AR_2", label = "Escolha do segundo parâmetro AR:",
                         min =  -1.1266, max = 0.1266, value = -0.1266, step = 0.2
                       ),
                       sliderInput(
                         "AR_3", label = "Escolha do teriro parâmetro AR:",
                         min = -0.0544, max = 1.0544, value = 0.0544, step = 0.2
                       )
                     ),
                     
                     mainPanel(
                       plotOutput("modelo")
                     ),
              ),
                tabPanel("Diagnostico")
              
)
  
)


server <- function(input, output, session) {
  thematic::thematic_shiny()
  

  # Grafico 1
  output$plot1 <- renderPlot({
    ggplot(precos) + 
      geom_line(aes(x =ref_date, y = price_adjusted), color = "green4")  + 
      ylab('Valores Ajustados') + xlab("Data")
  }, res = 96)
  
  #grafico 2
  output$plot2 <- renderPlot({
    acf(precos$price_adjusted)
  }, res = 96)
  
  # gráfico 3
  output$plot3 <- renderPlot({
    pacf(precos$price_adjusted)
  }, res = 96)
  
  # grafico 4
  output$plot4 <- renderPlot({
    ggplot(precos) + 
      geom_line(aes(x =ref_date, y = valores), color = "green4")  + 
      ylab('Valores Ajustados') + xlab("Data")
  }, res = 96)
  
  # grafico 5
  output$plot5 <- renderPlot({
    acf(precos$valores)
  }, res = 96)
  
  # grafico 6
  output$plot6 <- renderPlot({
    pacf(precos$valores)
  }, res = 96)
  
  p_valor<-c()
  for (i in 1:50) {
    
    x<-Box.test(precos$valores, lag = i, type = "Ljung-Box")
    p_valor[i]<-x$p.value  
  }
  
  # grafico 7
  output$plot7 <- renderPlot({
    plot(y= p_valor, x=1:50)
    lines(x= 1:50 , y=rep(0.05,times=50))
  }, res = 96)
  
  #fit
  fit <- Arima(precos$valores, order=c(0,1,3) , include.mean = T)
  
  #modelo
  output$modelo <- renderPlot({
    plot(arima.sim(model= list( ma = c(input$MA_1,input$MA_2,input$MA_3),ar = c(input$AR_1,input$AR_2,input$AR_3)) , n=249))+
      lines(precos)
  }, res = 96)
  
}

shinyApp(ui,server)
