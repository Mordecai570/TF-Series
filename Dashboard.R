library(bslib)
library(DT)
library(shinythemes)
library(shinydashboard)
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(plotly)
library(hrbrthemes)
library(thematic)
library(formattable)
library(forecast)
library(yfR)


precos <- read.csv("precos_orig.csv"))

prev <- c()  
diferencas<-c()

fit <- Arima(precos$valores, order=c(0,1,3) , include.mean = T)

for (i in 100:length(precos)){
  fit <- Arima(precos$valores[1:i], order=c(0,1,3) , include.mean = F)
  prev[i] <- forecast(fit,h=1)$mean[1]
  diferencas[i]<- abs(prev[i] - precos$valores[i +1])
}

c <- data.frame(mean(diferencas, na.rm = T))
colnames(c) <- c("Acurácia")

a <- forecast(fit,h=1)
a <- data.frame(a)

ui <-  navbarPage(selected = "precos",theme = shinytheme("cerulean"),
          "ME607 - Séries",
          tabPanel("Introdução", "Sequoia Ltda é uma empresa de distribuição logistica brasileira fundada em 2010. Nesse trabalho iremos investigar as ações da empresa no período em que se aprensentou um declínio para elas.",br(),br(),"A motivação por trás desse estudo vem tanto da análise direta sobre os valores da ação para investimento e também sobre o impacto das ações politicas em um aspecto econômico.",br(),br(),"Um dos maiores sites de vendas online hoje no Brasil é a",
                   strong("Shopee"),
                   "criação da empresa chinesa Sea Group, que durante a pandemia se tornou uma febre , tanto por conta do fechamento das lojas físicas quanto seus preços baixos. A",strong("Shopee"),"se tornou um dos grandes",
               em("ecomerces"),
               "do Brasil.",br(),br(),"A Sequoia sendo uma das empresas parceiras logistica da Shopee teve seu auje junto da mesma durante a pandemia e por conta dessa mudança de paradigima das compras feitas no Brasil como descrito no artigo de Pignati Giovana a Shopee teve um grande aumento em suas ações."),
          tabPanel("Análise",
                     tabBox(
                       side = "right", height = "250px",
                       tabPanel("Gráfico 1", plotOutput("plot1")),
                       tabPanel("Gráfico 2", plotOutput("plot2")),
                       tabPanel("Gráfico 3", plotOutput("plot3")),
                       tabPanel("Gráfico 4", plotOutput("plot4")),
                       tabPanel("Gráfico 5", plotOutput("plot5")),
                       tabPanel("Gráfico 6", plotOutput("plot6")),
                       tabPanel("Estacionáridade", "O gráfico nos mostra para diferente valores de lag, o p-valor resultante",plotOutput("plot7"))
                     ),
                     tabBox(
                       side = "right", height = "250px",
                       tabPanel(" ", "A série possui uma tendência de descreminto, como já mencionada anteriormente, além disso não visualiza-se uma sazionalidade clara.", br(),br()," Olhando aos gráficos de ACF (gráfico 2)  e PACF (gráfico 3) observa-se um padrão de uma série não estacionária.",br(),
                                br(),"O gráfico 4 nos mostra a nossa série diferenciada, onde tem um comportamento razoavelmente estacionário com alguns pontos que se destoantes dos outros",br(), "Nesses pontos vemos pelo gráfico de ACF (grafico 5) que a apenas a terça auto-correlação é destoante, enquanto as outras são baixas com um comportamento de decaimento que não nos permite determinar diretamente se é apenas um AR ou MA. O comportamento do PACF (gráfico 6) é similiar ao do nosso ACF (gráfico 5), sendo a terceira autocorrelação parcial destoante e com padrão de decrescimento dificultando a indentificação.",br(),br(), "Certos valores indicam uma sazionalidade pelas autocorrelações, mas por serem valores abaixos da linha de estacionáridade não iremos focar nessas interações e focaremos nas carcteristicas principais da série.", br(),br(), "Ao compararmos o gráfico da série diferenciada (gráfico 4) vemos pequenas sazionalidades possivelmente descartáveis.",br(),br(), "Utilizando o teste de Ljung-Box (estacionaridade), temos como evidência que nossa série diferenciada é estacionária, logo um modelo ARIMA é razoável, dado os padrões observados nos gráficos 5 e 6, não podemos decarta uma influência AR(1).", br(),br(), "Escolhemos esse valor por conta de não termos nenhuma evidencia de um valor para o parâmetro do modelo autoregressivo, mais significativa, ou MA(3), utilizamos o parametro MA pois no gráfico PACF (grafico 6) temos um grande valor para a autocorrelação parcial 3 e um decaimento", em("suave") ,", que é bem diferente de um comportamento AR(3) que teriamos um decaimento abrupto.") 
               )
        ),
          navbarMenu("Modelagem", 
                     tabPanel("Modelo","Segundo nossa análise exploratória nossa série é modelada por uma série ARIMA(0,1,3), para estimarmos os paramêtros utilizamos o método maximização  da log-verossimilhança condicional.",br(),br(),
                     "Com esses resultados temos uma estimação dos paramêtros da nossa serie , fazendo uma simulação com tais valores e os variando . Obtemos o seguinte gráfico interativo.",br(),br(),
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
                tabPanel("Diagnostico", "Com os principais métodos de diagnosticos para os resíduos do modelo temos:",br(),br(),plotOutput("residuos"), br(),br(),br(), "Vendo os principais gráficos para diagnósticos de resíduos temos que o comportamento deles é o desejado, a série parece ruído branco. As autocorrelações estão dentro das delimitações de interesse e o histograma dos resíduos tem formato normal com média 0.",br(), br(),"Ou seja os seja podemos ver que os resíduos tem um comportamento adequado ao que se deseja para termos uma boa aproximação utilizando nossa série modelada. E além disso o teste de Box-Lung nos da evidência a favor da estacionáridade da série de resíduos.",br(), br())
              
         ),
            tabPanel("Previsões e Validação Cruzada", "Para o cálculo da validação cruzada utilizamos o método de abertura de janelas aonde nosso conjunto inicial de treinamento é igual a 100.",br(),br(),datatable(c, options = list(paging = FALSE,    ## paginate the output
                                                             pageLength = 1,  ## number of rows to output for each page
                                                             scrollX = FALSE,   ## enable scrolling on X axis
                                                             scrollY = FALSE,   ## enable scrolling on Y axis
                                                             autoWidth = FALSE,
                                                             searching = FALSE),
                                           filter = c("none")), 
                     br(),br(),"Como o valor da acuracia é pequeno, consideramos valor muito bom indicando que nosso modelo tem um boa capacidade de previsão.",br(),br(),"Para realizarmos as previsões de um período após, para nossa série . Utilizamos que a previsão nada é que o valor esperado da observação t+1 ,condicionada que se tem os valores das outras t observações da série . Utilizamos para esse cálculo a função forecast que possui como justificativa teorica , esses conceitos sobre previsão.",
                     datatable(a, options = list(paging = FALSE,    ## paginate the output
                                                 pageLength = 1,  ## number of rows to output for each page
                                                 scrollX = FALSE,   ## enable scrolling on X axis
                                                 scrollY = FALSE,   ## enable scrolling on Y axis
                                                 autoWidth = FALSE,
                                                 searching = FALSE),
                               filter = c("none"))
                     
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
  
  #residuos
  output$residuos <- renderPlot({
    checkresiduals(fit)
  }, res = 96)
  
  prev <- c()
  
  diferencas<-c()
  
  for (i in 100:length(precos)){
    
    fit <- Arima(precos$valores[1:i], order=c(0,1,3) , include.mean = F)
    prev[i] <- forecast(fit,h=1)$mean[1]
    diferencas[i]<- abs(prev[i] - precos$valores[i +1])
  }
  
  
    output$c <- renderDataTable({ 
    c <- data.frame(mean(diferencas, na.rm = T))
    colnames(c) <- c("Acurácia")
  })

    output$a <- renderDataTable({ 
  a <- forecast(fit,h=1)
  a <- data.frame(a)
  })

}

shinyApp(ui,server)
