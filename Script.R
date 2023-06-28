library(forecast)
library(readxl)
library(yfR)


nome_acao <- "SEQL3.SA"   # Código no Yahoo Finance
data_ini  <- "2023-01-01" # Data de inicio
data_fim  <-  Sys.Date() # Data de fim


precos <- yf_get(tickers = nome_acao, first_date = data_ini, last_date = data_fim) 

precos <- precos %>% select(ref_date,price_adjusted)   # banco de dados filtrado

#igualando e juntando os data frames

df <- data.frame(diff(precos$price_adjusted))

df[nrow(df) + 1,] <- c(0)

colnames(df) <- c("valores")

precos <- cbind(precos,df)


#modelo e previsão

fit <- Arima(precos$valores, order=c(0,1,3) , include.mean = T)

df <- forecast(fit,h=1)$mean[1]



df <- data.frame(df)   #transformando em data frame

for (i in 1:119) {
df[nrow(df) + 1,] <- c(0)
}                           #igualando os data frames

precos <- cbind(precos,df) # Banco 

path=file.path("C:/Users/Windows/Desktop/Estatística/Series Temporais/")

precos_orig <- read_excel(file.path(path,"precos_orig"))

precos[nrow(precos) + 1,] <- precos_orig[1,] 

write.csv(precos, "C:\\Users\\Windows\\Desktop\\Estatística\\Series Temporais\\precos_orig.csv", row.names=FALSE)