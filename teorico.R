install.packages("dplyr")
install.packages("forecast")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("Metrics")
install.packages("tsibble")
install.packages("tsibbledata")
install.packages("TTR")
install.packages("fpp2")
library(dplyr)
library(fpp2)
library(forecast)
library(ggplot2)
library(lubridate)
library(Metrics)
library(tsibble)
library(tsibbledata)
library(TTR)


#1 carregamento e analise dos dados

vendas_cerveja<-ausbeer

vendas_cerveja
print(vendas_cerveja)

#grafico de linha
autoplot(vendas_cerveja)

#decomposição da série

vendas_cerveja%>%decompose()%>%autoplot()




#II Métodos de modelos de previsão
#Divisão de dados
0.8*length(vendas_cerveja)
174/4

cerveja_treino<-window(vendas_cerveja,end=c(1999,4))
cerveja_teste<-window(vendas_cerveja,start=c(2000,1))

#1-Previsor Naive

prev_naive_teste<-naive(cerveja_teste)

#vendo os valores previstos:
prev_naive_teste

#Calculando o erro:
mape(prev_naive_teste$fitted[2:42],cerveja_teste[2:42])*100

#grafico
autoplot(prev_naive_teste)

#2-previsor naive sazonal

prev_snaive_teste<-snaive(cerveja_teste)
prev_snaive_teste


#calculo do erro
mape(prev_snaive_teste$fitted[5:42],cerveja_teste[5:42])*100
autoplot(prev_snaive_teste)



#3 previsor de medias moveis
#escolha da janela(escolher a janela com menor MAPE)
janela<-2
media_movel_treino<-SMA(cerveja_treino,janela)
mape(cerveja_treino[janela:176],media_movel_treino[janela:176])*100

# previsões na amostra de teste
prev_media_movel<-SMA(cerveja_teste,janela)


#erro
mape(cerveja_teste[janela:42],prev_media_movel[janela:42])*100


#4- Amortecimento exponencial simples

#Treino(encontrando alfa)

aes_treino<-ses(cerveja_treino)
summary(aes_treino)

alfa_aes<-0.1562

#previsões ma amostra de teste
prev_aes_teste<-ses(cerveja_teste,alpha=alfa_aes)

#erro
mape(prev_aes_teste$fitted,cerveja_teste)*100

#grafico
autoplot(prev_aes_teste)

#Método de holt(tendencia)

holt_treino<-holt(cerveja_treino)
summary(holt_treino)

alfa_holt<-0.0643
beta_holt<-0.0249
#previsões na amostra de teste
prev_holt_test<-holt(cerveja_teste,alpha=alfa_holt,beta=beta_holt)

#erro
mape(prev_holt_test$fitted,cerveja_teste)*100

autoplot(prev_holt_test)


#Metodo de Holt-Winters (tendencia e sazonalidade)
#encontrando os valores de alfa, beta e gama
hw_treino<-hw(cerveja_treino)
summary(hw_treino)

alfa_hw<-0.2284
beta_hw<-0.0375
gama_hw<-0.2031

#previsões na amostra de teste
prev_hw_teste<-hw(cerveja_teste,alpha = alfa_hw,beta=beta_hw,gamma=gama_hw)


#erro
mape(prev_hw_teste$fitted,cerveja_teste)*100

#gráfico
autoplot(prev_hw_teste)

#vendo os números
prev_hw_teste








