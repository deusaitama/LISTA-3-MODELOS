passageiros<-ts(airline$Passageiros),frequency(=12,start=c(1949,1),end=)



vendas_cafe<-auscafe
vendas_cafe
autoplot(vendas_cafe)
vendas_cafe%>%decompose()%>%autoplot()

cafe_treino<-window(vendas_cafe,end=c(2009,12))
cafe_teste<-window(vendas_cafe,start=c(2010,1))

prev_naive_teste<-naive(cafe_teste)

#vendo os valores previstos:
prev_naive_teste

#Calculando o erro:
mape(prev_naive_teste$fitted[2:93],cafe_teste[2:93])*100

#grafico
autoplot(prev_naive_teste)


prev_snaive_teste<-snaive(cafe_teste)
prev_snaive_teste


#calculo do erro
mape(prev_snaive_teste$fitted[13:93],cafe_teste[13:93])*100
autoplot(prev_snaive_teste)
sazonal 13
autoplot(prev_snaive_teste)


aes_treino<-ses(cafe_treino)
summary(aes_treino)

alfa_aes<-0.4936

prev_aes_teste<-ses(cafe_teste,alpha=alfa_aes)
prev_aes_teste
mape(prev_aes_teste$fitted,cafe_teste)*100
autoplot(prev_aes_teste)

janela<-2
media_movel_treino<-SMA(cafe_treino,janela)
mape(cafe_treino[janela:333],media_movel_treino[janela:333])*100

# previsões na amostra de teste
prev_media_movel<-SMA(cafe_teste,janela)
prev_media_movel


#erro
mape(cafe_teste[janela:93],prev_media_movel[janela:93])*100



aes_treino<-ses(cafe_treino)
summary(aes_treino)

alfa_aes<-0.1562

#previsões ma amostra de teste
prev_aes_teste<-ses(cafe_teste,alpha=alfa_aes)

#erro
mape(prev_aes_teste$fitted,cafe_teste)*100

#grafico
autoplot(prev_aes_teste)

################################################
holt_treino<-holt(cafe_treino)
summary(holt_treino)

alfa_holt<-0.4033 
beta_holt<-1e-04
#previsões na amostra de teste
prev_holt_test<-holt(cafe_teste,alpha=alfa_holt,beta=beta_holt)
prev_holt_test

#erro
mape(prev_holt_test$fitted,cafe_teste)*100

autoplot(prev_holt_test)

