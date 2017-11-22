library(tseries)
library(forecast)
library(vars)
library(stargazer)
library(dynlm)
library(sandwich)


setwd("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut9")


#resolvendo o problema que o dick teve no ultimo tutorial
#objetos do tipo fator são um desgraça
#além disso o ultimo elemento era "ibge", dai quando converteu ele virou um NA
indust <- read.csv2("ProdIndustrial.csv")
indust <- as.numeric(gsub(",", ".", as.character(indust[,2])))
indust <- indust[!is.na(indust)]


#baixar duas series faz elas serem separadas por TAB, que o R lê como \t
#mesmos problemas que a serie anterior exceto que desse ves os NAs são por causa da diferença do IPCA e do INPC
inflacao <- read.csv("inflacao.csv", sep = "\t")
inflacao <- as.numeric(gsub(",", ".", as.character(inflacao[,3])))
inflacao <- inflacao[!is.na(inflacao)]


#mesma coisa pra selic mensal
juros <- read.csv2("selic.csv")
juros <- as.numeric(gsub(",", ".", as.character(juros[,2])))
juros <- juros[!is.na(juros)]


#mesma coisa pra ibovespa
ibovespa <- read.csv2("ibovespa.csv")
ibovespa <- as.numeric(gsub(",", ".", as.character(ibovespa[,2])))
ibovespa <- ibovespa[!is.na(ibovespa)]


#mesma coisa pra commodity
commodity <- read.csv2("commodity.csv")
commodity <- as.numeric(gsub(",", ".", as.character(commodity[,2])))
commodity <- commodity[!is.na(commodity)]


#trabalhar com objetos TS é mais facil então...
commodity <- ts(commodity, start = c(1998, 1), frequency = 12)
ibovespa <- ts(ibovespa, start = c(1987, 1), frequency = 12)
juros <- ts(juros, start = c(1986, 7), frequency = 12)
inflacao <- ts(inflacao, start = c(1980, 1), frequency = 12)
indust <- ts(indust, start = c(2002, 1), frequency = 12)


#masini de ultima hora mudou a variavel de interesse pra variação do índice de produção industrial
indust <- diff(indust)


#contruindo a matriz pra usar no modelo VAR e conrtando as variaveis pra ficarem do mesmo length
df <- ts.intersect(commodity, ibovespa, juros, inflacao, indust)


#sempre bom ver os dados
plot(df[,1]) #commodity
plot(df[,2]) #ibovespa
plot(df[,3]) #juros
plot(df[,4]) #inflacao
plot(df[,5]) #indust


#vamo ver se esses trem são estacionario
apply(df, 2, adf.test)
apply(df, 2, Box.test)


#tem alguns jeitos de arrumar
plot(diff(df[,1])) #commodity
plot(diff(df[,3])) #juros
#ou assim?
plot(decompose(df[,1])$random) #commodity
plot(decompose(df[,3])$random) #juros


#tirando a diferença da pra entender melhor
commodity <- diff(commodity)
juros <- diff(juros)


#ajustando a matriz pras novas variaveis
df <- ts.intersect(commodity, ibovespa, juros, inflacao, indust)
commodity <- df[,1]
ibovespa <- df[,2]
juros <- df[,3]
inflacao <- df[,4]


#vamo ver se agora esses trem tão bom
apply(df, 2, adf.test)
apply(df, 2, Box.test)


#### parte 1 ####
auto.arima(indust)


#### parte 2 ####
#estimando um modelo VAR
#A função VAR faz a mesma coisa que estimar um bom e velho LM só que pra cada variavel, exemplo:
fit11 <- VAR(df, p = 1)
fit12 <- lm(indust[2:188] ~ indust[1:187] + commodity[1:187] + ibovespa[1:187] + inflacao[1:187] + juros[1:187])

fit21 <- VAR(df, p = 2)
fit22 <- lm(indust[3:188] ~ indust[2:187] + commodity[2:187] + ibovespa[2:187] + inflacao[2:187] + juros[2:187] +
     indust[1:186] + commodity[1:186] + ibovespa[1:186] + inflacao[1:186] + juros[1:186])


#deixando mais bonito pra comparar
names(fit12$coefficients) <- c("const", "indust.l1", "commodity.l1", "ibovespa.l1", "inflacao.l1",  "juros.l1")
stargazer(fit11$varresult$indust ,fit12 ,type = "text")


names(fit22$coefficients) <- c("const", "indust.l1", "commodity.l1", "ibovespa.l1", "inflacao.l1",  "juros.l1",
                               "indust.l2", "commodity.l2", "ibovespa.l2", "inflacao.l2",  "juros.l2")
stargazer(fit21$varresult$indust, fit22, type = "text")


#tb tem esse package que calcula pela decomposição QR:
dynlm(indust ~ lag(indust) + lag(commodity) + lag(ibovespa) + lag(inflacao) + lag(juros))


#nem precisa corrigir quando vc só quer previsão, mas o masini gosta de reclamar então, ta ai um newey-west
coeftest(fit12, vcov. = NeweyWest)


#achando o melhor VAR do jeto aic
MAIC <- matrix(0,24,1)
for (k in 1:24){
  fitXXX <- VAR(df, p = k)
  MAIC[k,1] <- AIC(fitXXX)
}
which.min(MAIC)
plot(MAIC)

MBIC <- matrix(0, 24, 1)
for (k in 1:24){
  fitXXX <- VAR(df, p = k)
  MBIC[k,1] <- BIC(fitXXX)
}
which.min(MBIC)
plot(MBIC)
#lembrando da formula do AIC
#sepa que a likelihood ta caindo mais rapido que a penalização pelo numero de coeficientes
#mas ainda assim mais variaveis extras tão colocando um puta viés no modelo
#e o poder preditivo vai pro caralho



###############################################################
###################  Cross validando  #########################
###############################################################

df <- ts.intersect(commodity, ibovespa, juros, inflacao, indust)
fit000 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 5)
#fazendo pras outras combinações
#pra achar o numero de combinações possiveis lembramos da fomrula de combinações
#c = n!/(p!(n-p)!)
#no caso vão ser:
#4!/(1!(3!)) + 4!/(2!(2!)) + 4!/(3!(1!)) = 14
df <- ts.intersect(commodity, indust)
fit001 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 2)

df <- ts.intersect(ibovespa, indust)
fit002 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 2)

df <- ts.intersect(juros, indust)
fit003 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 2)

df <- ts.intersect(inflacao, indust)
fit004 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 2)

df <- ts.intersect(commodity, ibovespa, indust)
fit005 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 3)

df <- ts.intersect(commodity, juros, indust)
fit006 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 3)

df <- ts.intersect(commodity, inflacao, indust)
fit007 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 3)

df <- ts.intersect(ibovespa, juros, indust)
fit008 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 3)

df <- ts.intersect(ibovespa, inflacao, indust)
fit009 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 3)

df <- ts.intersect(juros, inflacao, indust)
fit010 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 3)

df <- ts.intersect(commodity, ibovespa, juros, indust)
fit011 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 4)

df <- ts.intersect(commodity, juros, inflacao, indust)
fit012 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 4)

df <- ts.intersect(commodity, ibovespa, inflacao, indust)
fit013 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 4)

df <- ts.intersect(ibovespa, juros, inflacao, indust)
fit014 <- crossValid(df, MaxDefasagens = 80, maxp = 15, posNoDf = 4)


#Dai só juntar tudo numa lista e pedir o minimo
lista <- cbind(fit000,fit001,fit002,fit003,fit004,fit005,fit006,fit007,fit008,fit009,fit010,fit011,fit012,fit013,fit014)
which(lista == min(lista), arr.ind = T)


#agora comparando com o arima que a gente fez la em cima
seriePreditaARIMA <- gerarSerieARIMA(indust, MaxDefasagens = 80, ordem = c(2,0,2))
erroQuadradoARIMA <- calcResiduosARIMA(indust, seriePreditaARIMA, defasagens = 80)


#fazendo uns plot bunitu
df <- ts.intersect(ibovespa, juros, inflacao, indust)
seriePreditaVAR <- gerarSerie(df, MaxDefasagens = 80, posNoDf = 4)
windows()
plot(indust[87:187], type = "l")
lines(seriePreditaARIMA[87:187], col ="purple")
lines(seriePreditaVAR[87:187], col = "red")


###############################################################
###############################################################
###############################################################


#pra fazer previsão os erros precisam ser normais pra montar o IC
#normality.test realiza testes jarque-bera multivariados nos residuos de um objeto do tipo VAR
xxx <- VAR(df, p = 2)
jarque.bera.test(xxx$varresult$indust$residuals)
plot(xxx$varresult$indust$residuals, type="l")
normality.test(xxx)
#ou seja precisariamos calcular a distribuição empirica com boostrap ou sei la, mas foje do escopo


windows()
plot(predict(VAR(df, p=5), n.ahead = 5))






















# produção industrial: bcb 21859
# IPCA: bcb 433
# INPC: bcb 188
# selic Mensal: bcb 4390
# ibovespa: bcb 7832
# Índice de Commodities: bcb 20048



#BONUS TIME
#exemplos e package

VARselect(df, lag.max = 20)

predict(VAR(df, p = 1))