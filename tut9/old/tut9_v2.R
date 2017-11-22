#resolvendo o problema que o dick teve no ultimo tutorial
#objetos do tipo fator são um desgraça
#além disso o ultimo elemento era "ibge", dai quando converteu ele virou um NA
indust <- read.csv2("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut9\\ProdIndustrial.csv")
indust <- as.numeric(gsub(",", ".", as.character(indust[,2])))
indust <- indust[!is.na(indust)]

#baixar duas series faz elas serem separadas por TAB, que o R lê como \t
#mesmos problemas que a serie anterior exceto que desse ves os NAs são por causa da diferença do IPCA e do INPC
inflacao <- read.csv("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut9\\inflacao.csv", sep = "\t")
inflacao <- as.numeric(gsub(",", ".", as.character(inflacao[,3])))
inflacao <- inflacao[!is.na(inflacao)]

#mesma coisa pra selic mensal
juros <- read.csv2("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut9\\selic.csv")
juros <- as.numeric(gsub(",", ".", as.character(juros[,2])))
juros <- juros[!is.na(juros)]

#mesma coisa pra ibovespa
ibovespa <- read.csv2("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut9\\ibovespa.csv")
ibovespa <- as.numeric(gsub(",", ".", as.character(ibovespa[,2])))
ibovespa <- ibovespa[!is.na(ibovespa)]

#mesma coisa pra commodity
commodity <- read.csv2("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut9\\commodity.csv")
commodity <- as.numeric(gsub(",", ".", as.character(commodity[,2])))
commodity <- commodity[!is.na(commodity)]

#trabalhar com objetos TS é mais facil então...
commodity <- ts(commodity, start = c(1998, 1), frequency = 12)
ibovespa <- ts(ibovespa, start = c(1987, 1), frequency = 12)
juros <- ts(juros, start = c(1986, 7), frequency = 12)
inflacao <- ts(inflacao, start = c(1980, 1), frequency = 12)
indust <- ts(indust, start = c(2002, 1), frequency = 12)

#sempre bom ver os dados
plot(commodity)
plot(ibovespa)
plot(indust)
plot(inflacao)
plot(juros)

#vamo ver se esses trem são estacionario
Box.test(commodity)
Box.test(ibovespa)
Box.test(indust)
Box.test(inflacao)
Box.test(juros)

#sepa que assim é melhor
indust <- diff(indust)
commodity <- diff(commodity)
juros <- diff(juros)
#ou assim?
library(mFilter)
plot(hpfilter(commodity))

#contruindo a matriz pra usar no modelo VAR e conrtando as variaveis pra ficarem do mesmo length
df <- ts.intersect(commodity, ibovespa, juros, inflacao, indust)
commodity <- df[,1]
ibovespa <- df[,2]
juros <- df[,3]
inflacao <- df[,4]


#### parte 1 ####


#### parte 2 ####
#estimando um modelo VAR
#A função VAR faz a mesma coisa que estimar um bom e velho LM só que pra cada variavel, exemplo:
library(vars)

fit11 <- VAR(df, p = 1)
fit12 <- lm(indust[2:188] ~ indust[1:187] + commodity[1:187] + ibovespa[1:187] + inflacao[1:187] + juros[1:187])

fit21 <- VAR(df, p = 2)
fit22 <- lm(indust[3:188] ~ indust[2:187] + commodity[2:187] + ibovespa[2:187] + inflacao[2:187] + juros[2:187] +
     indust[1:186] + commodity[1:186] + ibovespa[1:186] + inflacao[1:186] + juros[1:186])

#deixando mais bonito pra comparar
library(stargazer)

names(fit12$coefficients) <- c("const", "indust.l1", "commodity.l1", "ibovespa.l1", "inflacao.l1",  "juros.l1")
stargazer(fit11$varresult$indust ,fit12 ,type = "text")


names(fit22$coefficients) <- c("const", "indust.l1", "commodity.l1", "ibovespa.l1", "inflacao.l1",  "juros.l1",
                               "indust.l2", "commodity.l2", "ibovespa.l2", "inflacao.l2",  "juros.l2")
stargazer(fit21$varresult$indust, fit22, type = "text")


#tb tem esse package que calcula pela decomposição QR:
library(dynlm)
dynlm(indust ~ lag(indust) + lag(commodity) + lag(ibovespa) + lag(inflacao) + lag(juros))


#Cross validando


























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