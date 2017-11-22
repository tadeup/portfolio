library(urca)
library(vars)
library(tsDyn)

setwd("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut12")

consumo <- read.csv("consumo-ipea.csv", stringsAsFactors = F)
names(consumo) <- c("data", "consumo")
consumo <- ts(consumo$consumo, start = c(1991, 1), frequency = 4)

pib <- read.csv2("pib.csv", stringsAsFactors = F)
pib <- pib[-length(pib[,1]),]
pib[,2] <- gsub("\\.", "", pib[,2])
pib[,2] <- as.numeric(gsub(",", ".", pib[,2]))
pib <- ts(pib[,2], start = c(1990, 1), frequency = 12)
pib_trim <- aggregate(pib, nfrequency = 4)

############################################

moeda <- read.csv2("papel moeda.csv", stringsAsFactors = F)
moeda <- ts(as.numeric(gsub(",", "", moeda[,2][-454])), start = c(1980, 1), frequency = 12)

juros <- read.csv2("selic.csv", stringsAsFactors = F)
juros <- ts(as.numeric(gsub(",", "\\.", juros[,2][-377])), start = c(1986, 7), frequency = 12)

ipca <- read.csv("ipca nominal.csv", sep = "\t")
ipca <- ts(unlist(ipca), start = c(1979, 12), frequency = 12)

#############################################

preco_usa <- read.csv2("preco_usa.csv", stringsAsFactors = F)
preco_usa <- ts(as.numeric(preco_usa[,2][-440]), start = c(1980, 1), frequency = 12)

cambio <- read.csv2("cambio.csv", stringsAsFactors = F)
cambio <- ts(as.numeric(cambio[,2][-779]), start = c(1953, 1), frequency = 12)

############################################

df1 <- ts.intersect(consumo, pib_trim)
df2 <- ts.intersect(moeda, pib, ipca, juros)
df2 <- window(df2, start = c(1998, 1))
df3 <- ts.intersect(cambio, ipca, preco_usa)
df3 <- window(df3, start = c(1998, 1))

VARselect(df1) #deu 5
summary(ca.jo(df1, K = 5))
matplot(df1)
vec2var(ca.jo(df1))

VARselect(df2) #deu 4
summary(ca.jo(log(df2), K = 4))
matplot(log(df2))
vec2var(ca.jo(df2))

VARselect(df3) #deu 2
summary(ca.jo(df3, K = 2))
matplot(df3)
vec2var(ca.jo(df3))

###########################################

VECM(df1, lag = 2)









# séries
#3695 - Taxa de câmbio
#Consumo final das famílias - referência 200 - IPEA
#ipca - https://sidra.ibge.gov.br/tabela/1737
#4390 - Taxa de juros - BCB
#1825 - Papel moeda em poder do público - BCB
#4380 - PIB mensal - BCB
#3794 - Consumer price index - BCB




