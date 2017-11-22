desemprego <- read.csv("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut8\\desemprego_ipea_rmsp.csv")
#desemprego <- ts(desemprego[,2], start = c(1984, 12), frequency = 12)
desemprego <- desemprego[,2][-1]
desemprego <- ts(desemprego[seq(1, length(desemprego), 3)], start = c(1985, 1), frequency = 4)
desemprego <- window(desemprego, start = c(2002, 1))

IPCA <- read.csv("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut8\\IPCA_MENSAL.csv")
#IPCA <- ts(IPCA[,1], start = c(1980, 1), frequency = 12)
IPCA <- ts(IPCA[,1][seq(1, length(IPCA[,1]), 3)], start = c(1980, 1), frequency = 4)
IPCA <- window(IPCA, start = c(2002, 1))

expInflacao <- read.csv2("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut8\\expectativa de inflacao.csv")
expInflacao <- ts(expInflacao[,2][-length(expInflacao[,2])], start = c(2002, 1), frequency = 4)

##====================================================#
library(sandwich)
library(dynlm)
Box.test(desemprego, type = "Ljung-Box")
Box.test(expInflacao, type = "Ljung-Box")
Box.test(IPCA, type = "Ljung-Box")


#modelo 1
fit <- lm(IPCA ~ desemprego)


#modelo 2
y <- as.numeric(IPCA[2:length(IPCA)])
k <- rep(1, length(y))
x1 <- as.numeric(desemprego[2:length(desemprego)])
x2 <- as.numeric(lag(IPCA)[-length(IPCA)])
x <- cbind(k, x1)
x <- cbind(x, x2)
beta <- solve(t(x)%*%x) %*% (t(x)%*%y)

fit <- lm(IPCA[2:63]~desemprego[2:63]+IPCA[-63])
Box.test(fit$residuals, type = "Ljung-Box", lag = 10)
summary(fit)
#nw <- NeweyWest(fit, sandwich = F)
#v <- solve(t(x)%*%x) %*% nw %*% solve(t(x)%*%x)
#se <- sqrt(diag(v))
nw <- NeweyWest(fit)
se <- sqrt(diag(nw))

fit <- dynlm(IPCA ~ desemprego + lag(IPCA))
Box.test(fit$residuals, type = "Ljung-Box")
fit <- dynlm(IPCA ~ desemprego + lag(IPCA)+ lag(IPCA, 2))
Box.test(fit$residuals, type = "Ljung-Box", lag = 5)
fit <- dynlm(IPCA ~ desemprego + lag(IPCA)+ lag(IPCA, 2)+ lag(IPCA, 3))
Box.test(fit$residuals, type = "Ljung-Box")
fit <- dynlm(IPCA ~ desemprego + lag(IPCA)+ lag(IPCA, 2))
Box.test(fit$residuals, type = "Ljung-Box")
fit <- dynlm(IPCA ~ desemprego + lag(IPCA)+ lag(IPCA, 2))
Box.test(fit$residuals, type = "Ljung-Box")


#modelo 3
fit <- dynlm(IPCA ~ desemprego + expInflacao)
summary(fit)
nw <- NeweyWest(fit)
se <- sqrt(diag(nw))


AIC(fit)


#fazendo previsões
#fazer um cross validation e tals, um loop que calcula o coeficiente de toddas as regs e soma os erros ao quadrado e ve qual minimiza
calcARDL <- function(ModFitado, horizonte = 1){
  
  ytplus1 <- predict(ModFitado, horizonte)
  return(ytplus1)
  
}


calcSerie <- function(serieOriginal, ModFitado, MaxDefasagens, horizonte = 1){
  
  resultSet <- serieOriginal[1:(length(serieOriginal)-MaxDefasagens)]
  
  for(i in 0:MaxDefasagens){
    serieX <- serieOriginal[1:(length(serieOriginal)-MaxDefasagens+i)]
    predicX <- as.numeric(calcARDL(ModFitado, horizonte)$pred)
    resultSet <- append(resultSet, predicX)
  }
  
  return(resultSet)
  
}


calcRes <- function(serieOri, serieCortada, defasagens = 10){
  errosTot <- vector()
  for(i in 1:defasagens){
    erroX <- serieOri[length(serieOri)-defasagens+i] - serieCortada[length(serieOri)-defasagens+i]
    erroXX <- erroX^2
    errosTot <- append(errosTot, erroXX)
  }
  sumErros <- sum(errosTot)
  return(sumErros)
}





# fontes
# http://www.ipeadata.gov.br/Default.aspx
# 
