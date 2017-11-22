pib <- read.csv("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\PIB.csv", sep = ";")
pib <- as.numeric(gsub(",", ".", pib$PIB))
pib <- ts(pib,start = c(1996,1), frequency = 4)


logpib <- log(lag(pib)) - log(pib)
treino <- window(pib, end = c(2008,4))
teste <- window(pib, start=2009)


plot(pib)
plot(treino)
plot(teste)
plot(logpib)


calcArima <- function(serie, ordem, horizonte = 1){
  arimaObj <- arima(serie, ordem)
  ytplus1 <- predict(arimaObj, horizonte)
  return(ytplus1)
}


calcSerieArima <- function(serieOriginal, MaxDefasagens, ordem, horizonte = 1){
  resultSet <- serieOriginal[1:(length(serieOriginal)-MaxDefasagens)]
  for(i in 0:MaxDefasagens){
    serieX <- serieOriginal[1:(length(serieOriginal)-MaxDefasagens+i)]
    predicX <- as.numeric(calcArima(serieX, ordem, horizonte)$pred)
    resultSet <- append(resultSet, predicX)
    print(predicX)
  }
  return(ts(resultSet,start = start(serieOriginal), end = end(serieOriginal), frequency = 4))
}


calcResArima <- function(serieOri, serieCortada, defasagens){
  errosTot <- vector()
  for(i in 1:defasagens){
    erroX <- serieOri[length(serieOri)-defasagens+i] - serieCortada[length(serieOri)-defasagens+i]
    erroXX <- erroX^2
    errosTot <- append(errosTot, erroXX)
  }
  sumErros <- sum(errosTot)
  return(sumErros)
}



xxx <- calcSerieArima(logpib, 10, c(2,0,0))

arima101 <- arima(treino, c(1,0,1))
predict(arima101, 2)
forecast(arima101, 2)
