predizerARIMA <- function(df, ordem = c(2,0,2), h1 = 1){

  fit <- arima(df, ordem)
  a <- predict(fit, n.ahead = h1)
  y_predito <- predict(arima(df, c(2,0,2)))$pred
  return(y_predito)
}


gerarSerieARIMA <- function(df, MaxDefasagens=20, ordem=c(2,0,2), h1 = 1){
  
  resultSet <- df
  resultSet[(length(df)-MaxDefasagens+1):length(df)] <- 0
  for(i in 1:MaxDefasagens){
    serieX <- head(df, (length(df)-MaxDefasagens+i))
    predicX <- predizerARIMA(serieX, ordem=ordem, h1=h1)
    resultSet[length(resultSet)-MaxDefasagens+i] <- predicX
  }
  return(resultSet)
}


calcResiduosARIMA <- function(serieOri, serieCortada, defasagens){

  errosTot <- vector()
  for(i in 1:defasagens){
    erroX <- serieOri[length(serieOri)-defasagens+i] - serieCortada[length(serieOri)-defasagens+i]
    erroXX <- erroX^2
    errosTot <- append(errosTot, erroXX)
  }
  sumErros <- mean(errosTot)
  return(sumErros)
}