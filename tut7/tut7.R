install.packages("fGarch")
install.packages("tsDyn")
library(tseries)

serie <- read.csv("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut7\\PETR4.SA_2.csv")
precos <- ts(serie$Open)
precos <- ts(precos[precos!=527])
plot(precos)

logretorno <- diff(log(precos))
plot(logretorno)
retorno <- diff(precos)/precos[-length(precos)]
plot(retorno)

#### fato 1 ####


#### fato 2 ####
t.test(logretorno, mu = 0)

n <- length(logretorno)
std_error <- sd(logretorno) / sqrt(n)
t <- (mean(logretorno) - 0) / std_error
p_value <- 2 * pt(t, df = (n-1))


#### fato 3 ####
k = mean((logretorno-mean(logretorno))^4) / mean((logretorno-mean(logretorno))^2)^2
std_error <- sqrt(24/n)
teste <- k / std_error
pnorm(teste, sd = std_error)


#### fato 4 ####
# segundo as notas de aula series financeiras de log preço seguem um arima(0,1,0)
# logo a primeira diferença sera um arma(0,0)
# Yt = c + Et
# Et = sigmat * ut
# sendo: sigmat^2 = w + alpha * Et-1^2 + beta * sigmat-1^2
# defina Vt = Et^2 - sigmat^2
# Vt = (ut - 1) * sigmat^2
# substituindo: Et^2 = w + alpha * Et-1^2 + beta * (Et-i^2 - Vt-1) + Vt
# reescrevendo: Et^2 = w + (alpha * beta) * Et-1^2 - beta * Vt-1 + Vt
# logo Vt = Et^2 - w - (alpha * beta) * Et-1^2 + beta * Vt-1

residuos <-  logretorno - mean(logretorno)
residuos2 <- residuos^2

garchFind <- function(Et2){
  l = length(Et2)
  param <- c(w = 0, alpha = 0, beta = 0)
  SSE <- function(param){
    w <- param[1]
    alpha <- param[2]
    beta <- param[3]
    
    Vt <- vector()
    Vt[1] <- 0
    for(i in (2:l)){
      Vt[i] <- Et2[i] - w - (alpha*beta) * Et2[i-1] + beta*Vt[i-1]
    }
    return(sum(Vt*Vt))
  }
  
  foo <- nlminb(objective = SSE, start = param)
  return(foo)
}
garchFind(residuos2)


# bonus #
achadorDeGARCH <- function(serie){
  maic<-matrix(0,12,12)
  for(j in 0:11) {
    for (i in 0:11){
      maic[j+1,i+1]<-AIC(arima(serie,c(j,0,i)))
    }
  }
  which(maic == min(maic), arr.ind = TRUE)-c(1,1)
}
achadorDeGARCH(residuos2)

fit001 <- arima(residuos2, c(9, 0, 8))

garch(logretorno, c(9,8))
garch(logretorno, c(1,1))
# fim do bonus #

#### fato 5 ####
# Yt = c + Et
# Et = sigmat * ut
# sendo: sigmat^2 = w + delta * dt + alpha * Et-1^2 + beta * sigmat-1^2
# defina Vt = Et^2 - sigmat^2
# Vt = (ut - 1) * sigmat^2
# substituindo: Et^2 = w + delta * dt + alpha * Et-1^2 + beta * (Et-i^2 - Vt-1) + Vt
# reescrevendo: Et^2 = w + delta * dt + (alpha * beta) * Et-1^2 - beta * Vt-1 + Vt
# logo Vt = Et^2 - w - delta * dt - (alpha * beta) * Et-1^2 + beta * Vt-1

segundas <- as.numeric(weekdays(as.Date(serie$Date)) == "Monday")

garchFind5 <- function(Et2){
  dt <- segundas
  
  l = length(Et2)
  param <- c(w = 0, alpha = 0, beta = 0, delta = 0)
  SSE <- function(param){
    w <- param[1]
    alpha <- param[2]
    beta <- param[3]
    delta <- param[4]
    
    Vt <- vector()
    Vt[1] <- 0
    for(i in (2:l)){
      Vt[i] <- Et2[i] - w - delta * dt - (alpha*beta) * Et2[i-1] + beta*Vt[i-1]
    }
    return(sum(Vt*Vt))
  }
  
  foo <- nlminb(objective = SSE, start = param)
  return(foo)
}
garchFind5(residuos2)


#### fato 6 ####
# analogo ao item 5

#### fato 7 ####

erroPositivo <- as.numeric(logretorno > 0)

garchFind7 <- function(Et2){
  dt <- erroPositivo
  
  l = length(Et2)
  param <- c(w = 0, alpha = 0, beta = 0, delta = 0)
  SSE <- function(param){
    w <- param[1]
    alpha <- param[2]
    beta <- param[3]
    delta <- param[4]
    
    Vt <- vector()
    Vt[1] <- 0
    for(i in (2:l)){
      Vt[i] <- Et2[i] - w - delta * dt - (alpha*beta) * Et2[i-1] + beta*Vt[i-1]
    }
    return(sum(Vt*Vt))
  }
  
  foo <- nlminb(objective = SSE, start = param)
  return(foo)
}
garchFind7(residuos2)

#### fontes ####
# https://br.financas.yahoo.com/quote/PETR4.SA/history?period1=1262311200&period2=1357005600&interval=1d&filter=history&frequency=1d
# https://www.researchgate.net/post/What_is_the_acceptable_range_of_skewness_and_kurtosis_for_normal_distribution_of_data
# http://www.lithoguru.com/scientist/statistics/Lecture14.pdf
# https://www.r-bloggers.com/garch-estimation-using-maximum-likelihood/


#### BONUS TIME ####
#### fato 4.2 ####
#reescrevendo: Et^2 = w + delta * dt + (alpha * beta) * Et-1^2 - beta * Vt-1 + Vt
residuos <-  logretorno - mean(logretorno)

garchFind <- function(Et){
  l = length(Et)
  param <- c(w = 0, alpha = 0, beta = 0)
  SSE <- function(param){
    w <- param[1]
    alpha <- param[2]
    beta <- param[3]
    
    Vt <- vector()
    Vt[1] <- 0
    for(i in (2:l)){
      Et[i] <- w + (alpha*beta) * Et[i-1] - beta*Vt[i-1] + Vt[i]
    }
    return(sum(Et*Et))
  }
  
  foo <- nlminb(objective = SSE, start = param)
  return(foo)
}
garchFind(residuos)

#### fato 7.2 ####
egarchFind <- function(Et2, Et){
  
  l = length(Et2)
  
  param <- c(omega = 0, alfa = 0, beta = 0, gama = 0)
  
  SSE <- function(param){
    
    omega <- param[1]
    
    alfa <- param[2]
    
    beta <- param[3]
    
    gama <- param[4]
    
    
    l_sigma_2 <- vector(length = l)
    sigma_2 <- vector(length = l)
    
    sigma_2[1] <- Et2[1]
    
    l_sigma_2[1] <- log(sigma_2[1])
    
    for(i in (2:l)){
      
      l_sigma_2[i] <- omega + l_sigma_2[i-1] + alfa*((abs(Et[i-1]/sqrt(sigma_2[i-1])))- abs(sqrt(2/pi))) + gama*(Et[i-1]/sqrt(sigma_2[i-1]))
      
      sigma_2[i] <- exp(l_sigma_2[i])
      
      
    }
    return(sum(l_sigma_2[i]*l_sigma_2[i]))
    
  }
  
  foo <- nlminb(objective = SSE, start = param)
  
  return(foo)
}


egarchFind(epsilon_2, epsilon)

