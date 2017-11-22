library(zoo)
library(fpp)
library(xts)

df <- read.csv2("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\PIB.csv")

PIB <- ts(df$PIB,start = c(1996,1), frequency = 4)


filtroHP <- function(y, lambda){
  id <- diag(length(y))
  d <- diff(id, d=2)
  tendhp <- as.ts(solve(id + lambda * crossprod(d), y))
}

PIB1 <- filtroHP(PIB, lambda = 160)
PIB1 <- ts(PIB1, start = c(1996,1), frequency = 4)
plot(PIB1)

PIB2 <- filtroHP(PIB, lambda = 10)
PIB2 <- ts(PIB2, start = c(1996,1), frequency = 4)
plot(PIB2)

plot(PIB1)
lines(PIB)
lines(PIB2)

PIBma <- rollmean(PIB, 4, na.pad = T)

PIBjanela <- window(PIB, end = 2014)
fit <- auto.arima(PIBjanela)

summary(fit)

res <- ts(fit$residuals, start = start(PIBjanela), frequency = frequency(PIBjanela))
res_sq <- ts(fit$residuals^2, start = start(PIBjanela), frequency = frequency(PIBjanela))

plot(res_sq)

acf(res)

pacf(res)

Box.test(res, type = "Ljung")

pacf(res_sq)

hist(res)

jarque.bera.test(res)

checkresiduals(fit)

#------------------------------------------------------------------------------------------------


set.seed(23082017)

n = 12*10

modelo <- list(ar = c(0.5), ma=c(-0.3, 0,7))

c=5

mu <- c/(1-sum(modelo$ar))

y = mu + arima.sim(modelo, n, sd = 0.25)
y = ts(y, start = c(2000,1), frequency = 12)

plot(y)

treino <- window(y, end = c(2008,12))
teste <- window(y, start=2009)

fc <- naive(treino, h=12)

plot(fc)
lines(teste)


hf = 3
k = 2
C <- array(dim=c(k+1, k+1, hf))
for(i in 0:k){
  for(j in 0:k){
    fit <- function(x,h){forecast(arima(x,order = c(i,0,j)),h=h)}
    
    for(h in 1:hf){
      out <- tsCV(treino,forecastfunction = fit, h=h)
      outsq <- (out)^2
      meansqmean <- mean(outsq, na.rm = T)
      C[i+1, j+1, h] <-meansqmean
    }
  }
}