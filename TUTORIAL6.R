pibus <- read.csv("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\A191RL1Q225SBEA.csv")
colnames(pibus)[2] <- "varPib"
pibus <- ts(pibus$varPib, start = c(1947, 2), frequency = 4)

plot(pibus)

# quebra em 1984-3

subSerie1 <- window(pibus, end = c(1984, 2))
subSerie2 <- window(pibus, start = c(1984,3))

achadorDeArma <- function(serie){
  maic<-matrix(0,6,6)
  for(j in 0:5) {
    for (i in 0:5){
      maic[j+1,i+1]<-AIC(arima(serie,c(j,0,i)))
    }
  }
  which(maic == min(maic), arr.ind = TRUE)-c(1,1)
}

achadorDeArma(pibus)
achadorDeArma(subSerie1)
achadorDeArma(subSerie2)


fit1 <- arima(subSerie1, c(3,0,2))
fit2 <- arima(subSerie2, c(2,0,0))

Dummy1 <- vector(mode = "integer", length = length(pibus))
Dummy1[1:length(subSerie1)] <- 1

Dummy2 <- vector(mode = "integer", length = length(pibus))
Dummy2[(length(subSerie1)+1):length(Dummy2)] <- 1

library(dyn)
fit3 <- dyn$lm(pibus ~ (lag(pibus) + lag(pibus, 2) + lag(pibus, 3))*Dummy1 + 
                 (lag(pibus) + lag(pibus, 2))*Dummy2)

fit3 <- lm(pibus[4:length(pibus)] ~ (pibus*Dummy1)[3:(length(pibus)-1)] + (pibus*Dummy1)[2:(length(pibus)-2)] + (pibus*Dummy1)[1:(length(pibus)-3)] + 
             (pibus*Dummy2)[3:(length(pibus)-1)] + (pibus*Dummy2)[2:(length(pibus)-2)])





