#### parte 1 ####

pibus <- read.csv("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\A191RL1Q225SBEA.csv")
colnames(pibus)[2] <- "varPib"
pibus <- ts(pibus$varPib, start = c(1947, 2), frequency = 4)

#subSerie1 <- window(pibus, end = c(1984, 2))
#subSerie2 <- window(pibus, start = c(1984,3))
subSerie1 <- window(pibus, end = c(1984, 3))
subSerie2 <- window(pibus, start = c(1984,3))

#assumindo AR1 para ambos os modelos
# k = r + 2q
# r = 0
# q = 2
k= 4

yt_1.1 <- pibus[1:(length(pibus)-1)]
yt_1.1[(length(subSerie1)+1):length(yt_1.1)] <- 0

yt_1.2 <- pibus[1:(length(pibus)-1)]
yt_1.2[1:length(subSerie1)] <- 0

Dummy1 <- yt_1.1
Dummy1[yt_1.1 != 0] <- 1

Dummy2 <- yt_1.2
Dummy2[yt_1.2 != 0] <- 1

pibusCut <- pibus[2:length(pibus)]
fit1 <- lm(pibusCut ~ 0 + Dummy1 + yt_1.1 + Dummy2 + yt_1.2)
summary(fit1)

v <- fit1$residuals
v1 <- fit1$residuals[1:length(subSerie1)]
v2 <- fit1$residuals[length(subSerie1):length(fit1$residuals)]

b_hat1 <- fit1$coefficients[1:(length(fit1$coefficients)/2)]
b_hat2 <- fit1$coefficients[(length(fit1$coefficients)/2+1):length(fit1$coefficients)]

# m = ordem maxima do lag T^(1/4)
m = round((length(pibus)-1)^(1/4))

X <- cbind(Dummy1, yt_1.1, Dummy2, yt_1.2)


omega <- matrix(0,k,k)

for(j in 0:m){
  gama <- matrix(0,k,k)
  vprime <- as.numeric(t(v[(j+1):length(v)]) %*% v[(1):(length(v)-j)])
  gama <- (t(X[(j+1):(length(v)),]) %*% X[(j+1):(length(v)),]) * vprime
  omega <- omega + ( (m+1-j) / (m+1) ) * gama*(1+1*(j>0))
}


v_hat <- solve(t(X) %*% X) %*% omega %*% solve(t(X) %*% X)
v_hat1 <- v_hat[1:(length(v_hat[1,])/2),1:(length(v_hat[,2])/2)]
v_hat2 <- v_hat[((length(v_hat[1,])/2)+1):length(v_hat[1,]),((length(v_hat[,1])/2)+1):length(v_hat[,1])]

wald <- t(b_hat1 - b_hat2) %*% solve(v_hat1 + v_hat2) %*% (b_hat1 - b_hat2)

#### parte 2 - variancia ################################################

pibusCut <- pibus[2:length(pibus)]
pibuscutLAG <- pibus[1:(length(pibus)-1)]

fit2 <- lm(pibusCut ~ pibuscutLAG)
fit2 <- arima(pibus, c(1,0,0))

t1 <- as.numeric(length(subSerie1))
s1 <- 1/(t1-2)*(t(fit2$residuals[1:t1]) %*% fit2$residuals[1:t1])
s2 <- 1/(length(pibusCut)-t1-2)*(t(fit2$residuals[(t1+1):length(pibusCut)]) %*% fit2$residuals[(t1+1):length(pibusCut)])

#sob normalidade
wald2 <- s1/s2

#sem normalidade </3

aj <- function(j, fit, s, t1){
  total <- 0
  for(i in (j+1):t1){
    total <- total + (1/t1) * ((fit$residuals[i]^2-s^2)*(fit$residuals[i-j]^2-s^2))
  }
  return(total)
}

kappaSqr1 <- function(m, fit, s, t1){
  Kapper <- 0
  for(n in 0:m){
    Kapper <- Kapper <- Kapper + ( (m+1-j) / (m+1) ) * aj(as.numeric(n), fit, s, t1) * (1+1*(n>0))
  }
  Kapper <- Kapper/t1
  return(Kapper)
}

bj <- function(j, fit, s, t1, t){
  total <- 0
  for(i in (j+t1+1):t){
    total <- total + (1/(t-t1)) * ((fit$residuals[i]^2-s^2)*(fit$residuals[i-j]^2-s^2))
  }
  return(total)
}

kappaSqr2 <- function(m, fit, s, t1, t){
  Kapper <- 0
  for(n in 0:m){
    Kapper <- Kapper <- Kapper + ( (m+1-j) / (m+1) ) * bj(as.numeric(n), fit, s, t1, t) * (1+1*(n>0))
  }
  Kapper <- Kapper/(t-t1)
  return(Kapper)
}

kappa1 <- kappaSqr1(m, fit2, s1,t1)
kappa2 <- kappaSqr2(m, fit2, s2,t1, t = length(pibusCut))

wald2 <- (s1-s2)/(kappa1+kappa2)^(1/2)


#### parte 3 - momento da quebra 10conhecido ####



