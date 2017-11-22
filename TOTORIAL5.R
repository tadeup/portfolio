set.seed(666)
#### PARTE 1####
#### modelo 1 ####
# padrão com t variando

t <- 1200
r <- 10000
q <- matrix(NA, r, 2)
drift <- 0

for(i in 1:r){
  e <- rnorm(t, sd = 2)
  y <- matrix(NA, t, 1)
  y[1] <- k
  for(j in 2:t){y[j] <- y[j-1] + e[j]}
  y <- ts(y)
  
  y_1 <- as.ts(y[1:(t-1)])
  y <- y[2:t]
  
  out <- lm(y ~ 0 + y_1)
  out_summary <- summary.lm(out)
  
  beta <- out_summary$coefficients[1,1]
  variancia <- out_summary$coefficients[1,2]
  t_stat <- (beta-1)/(variancia)
  
  q[i,1] <- beta
  q[i,2] <- t_stat
}

hist(q[,1])
hist(q[,2])

quantile(q[,2], probs = c(0.05, 0.95))

#### modelo 2 ####
# com constante e tendencia linear

t <- 120
r <- 10000
k <- 5
q <- matrix(NA, r, 2)
drift <- 0.5

for(i in 1:r){
  e <- rnorm(t, sd = 2)
  y <- matrix(NA, t, 1)
  y[1] <- k
  for(j in 2:t){y[j] <- drift + y[j-1] + e[j]}
  y <- ts(y)
  
  y_1 <- as.ts(y[1:(t-1)])
  y <- y[2:t]
  
  out <- lm(y ~ y_1)
  out_summary <- summary.lm(out)
  
  beta <- out_summary$coefficients[2,1]
  variancia <- out_summary$coefficients[2,2]
  t_stat <- (beta-1)/(variancia)
  
  q[i,1] <- beta
  q[i,2] <- t_stat
}

hist(q[,1])
hist(q[,2])

quantile(q[,2], probs = c(0.05, 0.95))

#### modelo 3 ####
# autocorr do erro

t <- 120
r <- 10000
k <- 0
q <- matrix(NA, r, 2)
drift <- 0
rho <- 0.5

for(i in 1:r){
  e <- rnorm(t, sd = 2)
  y <- matrix(NA, t, 1)
  y[1] <- k
  for(j in 2:t){y[j] <- drift + y[j-1] + (rho*e[j-1] + e[j])}
  y <- ts(y)
  
  y_1 <- as.ts(y[1:(t-1)])
  y <- y[2:t]
  
  out <- lm(y ~ y_1)
  out_summary <- summary.lm(out)
  
  beta <- out_summary$coefficients[2,1]
  variancia <- out_summary$coefficients[2,2]
  t_stat <- (beta-1)/(variancia)
  
  q[i,1] <- beta
  q[i,2] <- t_stat
}

hist(q[,1])
hist(q[,2])

quantile(q[,2], probs = c(0.05, 0.95))

#### modelo 4 ####
# distribuição do erro

t <- 120
r <- 10000
k <- 0
q <- matrix(NA, r, 2)
drift <- 0

for(i in 1:r){
  e <- rgamma(t, 3)
  y <- matrix(NA, t, 1)
  y[1] <- k
  for(j in 2:t){y[j] <- drift + y[j-1] + e[j]}
  y <- ts(y)
  
  y_1 <- as.ts(y[1:(t-1)])
  y <- y[2:t]
  
  out <- lm(y ~ y_1)
  out_summary <- summary.lm(out)
  
  beta <- out_summary$coefficients[2,1]
  variancia <- out_summary$coefficients[2,2]
  t_stat <- (beta-1)/(variancia)
  
  q[i,1] <- beta
  q[i,2] <- t_stat
}

hist(q[,1])
hist(q[,2])

quantile(q[,2], probs = c(0.05, 0.95))

#pode ser util:
#plot(y)
#abline(out)

#### PARTE 2 ####
library(dyn)
pib <- read.csv("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\PIB.csv", sep = ";")
pib <- as.numeric(gsub(",", ".", pib$PIB))
pib <- ts(pib,start = c(1996,1), frequency = 4)

#### dickey fuller simples ####
# jeito1 #
deltaY <- pib-lag(pib)
deltaY <- diff(pib)
alpha <- (sum(lag(pib)*pib))/(sum(lag(pib)^2))-1
alpha <- dyn$lm(pib ~ 0 + lag(pib))$coefficients-1
s2 <- (1/(length(pib)-1))*sum((deltaY-alpha*lag(pib))^2)
dpalpha <- sqrt(s2)/sqrt(sum(lag(pib)^2))
t_stat <- alpha/dpalpha

# jeito 2 #
lags=0
z <- diff(pib)
z <- pib-lag(pib)
n <- length(z)
z.diff <- embed(z, lags+1)[,1]
z.lag.1 <- pib[(lags+1):n]
summary(lm(z.diff~0+z.lag.1 ))
summary(lm(z.diff~0+z.lag.1 ))$coefficients[1]/summary(lm(z.diff~0+z.lag.1 ))$coefficients[2]


#### ADF ####
# calculando os coeficientes #
# metodo 1 #
library(tseries)
adf.test(pib, k = 1)

metodo1 <- function(serie, pvalor = 0.05, maxTentativas = 10){
  convergiu <- FALSE
  contador <- 0
  while(convergiu==F){
    residuos <- arima(serie, c(contador, 0, 0))$residuals
    p <- Box.test(residuos, type = "Ljung")$p.value
    if(p >= pvalor){convergiu<-T}
    contador <- contador+1
    if(contador == maxTentativas){break()}
  }
  if(convergiu==T){
    return(contador)
  } else {print("não convergiu")}
}
metodo1(pib)

# metodo 2 #
metodo2 <- function(serie, pmaximo = 10){
  vetorzinho <- vector()
  for(i in 0:pmaximo){
    vetorzinho <- append(vetorzinho, AIC(arima(serie, c(i,0,0))))
  }
  return(which.min(vetorzinho))
}

metodo2(pib)

# fazendo a estatistica de teste t #
# metodo 1 #

deltaY <- pib-lag(pib)
lagdiff1 <- (lag(pib)-lag(pib, 2))
lagdiff2 <- (lag(pib, 2) - lag(pib,3))
alpha <- dyn$lm(deltaY ~ 0 + lag(pib) + lagdiff1 + lagdiff2)$coefficients-1
alpha <- dyn$lm(deltaY ~ lag(pib) + lagdiff1 + lagdiff2)$coefficients-1
erro <- dyn$lm(deltaY ~ lag(pib) + lagdiff1 + lagdiff2)$residuals
s2 <- (1/(length(pib)-1))*sum(erro^2)
dpalpha <- sqrt(s2)/sqrt(sum(lag(pib)^2))
t_stat <- alpha/dpalpha

# metodo 2 #

deltaY <- pib-lag(pib)
lagdiff1 <- (lag(pib)-lag(pib, 2))
lagdiff2 <- (lag(pib, 2) - lag(pib,3))
lagpib <- lag(pib)
alpha <- dyn$lm(deltaY ~ 0 + lag(pib) + lagdiff1 + lagdiff2)$coefficients-1
alpha <- dyn$lm(deltaY ~ lag(pib))$coefficients-1
erro <- dyn$lm(deltaY ~ lag(pib))$residuals
s2 <- (1/(length(pib)-1))*sum(erro^2)
dpalpha <- sqrt(s2)/sqrt(sum(lag(pib)^2))
t_stat <- alpha/dpalpha

