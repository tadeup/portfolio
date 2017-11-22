library(tseries)


t = 100

#### criando a série 4 ####
y4 <- numeric(t)
for(i in 2:t){
  y4[i] <- y4[i-1] + rnorm(1)
}


#### criando a série 3 ####
y3 <- numeric(t)
for(i in 2:t){
  y3[i] <- y3[i-1] + rnorm(1)
}


#### criando a série 2 ####
# primeiro vc chuta os gammas (aqui denotado por "g")

g12 <- 0.2
g13 <- 0.3
g14 <- 0.4

g21 <- 0.5
g23 <- 0.6
g24 <- 0.7

# depois vc "pluga" a formula do y1 no y2 de forma que:
# y1 = g12 * y2 + g13 * y3 + g14 * y4 + u1
# y2 = g21 * y1 + g23 * y3 + g24 * y4 + u2

# vira:
# y2 = g21 * (g12 * y2 + g13 * y3 + g14 * y4 + u1) + g23 * y3 + g24 * y4 + u2

# que se organizado vira:
# (1 - g21*g12)y2 = (g23+g21*g13)*y3 + (g24+g21*g14)*y4 + (g21*u1+u2)

# e passando o termo que multiplica y2 pro outro lado:
# y2 = ((g23+g21*g13)*y3 + (g24+g21*g14)*y4 + (g21*u1+u2)) / (1 - g21*g12)

# ATENÇÃO: note que todas as variaveis estão no mesmo periodo "t"
# a partir daí geram os erros u1 e u2:
u1 <- rnorm(t)
u2 <- rnorm(t)

#e finalmente a gente calcula o y2
y2 <- ((g23+g21*g13)*y3 + (g24+g21*g14)*y4 + (g21*u1+u2)) / (1 - g21*g12)


#### criando a série 1 ####
# com a série 2 em mãos criar a série 1 fica:
y1 <- g12 * y2 + g13 * y3 + g14 * y4 + u1
#ou do modo "extenso" (tem que dar a mesma coisa):
y1 <- ((g13+g12*g23)*y3 + (g14+g12*g24)*y4 + (u1+g12*u2)) / (1 - g21*g12)


#### PARTE 1 ####
#brincando de coitegração
#Primeiro todos elementos de Yt precisam ser I(1)
adf.test(y1)
adf.test(y2)
adf.test(y3)
adf.test(y4)

#tem algum I(2)?
adf.test(diff(y1))
adf.test(diff(y2))
adf.test(diff(y3))
adf.test(diff(y4))

#a estimação por MQO é consistente?
#estimando uma vez
fit1 <- lm(y1 ~ y2 + y3 + y4)
summary(fit1)

#agora repetindo isso uma caralhada de vezes
repetidor <- function(numrep = 100){
  for(i in 1:numrep){
    y <- simularSerie()
    fitx <- lm(y[[1]] ~ y[[2]] + y[[3]] + y[[4]])
    c1 <- coef(fitx)
    if(i == 1){
      c2 <- data.frame(c1)
    } else {
      c2 <- data.frame(c2, c1)
    }
  }
  return(t(c2))
}

fits <- repetidor(999)

#tiranda a famosa média
apply(fits, 2, mean)

#fazendo um ADF nos residuos (de uma da regressões)
adf.test(fit1$residuals)
plot(fit1$residuals, type = "l")

#plotando o histograma dos estimadores
#pra ver se parece uma normal
#e fazendo aquele jarque bera top
hist(fits[,2])
jarque.bera.test(fits[,2])

hist(fits[,3])
jarque.bera.test(fits[,3])

hist(fits[,4])
jarque.bera.test(fits[,4])


#### PARTE 2 ####
#fitando a regressão espuria
fit1 <- lm(y3 ~ y4)
summary(fit1)

#repetindo
repetidor <- function(numrep = 100){
  for(i in 1:numrep){
    y <- simularSerie()
    fitx <- lm(y[[3]] ~ y[[4]])
    c1 <- coef(fitx)
    if(i == 1){
      c2 <- data.frame(c1)
    } else {
      c2 <- data.frame(c2, c1)
    }
  }
  return(t(c2))
}

fits <- repetidor(999)

apply(fits, 2, mean)

fits <- simularSerie(t=10000)
fits <- simularSerie(t=1000000)
fit1 <- lm(fits[[3]] ~ fits[[4]])
summary(fit1)
#da pra ver que a estatistica t explode
#o p valor vai pra 0
#e o R2 teoricamente vai pra 1

hist(fits[,1])
jarque.bera.test(fits[,1])

hist(fits[,2])
jarque.bera.test(fits[,2])





library(urca)
summary(ca.jo(data.frame(y1,y2,y3,y4)))
