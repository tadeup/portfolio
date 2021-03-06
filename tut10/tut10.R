library(vars)
library(TED)
library(tseries)

#procedimento padr�o
setwd("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut10")

inflacao <- read.csv("inflacao.csv", sep = "\t")
inflacao <- as.numeric(gsub(",", ".", as.character(inflacao[,3])))
inflacao <- inflacao[!is.na(inflacao)]

juros <- read.csv2("selic.csv")
juros <- as.numeric(gsub(",", ".", as.character(juros[,2])))
juros <- juros[!is.na(juros)]

juros <- ts(juros, start = c(1986, 7), frequency = 12)
inflacao <- ts(inflacao, start = c(1980, 1), frequency = 12)

#### vamo la

#claramente tinha uma quebra estrutural na variancia antes de 2000 e pouco, n�o quero lidar com isso
df <- ts.intersect(juros, inflacao)
df <- window(df, start = c(2000, 1))
plot(df[,1])
plot(df[,2])

#adf pra ver se ta tudo cert
apply(df, 2, adf.test)
#os juros deu meio ruim, mas na teoria ele n�o pode ir pra baixo de 0, ent�o...

#ja ecolhemos VAR 300 vezes, ent�o vamo na maldade
VARselect(df)
fit <- VAR(df, p = 5)
res <- residuals(fit)
#passando awuele ljung box pra ver se tamo suave na nave
apply(res, 2, Box.test)

#BYt = AY + Ut
#Ou voce normaliza o B ou o U
#normalizando os choques estruturais temos um U ~ (0, In)
#SIGMA = var(Yt) = var(et) = t(C) %*% var(Ut) %*% C = t(C) %*% C
sigm <- cov(res)
c_matrix <- chol(sigm)

#s� checando esse a fun��o chol() faz o que ela promete msmo
t(c_matrix) %*% c_matrix

#agora da pra achar a matriz A e a matriz B
a_matrix <- lapply(Acoef(fit), function(x){solve(c_matrix) %*% x})
b_matrix <- solve(c_matrix)

#temb�m tem a famosa fun��o
restr <- diag(2)
#restr[upper.tri(restr, diag = T)] <- NA
restr[lower.tri(restr, diag = T)] <- NA
fit2 <- SVAR(fit, Amat = restr)
#como podemos ver da mais ou meno parecido
#na duvida bota a culpa nos floating points

#agora da pra plotar a IRF
#ir para o anexo - "source_simplificado" ou pro "source" por sua conta e risco
plot(irf(fit2))
plot(irf(fit2, n.ahead = 40))
plot(irf(fit2, n.ahead = 5))


####FAZENDO A MESMA COISA S� QUE INVERTENDO A ORDEM O CHUL�SKI
df <- ts.intersect(inflacao, juros)
df <- window(df, start = c(2000, 1))
fit <- VAR(df, p = 5)
res <- residuals(fit)
sigm <- cov(res)
c_matrix <- chol(sigm)
t(c_matrix) %*% c_matrix
a_matrix <- lapply(Acoef(fit), function(x){solve(c_matrix) %*% x})
b_matrix <- solve(c_matrix)
restr <- diag(2)
restr[lower.tri(restr, diag = T)] <- NA
fit2 <- SVAR(fit, Amat = restr)
plot(irf(fit2))
plot(irf(fit2, n.ahead = 40))
plot(irf(fit2, n.ahead = 5))
####



