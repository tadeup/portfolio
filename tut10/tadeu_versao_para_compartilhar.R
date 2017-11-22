############################################################################
############################################################################
####                                                                    ####
####                          tutorial 10                               ####
####                    feito por TADEU LARA                            ####
####          versão para compartilhar com as outras turmas             ####
####            "pode copiar, só não faz igualzinho"                    ####
####  eu podia ter comentado mais o codigo? podia, mas se virem ai tb   ####
####                                                                    ####
############################################################################
############################################################################


library(vars)
library(TED)
library(tseries)


setwd("C:\\Users\\User 2014\\Desktop\\FGV\\FGV\\6Semestre\\Econometria 2\\tut10")
inflacao <- read.csv("inflacao.csv", sep = "\t")
inflacao <- as.numeric(gsub(",", ".", as.character(inflacao[,3])))
inflacao <- inflacao[!is.na(inflacao)]
juros <- read.csv2("selic.csv")
juros <- as.numeric(gsub(",", ".", as.character(juros[,2])))
juros <- juros[!is.na(juros)]
juros <- ts(juros, start = c(1986, 7), frequency = 12)
inflacao <- ts(inflacao, start = c(1980, 1), frequency = 12)
df <- ts.intersect(juros, inflacao)
df <- window(df, start = c(2000, 1))

plot(df[,1])
plot(df[,2])

apply(df, 2, adf.test)

VARselect(df)

fit <- VAR(df, p = 5)
res <- residuals(fit)
apply(res, 2, Box.test)

sigm <- cov(res)
c_matrix <- chol(sigm)
a_matrix <- lapply(Acoef(fit), function(x){solve(c_matrix) %*% x})
b_matrix <- solve(c_matrix)
#USANDO FUNÇÃO DO R:
restr <- diag(2)
restr[lower.tri(restr, diag = T)] <- NA
fit2 <- SVAR(fit, Amat = restr)
plot(irf(fit2))
plot(irf(fit2, n.ahead = 40))
plot(irf(fit2, n.ahead = 5))
#### anexos ####


#a ideia aqui é deixar explicito o que a função irf faz
foo <- function (x, impulse, response, y.names, n.ahead, cumulative) 
{
  irf <- Phi(x, nstep = n.ahead)
  dimnames(irf) <- list(y.names, y.names, NULL)
  
  idx <- length(impulse)
  irs <- list()
  
  for (i in 1:idx) {
    irs[[i]] <- matrix(t(irf[response, impulse[i], 1:(n.ahead + 1)]), nrow = n.ahead + 1)
    print(irf[response, impulse[i], 1:(n.ahead + 1)])
    colnames(irs[[i]]) <- response
  }
  names(irs) <- impulse
  result <- irs
  return(result)
}


#essa função só ajuda na hora de deixar plotave
#plote o output dela, e não o da função "foo"
svarest <- function (x, n.ahead = 10, cumulative = FALSE, boot = TRUE) 
{
  y.names <- colnames(x$var$y)
  impulse <- y.names
  response <- y.names
  
  irs <- foo(x = x, impulse = impulse, response = response, 
             y.names = y.names, n.ahead = n.ahead, 
             cumulative = cumulative)
  result <- list(irf = irs, response = response, 
                 impulse = impulse, cumulative = cumulative, 
                 boot = boot, model = class(x))
  class(result) <- "varirf"
  return(result)
}


xxx <- svarest(fit2, n.ahead = 10)
plot(xxx)