foo <- function (x, impulse, response, y.names, n.ahead, cumulative) 
{
  #essa função basicamente calcula o impulso resposta pra cada variavel
  #dependendo do lag das outras variaveis e dela mesma
  
  #a função Phi faz o segunte:
  #Returns the estimated coefficient matrices of the moving average representation of
  #a stable VAR(p), of an SVAR as an array or a converted VECM to VAR.
  irf <- Phi(x, nstep = n.ahead)
  dimnames(irf) <- list(y.names, y.names, NULL)
  
  idx <- length(impulse)
  irs <- list()
  
  for (i in 1:idx) {
    #esse loop extrai de forma bonita os coeficientes o modelo MA
    #pra cada uma das variaveis e salva numa lista
    
    irs[[i]] <- matrix(t(irf[response, impulse[i], 1:(n.ahead + 1)]), nrow = n.ahead + 1)
    print(irf[response, impulse[i], 1:(n.ahead + 1)])
    colnames(irs[[i]]) <- response
  }
  names(irs) <- impulse
  result <- irs
  return(result)
}



##########################################


svarest <- function (x, n.ahead = 10, cumulative = FALSE, boot = TRUE) 
{
  #fodasse essa função, ela só agrega uns trem ai pra deixar plotavel
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

