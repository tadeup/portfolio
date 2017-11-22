crossValid <- function(df, MaxDefasagens=20, maxp = 20, posNoDf = 5){
  library(vars)
  predizer <- function(df, ordem = 1, h1 = 1, h2 = 0, posNoDf = 5){
    
    #df = matriz de variaveis endogenas
    #ordem = ordem do modelo VAR
    #h1 = horizonte minimo a ser predito
    #h2 = horizonte maximo adicional a ser predito
    #posNoDf = posição da variavel relavante no dataframe, no caso do tut é 5
    #caso o Masini fale "ah, mas eu não sei o que essa função predict faz":
    #https://cran.r-project.org/web/packages/vars/vars.pdf
    
    h2 <- h1 + h2
    fit <- VAR(df, ordem)
    a <- predict(fit, n.ahead = h2)
    y_predito <- a$fcst[[posNoDf]][,1][h1:h2]
    return(y_predito)
  }
  
  
  gerarSerie <- function(df, MaxDefasagens=20, ordem=1, h1 = 1, h2 = 0, posNoDf = 5){
    
    #MaxDefasagens = numero maximo de periodos que serão preditos
    #essa função cria uma série prevendo um periodo a frente a variavel de interesse
    #não recomendo mudar h1 nem h2
    
    resultSet <- df[,posNoDf]
    resultSet[(length(df[,posNoDf])-MaxDefasagens+1):length(df[,posNoDf])] <- 0
    for(i in 1:MaxDefasagens){
      serieX <- head(df, (length(df[,posNoDf])-MaxDefasagens+i))
      predicX <- predizer(serieX, ordem=ordem, h1=h1, h2=h2, posNoDf=posNoDf)
      resultSet[length(resultSet)-MaxDefasagens+i] <- predicX
    }
    return(resultSet)
  }
  
  
  calcResiduos <- function(serieOri, serieCortada, defasagens){
    
    #colocar "defasagens" como um numero maior ou igua ao MaxDefasagens das outras funções
    
    errosTot <- vector()
    for(i in 1:defasagens){
      erroX <- serieOri[length(serieOri)-defasagens+i] - serieCortada[length(serieOri)-defasagens+i]
      erroXX <- erroX^2
      errosTot <- append(errosTot, erroXX)
    }
    sumErros <- mean(errosTot)
    #sumErros <- sum(errosTot)
    return(sumErros)
  }
  
  #maxp = maior parametro P do modelo VAR que será testado no cross validation
  
  informacao <- vector()
  for(i in 1:maxp){
    seriePredita <- gerarSerie(df, MaxDefasagens = MaxDefasagens, ordem = i, posNoDf = posNoDf)
    erroQuadrado <- calcResiduos(df[,posNoDf], seriePredita, defasagens = MaxDefasagens)
    informacao <- append(informacao, erroQuadrado)
  }
  return(informacao)
}

#### multi threading ####
library(parallel)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

clusterCall(cl=cl, crossValid, df)

#finally
stopCluster(cl)
rm(cl)
