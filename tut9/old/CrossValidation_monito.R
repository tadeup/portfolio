# 4)	Finalmente, a previsão...

# Considere a tarefa de fazer previsões para as séries usadas no problema anterior  

# PARTE 1: Você ouviu falar de um procedimento no qual a amostra 
# é dividida em duas sub-amostras, onde na primeira o modelo é estimado 
# e a segunda é usada para avaliar o poder preditivo do modelo.
# Você cogita usar este procedimento no lugar do framework que aprendeu nas lectures.

# PARTE 2: Uma vez escolhido o modelo, você não está muito confortável 
# com a hipótese de normalidade dos erros, principalmente após olhar o 
# histograma dos resíduos da estimação. Entretanto, gostaria de reportar 
# um intervalo de confiança das suas previsões ao seu chefe.

# pacotes
library(ggplot2)
library(ggfortify)
library(fpp)
library(zoo)
library(xts)

# seleciona o working directory
setwd("C:/Users/Maurício/Google Drive/Econometria II - 2017/Problema 4")

# ====================
#       Previsão
# ====================

# vamos simular uma série
# Set seed
set.seed(23082017)

# número de observações
n = 12*10

# definindo o modelo
modelo = list(ar=c(0.5), ma=c(-0.3,0.7)) #ARMA(1,2)

# constante
c = 5

# média incondicional
media = c/(1-sum(modelo$ar))

# nossa série
y = media + arima.sim(modelo,n,sd=0.025)
y = ts(y,start=c(2000,1),frequency = 12)

# plot da série
autoplot(y)

# separa um pedaço para treino
treino <- window(y, end=c(2008,12))

# separa um pedaço para teste
teste <- window(y,start=2009)

# faz forecast "bobo"
# função naive: pega a última observação como forecast
fc <- naive(treino,h=12)

# gráfico de forecast versus treino
autoplot(fc) + autolayer(teste)

# vamos definir uma função para somar os termos ao elevar os termos ao quadrado 
sq <- function(u) {u^2}

# Vamos criar num array C que vai armazenar o erro quadrático médio de forecast
# para diferentes horizontes h de forecast, utilizando o horizonte cross validation
hf = 3 # horizonte de forecast
K = 3 # ordem máxima de AR e MA. Lembre que 0,0 também é uma ordem. (K+1)^2 combinações

# vamos armazenar num array (AR,MA,HF)
C = array(dim=c(K+1,K+1,hf))
for (i in 0:K){ for(j in 0:K){
        
        # estabelece o modelo para ser input no tsCV
        
        fit <- function(x,h){forecast(arima(x,order=c(i,0,j)),h=h)}
        
        for(h in 1:hf){
                out <- tsCV(y,forecastfunction=fit,h=h)
                outsq <- sq(out)
                outsqmean <- mean(outsq,na.rm = TRUE)
                C[i+1,j+1,h] <- outsqmean
        }
}
}

# Visualização de C
View(C)

# Visualizações estilizadas de C
View(C[,,1])
View(C[,,2])
View(C[,,3])

# Escolha do melhor modelo ARMA com base no horizonte de forecast h
# cada linha da matrix D dá a melhor combinação de AR e MA 
# para o horizonte h=núm. da linha
D = matrix(NA,hf,2)
for(i in 1:hf) {D[i,] = (which(C[,,i]==min(C[,,i]),arr.ind=TRUE)-1)}
colnames(D) = c("AR","MA")
View(D)


# Model Selection baseada em critério de informação (Bayes Information Criterea)
B = matrix(NA,5,5)
for (i in 0:4) for (j in 0:4) {
        out = arima(treino,order = c(i, 0, j))
        B[i+1,j+1] = BIC(out)
}

model_ic = which(B==min(B),arr.ind=TRUE)-1
colnames(model_ic) = c("AR","MA")
model_ic


# Seleção de Modelo Baseada no MSFE
A = matrix(NA,5,5)
for (i in 0:4){
        for (j in 0:4){
                out = predict(arima(treino,order=c(i,0,j)),n.ahead=12)
                A[i+1,j+1]=mean((out$pred-teste)^2)
        }
}

model_fc = which(A==min(A),arr.ind=TRUE)-1
colnames(model_fc) = c("AR","MA")
model_fc
