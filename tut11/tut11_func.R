simularSerie <- function(t = 100, 
                         g12 = 0.2,
                         g13 = 0.3,
                         g14 = 0.4,
                         g21 = 0.5,
                         g23 = 0.6,
                         g24 = 0.7){
  y4 <- numeric(t)
  for(i in 2:t){
    y4[i] <- y4[i-1] + rnorm(1)
  }
  
  y3 <- numeric(t)
  for(i in 2:t){
    y3[i] <- y3[i-1] + rnorm(1)
  }
  
  u1 <- c(0, rnorm(t-1))
  u2 <- c(0, rnorm(t-1))
  
  y2 <- ((g23+g21*g13)*y3 + (g24+g21*g14)*y4 + (g21*u1+u2)) / (1 - g21*g12)
  
  y1 <- g12 * y2 + g13 * y3 + g14 * y4 + u1
  #y1 <- ((g13+g12*g23)*y3 + (g14+g12*g24)*y4 + (u1+g12*u2)) / (1 - g21*g12)
  
  
  df <- list(y1, y2, y3, y4)
  return(df)
}