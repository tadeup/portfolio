# Specifying functions:

CalcResiduals <- function(th, data) {
  # Calculates the e_t and h_t for the GARCH(1, 1) model with given parameters.
  #
  # Argumentss:
  #   th: Parameters
  #          th[1] -> mean
  #          th[2] -> alpha.0
  #          th[3] -> alpha.1
  #          th[4] -> beta.1
  #          th[5] -> sigma.0
  #          th[6] -> beta.1(coefficient of NIFTY)
  #          th[7] -> beta.2(coefficient of INR/USD)
  #          th[8] -> beta.3(coefficient of MIBOR)
  #   data: The input data
  #
  # Returns: A list containing et and ht.
  
  th[1] -> mean
  th[2] -> alpha.0
  th[3] -> alpha.1
  th[4] -> beta.1
  th[5] -> sigma.0
  th[6] -> a
  th[7] -> b
  th[8] -> c
  
  
  if(is.null(data$Niftychange) || is.null(data$Exchange.change) || is.null(data$mibor.change)) 
    print("Some column not present")
  
  # These are the residuals "y"
  
  y <- data$Component1 - a*data$Niftychange - b*data$Exchange.change - c*data$mibor.change
  
  
  n <- length(y)
  sigma.sqs <- vector(length=n) 
  sigma.sqs[1] <- sigma.0 ^ 2
  for(ii in c(1:(n-1))) { ## This loop is where the h_t are calculated
    sigma.sqs[ii + 1] <- (
      alpha.0 +
        alpha.1 * (y[ii] - mean) ^ 2 + 
        beta.1 * sigma.sqs[ii])
  }
  
  return(list(et = y, ht = sigma.sqs)) # Returns the list of e_t and h_t
}


# This is the second function

GarchLogL <- function(th, data) {
  # Calculates the Log-Likelihood of the GARCH(1, 1) model with given
  # parameters. It is intended for use with nlm or other optimization
  # routines to arrive at the best GARCH model. This can also be called for
  # subsets of the data and then summed to arrive at other models.
  #
  # Args:
  #   th : This is a vector containing the parameters for the model:
  # Returns:
  #   The negative conditional log likelihood of the model
  
  
  res <- CalcResiduals(th, data)  ## Recall our earlier function CalcResiduals()
  sigma.sqs <- res$ht
  y <- res$et
  
  # Assuming normal density of the errors dnorm() gives the density of normal dist.
  # this will return the negative of the log likelihood.
  
  return (-sum(dnorm(y[-1], mean=th[1] , sd=sqrt(sigma.sqs[-1]), log=TRUE))) 
}

# Another function

GarchLogLSimpl <- function(th, y) { # Only setting the mean of the errors to 0  
  GarchLogL(c(0, th), y)
}

# This is the main program where we will call all our functions defined above.
# The nlm() function performs the non-linear optimization with "p" as the initial 
# values of the parameters and then goes ahead with the iterations till the value
# of the likelihood function does not converge.

fit2 <- nlm(GarchLogLSimpl, # function call 
            p = rep(1,7), # initial values = 1 for all parameters
            y <- logretorno , # data to be used
            hessian = TRUE, # also return the hessian matrix
            iterlim = 500) # maximum iterations

sqrt(diag(solve(fit2$hessian))) # standard errors

# Squre root of the Diagonal of the inverse of the hessian matrix gives the standard
# errors of the estimates