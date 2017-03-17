#' Estimate the parameters of a linear regression (intercept, slope, error standard deviation) 
#' with maximum likelihood estimations
#' 
#' 
#' @param par parameter triple (slope, intercept, error standard deviation)
#' @return deviance
#' @description Maximum likelihood estimation: Linear regression with one predictor
#' @references https://rpubs.com/YaRrr/MLTutorial
#' @export

# x: hits
# y: fa

lm.loss_x_y <- function(par) {
  
  a.par <- par[1]      # The current slope
  b.par <- par[2]      # The current intercept
  err.sigma <- par[3]  # The current error standard deviation
  
  # If the error standard deviation is invalid (i.e.; negative), then we need to return a very high deviance
  # This will tell the optimization procedure to stay away from invalid (either statistically or psychologically)
  # parameter values.
  
  if(err.sigma < 0) {deviance <- 10000000}
  
  # If the error standard deviation is valid (i.e.; > 0), then calculate the deviance...
  
  if(err.sigma > 0) {
    
    # Calculate the likelihood of each data point.
    # Here is where you specify the model and how you calculate likelihoods.
    
    likelihoods <- dnorm(x_data, mean = y_data * a.par + b.par, sd = err.sigma)
    
    # Now let's convert the vector of likelihoods to a summary deviance score (-2 times sub log-lik)
    
    # Calculate log-likelihoods of each data point
    log.likelihoods <- log(likelihoods)
    
    # Calculate the deviance (-2 times sum of log-likelihoods)
    deviance <- -2 * sum(log.likelihoods)
    
  }
  
  return(deviance)
  
}