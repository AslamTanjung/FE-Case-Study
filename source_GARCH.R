determine_params <- function(params, p, q) {
  ## Take the vector of parameters and divide them over the individual parameters
  ## and apply transformation
  omega <- params[1]
  omega <- exp(omega)
  
  if (q == 0) {
    alpha <- 0
    beta <- params[2:length(params)]
    beta <- logistic(beta)
  } else if (p == 0) {
    beta <- 0
    alpha <- params[2:length(params)]
    alpha <- logistic(alpha)
  } else {
    alpha <- params[2:(q + 1)]
    beta <- params[(q + 2):length(params)]
    alpha <- logistic(alpha)
    beta <- logistic(beta)
  }
  list(omega = omega,
       alpha = alpha,
       beta = beta)
}

llik_fun_GARCH <- function(params, p, q, x) {
  ## Likelihood function of the GARCH multiplied by n
  params <- determine_params(params, p, q)
  var <- variance(x, params$alpha, params$beta, params$omega)
  l <- mean(-(1 / 2) * log(2 * pi) - (1 / 2) * log(var) - (1 / 2) * (x ^ 2 / var))
  return(l)
}

variance <- function(x, alpha, beta, omega) {
  n <- length(x)
  var <- rep(0,n)
  var[1] <- var(x)
  
  for(t in 2:n){
    var[t] = omega + alpha*x[t-1]^2 + beta*var[t-1]
  }
  var
}


estimate_GARCH <- function(x, alpha, beta) {
  ## Estimate the GARCH(p,q) model
  q <- length(alpha)
  p <- length(beta)
  omega <-  (1 - sum(alpha) - sum(beta)) * var(x)
  ## Transform the parameters
  if (p == 0) {
    params <- c(log(omega), inv_logistic(alpha))
  } else if (q == 0) {
    params <- c(log(omega), inv_logistic(beta))
  } else {
    params <-
      c(log(omega), inv_logistic(alpha), inv_logistic(beta))
  }
  
  est <-
    optim(
      par = params,
      fn = function(params)
        - llik_fun_GARCH(params, p, q, x),
      method = "BFGS"
    )
  params <- est$par
  ### Print out the information criterion and likelihood
  IC <- cbind(aic(-1*est$value*length(x), p+q+1), bic(-1*est$value*length(x), p+q+1, x), -1*est$value*length(x))
  colnames(IC) <- c("AIC", "BIC", "LogL")
  print(IC)
  determine_params(params, p, q)
}