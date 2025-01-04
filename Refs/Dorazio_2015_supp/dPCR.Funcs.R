#  R FUNCTIONS USED IN NUMERICAL MAXIMIZATION OF LIKELIHOOD FUNCTION IN APPENDIX S1


# negative of log-likelihood function

negLL = function(beta, y, m, v, X) {
    lambda = as.vector(exp(X %*% beta))
    logL = y*log(1-exp(-lambda*v)) - lambda*v*(m-y)
    (-1)*sum(logL)
}

# negative of gradient of log-likelihood function

negGradLL =  function(beta, y, m, v, X) {
    lambda = as.vector(exp(X %*% beta))
    wvec = lambda * (y/(1-exp(-lambda*v)) - m)
    retVal = -v * as.vector(t(X) %*% wvec)
    retVal
}

# negative of hessian of log-likelihood function

negHessLL =  function(beta, y, m, v, X) {
    lambda = as.vector(exp(X %*% beta))
    uvec = lambda * ( y*(1-(1+lambda*v)*exp(-lambda*v))/(1-exp(-lambda*v))^2 - m)
    retVal = -v * t(X) %*% diag(uvec) %*% X 
    retVal
}

# negative of saturated log-likelihood function (used to compute deviance statistic)

negLL.saturated = function(y, m, v) {
    lambda = -(1/v)*log(1 - y/m)
    logL = y*log(1-exp(-lambda*v)) - lambda*v*(m-y)
    (-1)*sum(logL)
}
