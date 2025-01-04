#  R CODE USED TO ANALYZE DATA FROM EXPERIMENT "Expression of SDPR gene"

# likelihood function is maximized using optim()


# load required R libraries
library(fastGHQuad)
library(mvtnorm)


# Definitions of functions used in the analysis

# ... negative of log-likelihood function for mixed model using adaptive Gauss-Hermite quadrature
negLL.mixed.adaptive = function(param, y, m, v, X, Z, subj, ghRule) {
    beta = param[1:ncol(X)]
    Lvec = param[-(1:ncol(X))]
    L = diag(exp(Lvec[1:2]))  # L = Cholesky factor of Sigma
    L[2,1] = Lvec[3]
    Sigma = L %*% t(L)
    phi = as.vector(X %*% beta)

    # assign quadrature basepoints and weights
    npoints = length(ghRule$x)
    b1 = rep(ghRule$x, npoints)
    b2 = rep(ghRule$x, each=npoints)
    wt1 = rep(ghRule$w, npoints)
    wt2 = rep(ghRule$w, each=npoints)
    b = cbind(b1, b2)
    logOfwt = log(wt1) + log(wt2) + b1*b1 + b2*b2

    # evaluate log-likelihood for each subject
    nsubj = max(subj)
    logL = rep(NA, nsubj)
    for (k in 1:nsubj) {
        ind = subj==k
        yvec = y[ind]
        mvec = m[ind]
        phiVec = phi[ind]
        Zmat = Z[ind, ]

        bModeGuess = c(0,0)
        fit = optim(par=bModeGuess, fn=negConditionalLL, method='BFGS', hessian=TRUE, y=yvec, m=mvec, v=v, phi=phiVec, Z=Zmat, Sigma=Sigma)
        bMode = fit$par
        bCov = chol2inv(chol(fit$hessian))
        bL = t(chol(bCov))
        bModeMat = matrix(rep(bMode, each=nrow(b)), nrow=nrow(b))
        
        bmat = bModeMat + sqrt(2) * (b %*% t(bL))
        integrandVec = ConditionalLogLikelihood(b=bmat, y=yvec, m=mvec, v=v, phi=phiVec, Z=Zmat, Sigma)
        logL[k] = log(2) + 0.5*log(det(bCov)) + log( sum(exp(integrandVec + logOfwt)) )
    }
    (-1)*sum(logL)
}

# ... negative of conditional likelihood used to find mode of integrand
negConditionalLL = function(b, y, m, v, phi, Z, Sigma) {
    zero = rep(0,length(b))
    lambda = exp(phi + as.vector(Z %*% b))
    logL = dmvnorm(b, mean=zero, sigma=Sigma, log=TRUE) + sum(dbinom(y, size=m, prob=1-exp(-lambda*v), log=TRUE))
    (-1)*logL
}

# ... integrand for adaptive quadrature
ConditionalLogLikelihood = function(b, y, m, v, phi, Z, Sigma) {
    zero = rep(0,ncol(b))
    dbinomMat = matrix(nrow=nrow(b), ncol=length(y))
    for (j in 1:length(y)) {
        lambda = exp(phi[j] +  b %*% Z[j,])
        dbinomMat[, j] = dbinom(y[j], size=m[j], prob=1-exp(-lambda*v), log=TRUE)
     }
    retVal = apply(dbinomMat, 1, sum) + dmvnorm(b, mean=zero, sigma=Sigma, log=TRUE)
    retVal
}




# Input dPCR data for gene expression
d = read.csv(file='geneExpressData.csv')
y = d[,'npositive']
m = d[,'ntotal']
targ = as.character(d[,'gene'])
trt = as.character(d[,'treatment'])
subj = as.character(d[,'subject'])

v = 0.91 / 1000  # constant volume (microliters) per droplet (physical constant)



# compute summary statistics for each combination of target and trt
ysum = tapply(y, list(trt, targ), sum)
msum = tapply(m, list(trt, targ), sum)
meanCopyNum = -log(1 - ysum/msum)
meanConc = -log(1 - ysum/msum) / v



# fit model w/o random effects using glm()
v.offset = rep(log(v), length(y))
ymat = cbind(y, m-y)
fit = glm(ymat~targ*trt, family=binomial(link='cloglog'), offset=v.offset)
beta.mle = fit$coefficients
beta.vcv = vcov(fit)
beta.se = sqrt(diag(beta.vcv))


# fit model w/o random effects using optim()
lambda.repl = -(1/v) * log(1 - y/m)
fit.lm = lm(log(lambda.repl) ~ targ*trt)
beta.guess = round(fit.lm$coefficients, 2)
X = model.matrix(~targ*trt)
source('dPCR.Funcs.R')
fit = optim(par=beta.guess, fn=negLL, gr=negGradLL, method='BFGS', hessian=FALSE, y=y, m=m, v=v, X=X)
fit.hessian = negHessLL(fit$par, y=y, m=m, v=v, X=X)
beta.mle = fit$par
beta.vcv = chol2inv(chol(fit.hessian))
beta.se = sqrt(diag(beta.vcv))


# fit model w/ random effects using optim()
X = model.matrix(~targ*trt)
Z = model.matrix(~targ-1)
SigmaGuess = matrix(c(0.1,0,0,0.1), nrow=2)
dimnames(SigmaGuess) = list(c('Ref','Targ'), c('Ref','Targ'))
Lguess = t(chol(SigmaGuess))
LvecGuess = c(log(diag(Lguess)), Lguess[2,1])
paramGuess = c(beta.mle, LvecGuess)

# ... use adaptive Gauss-Hermite quadrature
ghRule = gaussHermiteData(40)
fit.mixed = optim(par=paramGuess, fn=negLL.mixed.adaptive, method='Nelder-Mead', hessian=TRUE, control=list(maxit=5000), y=y, m=m, v=v, X=X, Z=Z, subj=as.integer(factor(subj)), ghRule=ghRule)


# compute MLE of beta and Sigma and SEs of beta
param.mle = fit.mixed$par
param.vcv = chol2inv(chol(fit.mixed$hessian))
param.se = sqrt(diag(param.vcv))
beta.mle = param.mle[1:dim(X)[2]]
beta.vcv = param.vcv[1:dim(X)[2], 1:dim(X)[2]]
beta.se = sqrt(diag(beta.vcv))

Lvec.mle = param.mle[-(1:ncol(X))]
L.mle = diag(exp(Lvec.mle[1:2]))
L.mle[2,1] = Lvec.mle[3]
Sigma.mle = L.mle %*% t(L.mle)



# estimate expression of target gene (SDPR)
R.estimates = matrix(nrow=3, ncol=3)
dimnames(R.estimates) = list(c('NegSel','PosSel','Sort'),c('Estimate','2.5%','97.5%'))

alpha = 0.05  # significance level for confidence intervals
zcrit = qnorm(1-alpha/2)

# .... for treatment NegSel
Xvec = matrix(c(0,1,0,0,0,0), ncol=1)
logR.est = t(Xvec) %*% beta.mle
logR.var = t(Xvec) %*% beta.vcv %*% Xvec

R.est = exp(logR.est)
R.lowerCL = exp(logR.est - zcrit*sqrt(logR.var))
R.upperCL = exp(logR.est + zcrit*sqrt(logR.var))
R.estimates[1,] = c(R.est, R.lowerCL, R.upperCL)


# .... for treatment PosSel
Xvec = matrix(c(0,1,0,0,1,0), ncol=1)
logR.est = t(Xvec) %*% beta.mle
logR.var = t(Xvec) %*% beta.vcv %*% Xvec

R.est = exp(logR.est)
R.lowerCL = exp(logR.est - zcrit*sqrt(logR.var))
R.upperCL = exp(logR.est + zcrit*sqrt(logR.var))
R.estimates[2,] = c(R.est, R.lowerCL, R.upperCL)

# .... for treatment Sort
Xvec = matrix(c(0,1,0,0,0,1), ncol=1)
logR.est = t(Xvec) %*% beta.mle
logR.var = t(Xvec) %*% beta.vcv %*% Xvec

R.est = exp(logR.est)
R.lowerCL = exp(logR.est - zcrit*sqrt(logR.var))
R.upperCL = exp(logR.est + zcrit*sqrt(logR.var))
R.estimates[3,] = c(R.est, R.lowerCL, R.upperCL)


# Output estimates of gene expression
print(R.estimates)
