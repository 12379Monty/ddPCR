#  R CODE USED TO ANALYZE DATA FROM EXPERIMENT "Copy number variation of MYC gene"

# likelihood function may be maximized using either glm() or optim()



# Input dPCR data for CNV samples

d = read.csv(file='BioRadData-CNV.csv')
yVec = d[,'Positives']
mVec = d[,'TotalDroplets']
sampleID = as.character(d[,'Sample'])
targetID = as.character(d[,'Target'])

v = 0.91 / 1000  # constant volume (microliters) per droplet (physical constant)


# Estimate copy number ratio for each sample
sample = unique(sampleID)
R.estimates = matrix(nrow=length(sample), ncol=3)
dimnames(R.estimates) = list(sample, c('CNV', '2.5%', '97.5%'))
GOF = rep(NA, length(sample))
names(GOF) = sample
for (i in 1:length(sample)) {

    ind = sampleID==sample[i]
    y = yVec[ind]
    m = mVec[ind]
    target = targetID[ind]

    # ... fit model using glm()
    v.offset = rep(log(v), length(y))
    ymat = cbind(y, m-y)
    fit = glm(ymat~target-1, family=binomial(link='cloglog'), offset=v.offset)
    beta.mle = fit$coefficients
    beta.vcv = vcov(fit)
    beta.se = sqrt(diag(beta.vcv))
    alpha = 0.05  # significance level for confidence intervals
    zcrit = qnorm(1-alpha/2)
    beta.lowerCL = beta.mle - zcrit*beta.se
    beta.upperCL = beta.mle + zcrit*beta.se
    deviance = fit$deviance

    
    # ... fit model using optim()
    lambda.repl = -(1/v) * log(1 - y/m)
    lambda.guess = tapply(lambda.repl, target, mean)
    beta.guess = log(lambda.guess)
    X = model.matrix(~target-1)
    source('dPCR.Funcs.R')
    fit = optim(par=beta.guess, fn=negLL, gr=negGradLL, method='BFGS', hessian=FALSE, y=y, m=m, v=v, X=X)
    fit.hessian = negHessLL(fit$par, y=y, m=m, v=v, X=X)
    beta.mle = fit$par
    beta.vcv = chol2inv(chol(fit.hessian))
    beta.se = sqrt(diag(beta.vcv))
    alpha = 0.05  # significance level for confidence intervals
    zcrit = qnorm(1-alpha/2)
    deviance = 2 * (fit$value - negLL.saturated(y=y, m=m, v=v))

    # ... compute estimate of copy number ratio
    Xvec = matrix(c(-1, 1), ncol=1)
    logR.est = t(Xvec) %*% beta.mle
    logR.var = t(Xvec) %*% beta.vcv %*% Xvec

    R.est = exp(logR.est)
    R.lowerCL = exp(logR.est - zcrit*sqrt(logR.var))
    R.upperCL = exp(logR.est + zcrit*sqrt(logR.var))

    R.estimates[i, ] = c(R.est, R.lowerCL, R.upperCL)

    GOF[i] = 1 - pchisq(deviance, df=length(y)-length(beta.mle))
}


# Output estimates of copy number ratio and p-value for goodness of fit

R.out = cbind(R.estimates, GOF)
print(R.out)

