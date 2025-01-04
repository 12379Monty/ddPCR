#  R CODE USED TO ANALYZE DATA FROM EXPERIMENT "Serial dilution of a synthetic gene"

# likelihood function may be maximized using either glm() or optim()



# Input dPCR data for serial dilution experiment
d = read.csv(file='dilutionData.csv')
y = d[,'npositive']
m = d[,'ntotal']
x = d[,'conc']

v = 0.91 / 1000  # constant volume (microliters) per droplet (physical constant)



# fit model using glm()
v.offset = rep(log(v), length(y))
ymat = cbind(y, m-y)
fit = glm(ymat~log(x), family=binomial(link='cloglog'), offset=v.offset)
beta.mle = fit$coefficients
beta.vcv = vcov(fit)
beta.se = sqrt(diag(beta.vcv))
alpha = 0.05  # significance level for confidence intervals
zcrit = qnorm(1-alpha/2)
beta.lowerCL = beta.mle - zcrit*beta.se
beta.upperCL = beta.mle + zcrit*beta.se
deviance = fit$deviance
GOF = 1 - pchisq(deviance, df=length(y)-length(beta.mle))


# fit model using optim()
lambda.repl = -(1/v) * log(1 - y/m)
fit.lm = lm(log(lambda.repl) ~ log(x))
beta.guess = round(fit.lm$coefficients, 2)
X = model.matrix(~log(x))
source('dPCR.Funcs.R')
fit = optim(par=beta.guess, fn=negLL, gr=negGradLL, method='BFGS', hessian=FALSE, y=y, m=m, v=v, X=X)
fit.hessian = negHessLL(fit$par, y=y, m=m, v=v, X=X)
beta.mle = fit$par
beta.vcv = chol2inv(chol(fit.hessian))
beta.se = sqrt(diag(beta.vcv))
alpha = 0.05  # significance level for confidence intervals
zcrit = qnorm(1-alpha/2)
beta.lowerCL = beta.mle - zcrit*beta.se
beta.upperCL = beta.mle + zcrit*beta.se
deviance = 2 * (fit$value - negLL.saturated(y=y, m=m, v=v))
GOF = 1 - pchisq(deviance, df=length(y)-length(beta.mle))


# Estimate concentration of each dilution
X.pred = unique(X)
lambda.est = rep(NA,dim(X.pred)[1])
lambda.lowerCL = rep(NA,dim(X.pred)[1])
lambda.upperCL = rep(NA,dim(X.pred)[1])
for (i in 1:dim(X.pred)[1]) {
    Xvec = matrix(X.pred[i,], ncol=1)
    loglambda.est = t(Xvec) %*% beta.mle
    loglambda.var = t(Xvec) %*% beta.vcv %*% Xvec
    lambda.est[i] = exp(loglambda.est)
    lambda.lowerCL[i] = exp(loglambda.est - zcrit*sqrt(loglambda.var))
    lambda.upperCL[i] = exp(loglambda.est + zcrit*sqrt(loglambda.var))
}

# Summarize results
beta.out = cbind(beta.mle, beta.se, beta.lowerCL, beta.upperCL)
dimnames(beta.out)[2] = list(c('MLE', 'SE', '2.5%', '97.5%'))

print(beta.out)
