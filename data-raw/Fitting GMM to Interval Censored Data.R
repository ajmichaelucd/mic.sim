library(SPREDA)
library(LearnBayes)

##INTERVAL CENSORED DATA

#https://blogs2.datall-analyse.nl/2016/02/18/rcode_mixture_distribution_censored/

#function for fitting a mixture distribution to INTERVAL censored data

#NOTE: this function employs the type="interval" coding scheme as used
#in the "Surv" function (from the survival package) for interval censored data

mixcensoredInt <- function (y1, y2, d, wt=rep(1, length(y1)),
                            dist="gaussian", n, cluster=NULL, classify="EM",
                            maxiter=10000, tol=1e-6) {
  #y1 is the right/left censored value, the exact lifetime observation, or for
  # interval censoring the lower value of the censoring interval
  #y2 is the upper value of the censoring interval
  #d is the censoring indicator (0=right censored, 1=event at time,
  # 2=left censored, 3=interval censored)
  #wt are the weights for the observations
  #dist: either the "weibull", "lognormal", or "gaussian" distribution
  #n is the number of components
  #cluster: start with random initialization of posterior probabilities (=NULL), or
  # a matrix with n columns of initial posterior probabilities for the observations
  #classify: "EM", "CEM", or "SEM" strategy
  #maxiter is the maximum number of iterations
  #tol is the convergence criterion

  nobs <- sum(wt) #number of observations

  parms <- matrix(NA, ncol=3, nrow=n)
  colnames(parms) <- c("mu", "sigma", "logLik")
  posteriorP <- matrix(NA, ncol=n, nrow=length(y1))
  stdErr <- matrix(NA, ncol=2, nrow=n)
  colnames(stdErr) <- c("mu", "log(sigma)")
  P <- matrix(NA, ncol=n, nrow=length(y1))

  iteration <- 0 #initialize iteration counter
  loglikInit <- logLikC <- 0 #initialize log-likelihood estimates

  #initialize posterior probabilities
  if (is.null(cluster)) {
    alpha <- rep(.1, n)
    posteriorP <- rdirichlet(length(y1), alpha)}
  else {posteriorP <- cluster}

  while (iteration < maxiter) {
    #estimate prior probabilities
    priorP <- apply(wt*posteriorP, 2, sum)/nobs

    #estimate distribution parameters for each component
    if (classify=="EM"){
      for (i in 1:n) {
        wtp <- ifelse(wt*posteriorP[,i]<=0, 1e-15, wt*posteriorP[,i]) #E step
        cp <- survreg(Surv(y1,y2,d, type="interval")~1, weights=wtp, dist=dist) #M step
        parms[i,] <- c(coef(cp)[1], cp$scale, logLik(cp)[1])
        stdErr[i,] <- sqrt(diag(vcov(cp)))}
    }

    else if (classify=="CEM"){
      compPost <- apply(posteriorP, 1, which.max)
      for (i in 1:n) {
        cp <- survreg(Surv(y1,y2,d, type="interval")~1, weights=wt, dist=dist,
                      subset=(compPost==i))
        parms[i,] <- c(coef(cp)[1], cp$scale, logLik(cp)[1])
        stdErr[i,] <- sqrt(diag(vcov(cp)))}
    }

    else if (classify=="SEM"){
      compPost <- apply(posteriorP, 1, function (p) sample(x=1:n, size=1, prob=p))
      for (i in 1:n) {
        cp <- survreg(Surv(y1,y2,d, type="interval")~1, weights=wt, dist=dist,
                      subset=(compPost==i))
        parms[i,] <- c(coef(cp)[1], cp$scale, logLik(cp)[1])
        stdErr[i,] <- sqrt(diag(vcov(cp)))}
    }

    #compute the (complete) log-likelihood value
    logLikC <- sum(parms[,3]) + sum(wt*(posteriorP%*%log(priorP)))
    logLikC <- ifelse(is.na(logLikC), 0, logLikC)

    #estimate posterior probabilities
    if (dist=="weibull") {
      for (j in 1:n) {
        mu <- parms[j,1]; sigma <- parms[j,2]

        z1 <- (log(y1)-mu)/sigma
        z2 <- (log(y2)-mu)/sigma

        cp <- ifelse(d==0, 1-psev(z1), #right censoring
                     ifelse(d==1, 1/(sigma*y1) * dsev(z1), #exact observations
                            ifelse(d==2, psev(z1), #left censoring
                                   psev(z2)-psev(z1) ))) #interval censoring
        P[,j] <- priorP[j]*cp}
    }

    else if (dist=="lognormal") {
      for (j in 1:n) {
        mu <- parms[j,1]; sigma <- parms[j,2]

        z1 <- (log(y1)-mu)/sigma
        z2 <- (log(y2)-mu)/sigma

        cp <- ifelse(d==0, 1-pnorm(z1), #right censoring
                     ifelse(d==1, 1/(sigma*y1) * dnorm(z1), #exact observations
                            ifelse(d==2, pnorm(z1), #left censoring
                                   pnorm(z2)-pnorm(z1) )))
        P[,j] <- priorP[j]*cp}
    }

    else if (dist=="gaussian") {
      for (j in 1:n) {
        mu <- parms[j,1]; sigma <- parms[j,2]

        z1 <- (y1-mu)/sigma
        z2 <- (y2-mu)/sigma

        cp <- ifelse(d==0, 1-pnorm(z1), #right censoring
                     ifelse(d==1, 1/(sigma) * dnorm(z1), #exact observations
                            ifelse(d==2, pnorm(z1), #left censoring
                                   pnorm(z2)-pnorm(z1) )))
        P[,j] <- priorP[j]*cp}
    }

    posteriorP <- P/rowSums(P)

    #check convergence criterion
    if (( abs(logLikC-loglikInit) / (abs(loglikInit)+.001*tol) ) < tol) break
    loglikInit <- logLikC #update log-likelihood
    iteration <- iteration + 1 #increment counter
  }

  if (iteration == maxiter) warning("Maximum number of iterations exceeded")
  list(components=parms[,1:2], prior=priorP, loglik=logLikC,
       AIC=-2*logLikC + 2*(n-1+2*n), BIC=-2*logLikC + (n-1+2*n)*log(nobs),
       strategy=classify, distribution=dist, iterations=iteration,
       standardError=stdErr, posterior=posteriorP)
}
