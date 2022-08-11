mixcensoredInt(y1 = data.sim$observed_value,
               y2 = data.sim$observed_value,
               d = rep(1, nrow(data.sim)),
               wt=rep(1, length(data.sim$observed_value)),
               dist="gaussian",
               n = 2,
               cluster=NULL,
               classify="EM",
               maxiter=10000, tol=1e-6)
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








