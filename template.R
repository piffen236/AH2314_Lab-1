## This R file shows an example on how to estimate a simple multinomial logit
## model.
##
## The structure of this file is
##
## 1. Sourcing of the three R files data.R, loglikelihoods.R, and output.R
##    (see lab_intstructions.pdf in https://canvas.kth.se/courses/41382/assignments)
##    Where you read-in and prepare data in data.R, write specification and
##    log-likelihood functions in log-likelihoods.R. The file output.R contains
##    functions for processing and printing estimation results.
##    
##
## 2. Specification of a model to estimate, given data prepared in data_R. This
##    is the setting of the variable MNL_SPEC by the function 'mnl_spec' below.
##
## 3. Log-likelihood estimation of model parameters,
##
## 4- Printing the result of the estimation
##
##
## To run the sample model estimation, you need to (1) read-in the data for
## Lab 1 and create the mode availability matrix 'avail', in the file data.r,
## and (2) to complete the log-likelihood function 'mnl_ll' which is given as
## a code skeleton in loglikelihoods.R.
##
## In an actual lab, you will write specification functions and log-likelihood
## functions and then iterate over step 2-4 above to find the best model.
##
## NOTE. 'est_summary' summary uses a Cholesky decomposition to invert the
##       Hession from the estimation. Any error message containng "x$hessian"
##       and/or "chol*" means that the Hessian is not numerically invertible.
##       This can be handled by scaling the parameters of the model or, in the
##       worst case, by deleting parameters from the model.
##        






## 1. Sourcing needed R-files 

source("data.R")
source("loglikelihoods.R")
source("output.R")


## 2. Model specification

## Setting of the variable MNL_SPEC by the function 'mnl_spec' below.
##
## Note that the utility for an alternative (mode) has to be a vector of
## length equal to the number of observations (rows) in 'data'. Usually, R can
## infer this from the variables in the utility expression. The exception is
## when a utility for an alternative contains only an alternative specific
## constant (ASC). The vector 'ones' containing only 1's is a remedy for this
## problem.
##
## See the comments in loglikelihoods.R what arguments must be given to 'mnl_spec'
## in order to specify an MNL model.
## 
ones <- rep(1, nrow(fw))
MNL_SPEC = mnl_spec(
    utils=c(
        u_car =       p[6]*BILKOST,
        u_pass=p[1] + p[6]*PASSKOST,
        u_bus =p[2] + p[6]*BUSSKOST,
        u_rail=p[3] + p[6]*TAGKOST,
        u_walk=p[4]*ones,
        u_bike=p[5]*ones,
        use.names=FALSE),
    data=fw,
    choice_var="MODE",
    nchoice=6,
    available=avail
)



## 3. Model log-likelihood estimation

## Estimation is done by the built-in R function 'nlm'. You should read the
## help pages for this function carefully. Take special care about the
## returned value 'code' which tells you whether the optimization converged,
## and if not, gives direction on how to make it converge.
##
## 'nlm' minimizes the objective function, therefore must the log-likelihood
## function be called with a minus-sign to make sure that it is maximized. In
## this case 'mnl_ll' is called as -mnl_ll
##
## When calling 'nlm' the objective function is must be a function of the
## parameter vector 'p' only. However, we want to pass the variable MNL_SPEC
## as an argument to our log-likelihood function 'mnl_ll'. Therefore, it is
## wrapped inside an anomymous function, as
##
##     function(p) { -mnl_ll(p, spec=MNL_SPEC) }
##
## which results in a function that has 'p' as its only argument.
##
## Always call nlm with 'hessian=TRUE' and 'print_level' at least set to 2. 
##

system.time(
    est_res  <-  nlm(function(p) { -mnl_ll(p, spec=MNL_SPEC) },
                   p=rep(0, 6), hessian=TRUE, print_level=2)
)



## 4. Printing the result of the estimation

## 'est_summary' prints a table of the estimated parameters, showing
##
##  1. Parameter names
##  2. Estimated values
##  3. Robust standard deviations of the estimates, se (Train, 2009, Sec. 8.6)
##  4. Z ratios (also called t ratios) of the estiamtes
##  5. P-values of the estimates.
##  6. In addition, the maximum log-likelihood value, the convergence code of
##     the 'nlm' maximization, and the number of iterations used for the
##     maximization is printed. 
##
## 'est_summary' takes the following arguments
##
##      res        the returned value from an 'nlm' optimization
##
##      llc_fun    the function giving the log-likelihood contribution for each
##                 observation in model data. This should be exactly the
##                 log-likelihood function given as objective function to
##                 'nlm', but with the argument 'sum=FALSE' added. In this case
##
##                      function(p) { -mnl_ll(p, spec=MNL_SPEC, sum=FALSE) }
##
##      par_names  a vector of parameter names (to p[1] - p[6], in this case,
##                 that you provide.
##                 
est_summary(res=est_res,
            llc_fun=function(p) { -mnl_ll(p, spec=MNL_SPEC, sum=FALSE) },
            par_names=c("asc_pass", "asc_bus", "asc_rail",
                        "asc_walk", "asc_bike", "cost")
            )
##
## Estimation output.
##
##   Parameter    Estimate   Rob. ste    Z ratio    P-value
## 1  asc_pass -1.45235594 0.04053298 -35.831462 0.00000000
## 2   asc_bus -1.59587339 0.05282285 -30.211802 0.00000000
## 3  asc_rail -1.50379854 0.11815977 -12.726823 0.00000000
## 4  asc_walk -3.80153209 0.12511497 -30.384310 0.00000000
## 5  asc_bike -1.41590133 0.04826013 -29.338949 0.00000000
## 6      cost  0.02177317 0.01085486   2.005845 0.04487279
## 
## Maximum log-likelihood: -6231.605 
## 
## Convergence code: 1 
## Iterations: 35 

