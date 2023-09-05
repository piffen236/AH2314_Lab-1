## Code for estimation output.


library(numDeriv)




## 'rob_ste', function returning robust standard errors for estimated
## parameters.
##
## res    output object from 'nlm'.
##
## llc_fun   a function giving log-likelihood contributions for the
##           observations of the model estimated in 'res'-
##           
rob_ste = function(x, llc_fun)
{
    scores = numDeriv::jacobian(llc_fun, x$estimate)
    B = array(0, c(ncol(scores), ncol(scores)))
    for (n in 1:nrow(scores)) {
        B = B + scores[n,] %*% t(scores[n,])
    }

    H_inv = -chol2inv(chol(x$hessian)) # assumption: minimized -sum(llc_fun)

    return(sqrt(diag(H_inv %*% B %*% H_inv)))
}

p_vals = function(z, double_sided=TRUE, dist=pnorm)
{
    (if (double_sided) 2 else 1) * (1 - dist(abs(z)))
}

## see template.R for a description of the arguments
## 
est_summary = function(res, par_names=NULL, llc_fun)
{
    std_errors = rob_ste(res, llc_fun) #error
    z_ratios = res$estimate / std_errors
    pvals = p_vals(z_ratios)
    print(data.frame(
        Parameter=if (!is.null(par_names)) par_names else 1:length(res$estimate),
        Estimate=res$estimate,
        `Rob_ste`=std_errors,
        "Z ratio"=z_ratios,
        "P-value"=pvals,
        check_names=FALSE
    ))
    cat("\nMaximum log-likelihood:", -res$minimum, "\n", file="")
    cat("\nConvergence code:", res$code, "\n", file="")
    cat("Iterations:", res$iterations, "\n", file="")
}

