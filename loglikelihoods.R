## Code for your log-likelihood functions


## MNL log-likelihood

## 'mnl_spec' returns a list of containing data on the MNL model specification.
## This list is supposed to ge input as the 'spec' argument to the
## log-likelihood function 'mnl_ll'.
##
## The arguments to the function is
##
## utils    a vector of utility expressions. When evaluated it should results
##          in a vector which is (# observations) times (# choices) long
##          consisting of the stacked vectors of utilities for ech choice.
##          An example of how such an utilty expression should look like is
##          given in template.R
##
## data     an R data frame containg the choice variable (of chosen
##          alternatives of the observations), and possible utility attributes.
##
## choice_var    a character string giving the name of the choice variable in
##               'data'.
##
## nchoice  an integer giving the number of alternatives (choices).
## 
## avail    a logical (TRUE/FALSE) of dimensions
##         (# of observations) x (# of alternatives) indicating the
##         availabilities of an alternative for an observation in 'data'.
##         
mnl_spec <- function(utils, data, choice_var, nchoice, available=NULL)
{    
    return(list(
        utils=substitute(utils),
        data=data,
        nobs=nrow(data),
        nchoice=nchoice,
        avail=log(available), # converts the TRUE/FALSE matrix to a 0/-Inf matrix
        index.chosen=array(c(1:nrow(data), data[,choice_var]), c(nrow(data), 2))
    ))
}

## Sample code for your likelihood functions

## 'mnl_ll' returns the log-likelihood of an MNL model specified by the 'spec'
## argument. Optionally, returns the vector of choice probabilities for the
## observations when 'probs=TRUE', and the log-likelihood contribution for the
## observations when 'sum=TRUE'.
## 
mnl_ll <- function(p, spec, probs=FALSE, sum=TRUE)
{
    ## 'u' is a matrix with dimension (no. of observations) x (no. of choices),
    ## i.e. observation index are rows and choice index are columns.
    #browser()
    u <- array(eval(spec$utils, spec$data), c(spec$nobs, spec$nchoice))
    #print(u)
    
    ## The rest of your likelihood code goes below this comment.

    ## I add the availability matrix to the utility matrix
    ## This "removes" (sets to -inf) the unavailable modes
    u <- u + spec$avail
    
    ## calc the exp of u, this will bethe numerator
    ## creat a empty array
    chosen_probabilities <- numeric(nrow(spec$index.chosen))
    ## Loop over each row to get the corresponding probability
    ## for the chosen alternative
    #browser()
    for(i in 1:nrow(spec$index.chosen)) {
      row_idx <- spec$index.chosen[i, 1]
      col_idx <- spec$index.chosen[i, 2]
      chosen_probabilities[i] <- exp(u[i, col_idx])/sum(exp(u[i,]))
    }

    epsilon <- 1e-10
    chosen_probabilities <- chosen_probabilities + epsilon
    
    p_mnl<-chosen_probabilities
    #browser()
    #print(p_mnl)
    #print(sum(log(p_mnl)))

    return(if (probs) {
      p_mnl
           } else if (sum) {
               sum(log(p_mnl))
           } else {
               log(p_mnl)
           })


}

gnl_spec <- function(utils, data, choice_var, nchoice, available=NULL, nest_matrix) {  
  return(list(
    utils=substitute(utils),
    data=data,
    nobs=nrow(data),
    nchoice=nchoice,
    avail=log(available),
    nest_matrix=nest_matrix,
    index.chosen=array(c(1:nrow(data), data[,choice_var]), c(nrow(data), 2))
  ))
}

gnl_ll <- function(p, spec, probs=FALSE, sum=TRUE) {
  u <- array(eval(spec$utils, spec$data), c(spec$nobs, spec$nchoice))
  u <- u + spec$avail
  
  lambda <- p[10]
  
  # Denominator Part
  denominator <- numeric(nrow(u))
  for (nest in 1:ncol(spec$nest_matrix)) {
    nest_value <- numeric(nrow(u))
    for (choice in 1:ncol(u)) {
      if (spec$nest_matrix[choice, nest] == 1) {
        nest_value <- nest_value + exp(u[, choice])^(1 / lambda)
      }
    }
    denominator <- denominator + nest_value^lambda
  }
  
  # Numerator and Overall Probability
  chosen_probabilities <- numeric(nrow(spec$index.chosen))
  for (i in 1:nrow(spec$index.chosen)) {
    row_idx <- spec$index.chosen[i, 1]
    col_idx <- spec$index.chosen[i, 2]
    
    nest_for_choice = which(spec$nest_matrix[col_idx, ] == 1)
    nest_value <- sum(exp(u[row_idx, spec$nest_matrix[, nest_for_choice] == 1])^(1 / lambda))
    
    chosen_probabilities[i] <- (exp(u[row_idx, col_idx])^(1 / lambda) * nest_value^(lambda - 1)) / denominator[row_idx]
  }
  
  epsilon <- 1e-10
  chosen_probabilities <- chosen_probabilities + epsilon
  
  return(if (probs) {
    chosen_probabilities
  } else if (sum) {
    sum(log(chosen_probabilities))
  } else {
    log(chosen_probabilities)
  })
}
