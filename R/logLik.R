


#' @title Log-Likelihood of `'mixEM'` Object
#' 
#' @description 
#' To obtain the log-Likelihood of `'mixEM'` object, based on \CRANpkg{mixtools} 2020-02-05.
#' 
#' @param object `'mixEM'` object, currently only the returned value of 
#' \link[mixtools]{normalmixEM} and \link[mixtools]{gammamixEM} are supported
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @returns 
#' Function [logLik.mixEM()] returns a \link[stats]{logLik} object.
#' 
#' @keywords internal
#' @importFrom stats logLik 
#' @export logLik.mixEM
#' @export
logLik.mixEM <- function(object, ...) {
  val <- object[['loglik']]
  attr(val, which = 'nobs') <- length(object[['x']])
  
  parnms <- switch(object[['ft']], normalmixEM = {
    c('mu', 'sigma')
  }, gammamixEM = {
    'gamma.pars'
  }, stop('mixEM fit type not programmed yet, its very simple though'))
  
  attr(val, which = 'df') <- (length(object[['lambda']]) - 1L) + sum(lengths(object[parnms], use.names = FALSE))
  class(val) <- 'logLik'
  return(val)
}







# NOT USED!!!

# @title Names of Distribution Parameters of `'mixEM'` Object
# 
# @description 
# Names of distribution parameters of `'mixEM'` object, based on \CRANpkg{mixtools} 2020-02-05.
# 
# @param object `'mixEM'` object, currently only the returned value of 
# \link[mixtools]{normalmixEM} and \link[mixtools]{gammamixEM} are supported
# 
# @returns 
# Function [mixEM_pars()] returns a \link[base]{character} \link[base]{vector}
# 
# @seealso 
# \link[mixtools]{normalmixEM} \link[mixtools]{gammamixEM}
# 
# @keywords internal
# @export
#mixEM_pars <- function(object) {
#  switch(object[['ft']], normalmixEM = {
#    c('mu', 'sigma')
#  }, gammamixEM = {
#    'gamma.pars'
#  }, stop('mixEM fit type not programmed yet, its very simple though'))
#}



