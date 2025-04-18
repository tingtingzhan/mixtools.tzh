


#' @title Sort `'mixEM'` Object by First Parameters
#' 
#' @description 
#' To \link[base]{sort} a `'mixEM'` object by its first parameters, i.e.,
#' \eqn{\mu}'s for normal mixture, \eqn{\alpha}'s for \eqn{\gamma}-mixture, etc.
#' 
#' @param x `'mixEM'` object
#' 
#' @param decreasing \link[base]{logical} scalar. Should the sort by \eqn{mu}'s 
#' be increasing (`FALSE`, default) or decreasing (`TRUE`)?
#' 
#' @param ... additional parameters, currently not in use
#' 
#' @details 
#' \link[mixtools]{normalmixEM} does *not* order the location parameter
#' 
#' @returns 
#' 
#' Function [sort.mixEM()] returns a `'mixEM'` object.
#' 
#' @keywords internal
#' @export sort.mixEM
#' @export
sort.mixEM <- function(x, decreasing = FALSE, ...) {
  ret <- x
  par1st <- switch(x[['ft']], normalmixEM = {
    x[['mu']]
  }, gammamixEM = {
    x[['gamma.pars']][1L, ]
  }, stop('not supported yet'))
  comp_seq <- paste0('comp.', seq_along(par1st))
  
  o <- order(par1st, decreasing = decreasing)
  if (length(names(x[['lambda']]))) stop('they now name `lambda`?')
  ret[['lambda']] <- x[['lambda']][o]
  ret[['posterior']] <- x[['posterior']][, o] # ?mixtools::normalmixEM names `posterior`
  colnames(ret[['posterior']]) <- comp_seq
  
  switch(x[['ft']], normalmixEM = {
    ret[['mu']] <- x[['mu']][o]
    ret[['sigma']] <- x[['sigma']][o]
  }, gammamixEM = {
    ret[['gamma.pars']] <- x[['gamma.pars']][, o]
    colnames(ret[['gamma.pars']]) <- comp_seq
  }, stop('not supported yet'))
  
  return(ret)
}


