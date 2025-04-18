

#' @title Convert `mixEM` Objects to \linkS4class{fmx} Class
#' 
#' @description
#' To convert `mixEM` objects (from package \CRANpkg{mixtools}) 
#' to \linkS4class{fmx} class.
#' 
#' Currently only the returned value of 
#' \link[mixtools]{normalmixEM} and \link[mixtools]{gammamixEM} are supported
#' 
#' @param x `mixEM` object
#' 
#' @param data \link[base]{numeric} \link[base]{vector}
#' 
#' @param ... ..
#' 
#' @note 
#' \link[mixtools]{plot.mixEM} not plot \link[mixtools]{gammamixEM} returns, as of 2022-09-19.
#' 
#' @returns 
#' Function [as.fmx.mixEM()] returns an \linkS4class{fmx} object.
#' 
#' @examples
#' library(mixtools)
#' library(fmx)
#' faithful$waiting |> 
#'   normalmixEM(k = 2) |> 
#'   as.fmx()
#' 
#' @keywords internal
#' @importFrom fmx as.fmx
#' @importClassesFrom fmx fmx 
#' @importFrom methods new
#' @method as.fmx mixEM
#' @export as.fmx.mixEM
#' @export
as.fmx.mixEM <- function(x, data = x[['x']], ...) {
  if (!length(data)) stop('wont happen')
  x <- sort.mixEM(x, decreasing = FALSE)
  
  switch(x[['ft']], normalmixEM = {
    pars <- cbind(mean = x[['mu']], sd = x[['sigma']])
    distname <- 'norm'
  }, gammamixEM = {
    pars <- x[['gamma.pars']] |> t.default()
    colnames(pars) <- c('shape', 'scale') # names of parameters of ?stats::dgamma
    # read \link[mixtools]{gammamixEM} carefully: 'beta' is actually `scale`
    distname <- 'gamma'
  }, stop(x[['ft']], ' not supported yet'))
  new(Class = 'fmx', 
      pars = pars, w = x[['lambda']], distname = distname,
      data = data)
}






