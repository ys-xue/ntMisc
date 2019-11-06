##' Dahl's Method for MCMC Inference
##'
##' @usage dahl(mcmc.object)
##'
##' @param mcmc.object an MCMC object
##'
##' @return a list:
##' \itemize{
##' \item min.sse the smallest SSE
##' \item cLS the index for the iteration that minimize SSE
##' \item cluster the final clustering outcome
##' }
##'
##' @importFrom purrr map map_dbl
##' @export

dahl <- function(mcmc.object) {
  niter <- nrow(mcmc.object)
  BList <- map(1:niter, ~outer(mcmc.object[.x,], mcmc.object[.x,], "=="))
  BBar <- Reduce("+", BList) / niter
  SSE <- map_dbl(BList, ~sum((.x - BBar)^2))
  return(list(min.sse = min(SSE), cLS = which.min(SSE),
              cluster = as.numeric(mcmc.object[which.min(SSE),])))
}
