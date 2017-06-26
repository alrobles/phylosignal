#' Convert \code{data.frame} to \code{list}.
#' @rdname read.annotated.nexus.par
#' @export
#' @param trees A \code{list} object with nexus tree names.
#' @examples
#' my_result <- foo(iris)
#'

read.annotated.nexus.par <- function(trees){
  ncores <- parallel::detectCores()-1
  CL <- parallel::makeCluster(ncores)
  parallel::clusterExport(cl = CL, list( paste0(trees), "read.annotated.nexus"), envir=environment() )
  treesif <- parallel::parLapply(CL, 1:length(trees), function(i) {OutbreakTools::read.annotated.nexus( trees[i]) })
  return(treesif)
}
