#' Gets \code{tree} phylogenetic signal from trait to \code{matrix}.
#' @rdname bootstrap_phylo_sig
#' @importFrom foreach %dopar%
#' @export
#' @param trees A \code{list} object with nexus tree.
#' @param traits A \code{matrix} with traits.
#' @param sample A \code{numeric} integer. Is the sample size.
#' @param rep A \code{numeric} integer. Is the sample size.
#' @examples
#' my_result <- foo(iris)
#'
bootstrap_phylo_sig <- function(trees, traits, sample, rep){
  cl <- parallel::makeCluster( parallel::detectCores() - 1)
  doParallel::registerDoParallel(cl)
  boot <- foreach::foreach(i = 1:rep, .export=c('sample_phylo_sig'),
                           .packages=c('phytools', 'magrittr') ) %dopar%{
            phylosignal::sample_phylo_sig(trees, traits, sample) }
  parallel::stopCluster(cl)
  return(boot)
}


