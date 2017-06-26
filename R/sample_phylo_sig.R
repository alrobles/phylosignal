#' Gets \code{tree} phylogenetic signal from trait to \code{matrix}.
#' @rdname sample_phylo_sig
#' @export
#' @param trees A \code{list} object with nexus tree.
#' @param traits A \code{matrix} with traits.
#' @param s A \code{numeric} integer. Is the sample size.
#' @examples
#' my_result <- foo(iris)

sample_phylo_sig <- function(trees, traits, s){
  S <- sample(trees, s)
  K <- lapply(S, function(x){
    n <- which(x$tip.label == traits[, 1] )
    sapply(traits[n, -1],  function(y){
      phytools::phylosig(x , y, method="K",test=TRUE, nsim = 1000)})  })
  l_K <- lapply(K, function(x){x[1, ]})
  l_K_p<- lapply(K, function(x){x[2, ]})
  df_K <- do.call(rbind, l_K)
  df_K_p <- do.call(rbind, l_K_p)
  d_K <- apply(df_K, 2, function(x){
    x %>% as.data.frame() %>% as.numeric %>% stats::density })
  max_K <- lapply(d_K, function(i){
    i$x[match(max(i$y),  i$y)] })
  l <- list(d_K, df_K, max_K, df_K_p )
  names(l) <- c("densities", "data.frame", "max", "p-value")
  return(l)
}

