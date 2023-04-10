#' @import magrittr
#' @import dplyr
#' @import pracma
#' @export
TopNQuantlets = function(grid_tibble, selection_count_cutoff, orthogonalize = TRUE){
  quantlet_mat = grid_tibble %>%
    filter(selection_counts >= selection_count_cutoff) %>%
    arrange(desc(selection_counts)) %>%
    pull(p_grids) %>%
    do.call(what = rbind) %>%
    t() %>%
    pracma::gramSchmidt() %>%
    (\(GS) GS$Q)

  return(quantlet_mat)
}
