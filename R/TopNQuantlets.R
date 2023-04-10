#' @importFrom magrittr `%>%`
#' @export
TopNQuantlets = function(grid_tibble, selection_count_cutoff, orthogonalize = TRUE){
  quantlet_mat = grid_tibble %>%
    dplyr::filter(selection_counts >= selection_count_cutoff) %>%
    dplyr::arrange(dplyr::desc(selection_counts)) %>%
    dplyr::pull(p_grids) %>%
    do.call(what = rbind) %>%
    t() %>%
    pracma::gramSchmidt() %>%
    (\(GS) GS$Q)

  return(quantlet_mat)
}
