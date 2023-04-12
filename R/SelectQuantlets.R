#' @importFrom magrittr `%>%`
#' @importFrom dplyr filter pull mutate arrange desc
#' @importFrom purrr map2_dbl
#' @export
SelectQuantlets = function(selection_counts, selection_count_cutoff,
                         grid_size = NULL, orthogonalize = TRUE,
                         force_intercept = TRUE){
  if(is.null(grid_size))
    grid_size = 1024

  intercept_included = "intercept" %in% (selection_counts %>%
                                           filter(
                                             selection_counts >= selection_count_cutoff
                                           ) %>%
                                           pull(distribution)
                                         )

  if(force_intercept || intercept_included)
    selection_counts = selection_counts %>%
      mutate(
        selection_counts = map2_dbl(distribution, selection_counts, \(dist, c){
          if(dist == "intercept") return(Inf)
          else return(c)
        })
      )

  quantlet_mat = selection_counts %>%
    filter(selection_counts >= selection_count_cutoff) %>%
    arrange(desc(selection_counts)) %>%
    MakeCDFGrids(grid_size = grid_size) %>%
    do.call(what = rbind) %>%
    t()

  if(orthogonalize){
    quantlet_mat = quantlet_mat %>%
      pracma::gramSchmidt() %>%
      (\(GS) GS$Q)
  }

  if(force_intercept || intercept_included)
    quantlet_mat[,1] = 1

  return(quantlet_mat)
}
