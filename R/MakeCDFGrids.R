#' @importFrom dplyr mutate group_by group_map
#' @importFrom magrittr `%>%`
#' @export
MakeCDFGrids = function(selection_counts, grid_size){
  p_grid = (1:grid_size)/(grid_size + 1)

  selection_counts %>%
    mutate(index = 1:n()) %>%
    group_by(index) %>%
    group_map(\(x, y){
      if(x$distribution == 'beta')
        return(GENERATE_BETA_CDF(x$a, x$b, p_grid, TRUE))
      else if(x$distribution == 'intercept')
        return(rep(1, length(p_grid)))
      else{
        std_norm_cdf_grid <- qnorm(p_grid, x$a, x$b)
        normalized_std_norm_cdf_grid <- (std_norm_cdf_grid - mean(std_norm_cdf_grid)) /
        sd(std_norm_cdf_grid)
        return(normalized_std_norm_cdf_grid)
      }
    })
}
