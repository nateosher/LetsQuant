#' @importFrom purrr map_dbl map
GetQuantletCoefficients = function(data_list, quantlet_matrix, progress = FALSE) {
  n = length(data_list)
  quantlet_coefs = vector('list', n)
  reconstructed_data = vector('list', n)

  p_grid_size = nrow(quantlet_matrix)

  # The common quantile grid has to be at least as long as the maximum number
  # of observations per patient
  if(p_grid_size < (map_dbl(data_list, ~ length(.x)) %>% max())){
    stop(paste("`p_grid_size` must be at least as large as maximum #",
               "observations across data set"))
  }

  data_list_common_grid = SameLengthData(data_list, p_grid_size)

  quantlet_coefs = map(data_list_common_grid, \(y){
    gen_inv = solve(t(quantlet_matrix) %*% quantlet_matrix) %*% t(quantlet_matrix)

    Q_i_star = gen_inv %*% y
    Q_i_star
  })

  if(progress)
    ProgressBar(n+1, n)

  return(quantlet_coefs)
}
