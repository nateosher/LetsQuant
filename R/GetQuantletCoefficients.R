#' @export
GetQuantletCoefficients = function(data_list, selected_quantlet_tib) {
  n = length(data_list)
  quantlet_coefs = vector('list', n)
  reconstructed_data = vector('list', n)
  selected_quantlet_tib = selected_quantlet_tib %>%
    mutate(
      index = 1:nrow(selected_quantlet_tib)
    )

  for (i in 1:n) {
    ProgressBar(i, n)
    y <- data_list[[i]]
    n_i <- length(y)
    p_grid <- seq(1 / (n_i + 1), n_i / (n_i + 1), 1 / (n_i + 1))

    # Using the notation from the paper
    PSI_tib = selected_quantlet_tib %>%
      mutate(
        quantlet_grids = group_map(selected_quantlet_tib %>% group_by(index), \(x, y){
          # browser()
          if(x$distribution == 'beta')
            return(GENERATE_BETA_CDF(x$a, x$b, p_grid, TRUE))
          else if(x$distribution == 'intercept')
            return(rep(1, length(p_grid)))
          else{
            std_norm_cdf_grid <- qnorm(p_grid, x$a, x$b)
            normalized_std_norm_cdf_grid <- (std_norm_cdf_grid - mean(std_norm_cdf_grid))/
              sqrt((std_norm_cdf_grid - mean(std_norm_cdf_grid))^2)
            return(normalized_std_norm_cdf_grid)
          }
        })
      )
    PSI_mat = do.call(rbind, PSI_tib$quantlet_grids) %>% t()
    gen_inv = solve(t(PSI_mat) %*% PSI_mat) %*% t(PSI_mat)

    Q_i_star = gen_inv %*% data_list[[i]]
    quantlet_coefs[[i]] = Q_i_star
    y_star = PSI_mat %*% Q_i_star
    reconstructed_data[[i]] = y_star
  }

  return(list(
    Q_star = quantlet_coefs,
    y_star = reconstructed_data
  ))
}
