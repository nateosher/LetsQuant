GENERATE_BETA_CDF_GRID <- function(alpha_vec, beta_vec, p_grid, center_and_scale = TRUE) {
  cdf_tibble = tidyr::expand_grid(alpha_vec, beta_vec) %>%
    mutate(centered_cdf_values = map2(alpha_vec, beta_vec, \(a, b){
      GENERATE_BETA_CDF(a, b, p_grid, center_and_scale)
    })
  )

  return(do.call(rbind, cdf_tibble$centered_cdf_values))
}
