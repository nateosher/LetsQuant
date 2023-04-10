GENERATE_BETA_CDF <- function(alpha, beta, p_grid, center_and_scale = TRUE) {
  cdf_vals = pbeta(p_grid, alpha, beta)
  if(center_and_scale)
    cdf_vals = (cdf_vals - mean(cdf_vals)) / sqrt(sum((cdf_vals - mean(cdf_vals))^2))


  return(cdf_vals)
}
