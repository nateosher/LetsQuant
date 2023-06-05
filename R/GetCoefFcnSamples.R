#' @export
GetCoefFcnSamples = function(qmat, reg_output){
  qmt = t(qmat)

  # Dimensions
  p = dim(reg_output$beta_sample_array)[1]
  n_quantlets = dim(reg_output$beta_sample_array)[2]
  n_samples = dim(reg_output$beta_sample_array)[3]
  grid_size = ncol(qmt)

  # Dimension names
  p_names = dimnames(reg_output$beta_sample_array)[[1]]
  grid_names = paste0("p", (1:grid_size) / (grid_size + 1))
  sample_names = paste0("sample ", 1:n_samples)

  fcn_sample_array = array(dim = c(p, grid_size, n_samples),
                           dimnames = list(
                              p_names,
                              grid_names,
                              sample_names
                           ))

  for(i in 1:n_samples){
    fcn_sample_array[,,i] = reg_output$beta_sample_array[,,i] %*% qmt
  }

  return(fcn_sample_array)
}
