#' @export
PlotDensities = function(sample_array, coef_mat, colors,
                         p_grid_size = 100){
  n_samples = dim(sample_array)[3]
  p_grid_indices = seq(1, dim(sample_array)[2], length.out = p_grid_size) %>%
                    floor() %>% unique()
  n_coefficient_settings = nrow(coef_mat)

  quantile_function_samples = array(dim = c(n_coefficient_settings,
                                  length(p_grid_indices),
                                  n_samples),
                          dimnames = list(
                            dimnames(coef_mat)[[1]], # "names" of settings
                            dimnames(sample_array)[[2]][p_grid_indices], # p grid labels
                            dimnames(sample_array)[[3]] # sample numbers
                          ))

  delta = 1 / p_grid_size

  for(i in 1:dim(sample_array)[3]){
    quantile_function_samples[,,i] = coef_mat %*% sample_array[,p_grid_indices,i]
  }

  quantile_function_means = apply(quantile_function_samples, c(1,2), mean)

  density_samples = array(dim = c(n_coefficient_settings,
                                  p_grid_size - 1,
                                  n_samples),
                          dimnames = list(
                            dimnames(coef_mat)[[1]], # "names" of settings
                            dimnames(sample_array)[[2]][p_grid_indices[2:p_grid_size]], # p grid labels
                            dimnames(sample_array)[[3]] # sample numbers
                          ))

  for(i in 1:dim(quantile_function_samples)[3]){
    density_samples[,,i] = (delta / (apply(quantile_function_samples[,,i], 1, diff) %>%
                                      t())) %>%
                           apply(MARGIN = 1:2, FUN = \(x) max(x, 0))
  }

  density_means = apply(density_samples, c(1,2), mean)

  plot_tib = tibble(
    x = t(quantile_function_means[,2:p_grid_size]) %>% as.numeric(),
    y = t(density_means) %>% as.numeric(),
    setting = rep(rownames(coef_mat), each = p_grid_size - 1) %>%
      factor(levels = rownames(coef_mat))
  )

  ggplot(plot_tib) +
    geom_smooth(aes(x = x, y = y, group = setting, color = setting),
                se = FALSE) +
    scale_color_manual(values = colors) +
    theme_bw()

}
