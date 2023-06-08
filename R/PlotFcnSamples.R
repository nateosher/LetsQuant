#' @import ggplot2
#' @importFrom tibble tibble
#' @export
PlotFcnSamples = function(sample_array, coef_name, lower = 0.025, upper = 0.975){
  coef_index = which(dimnames(sample_array)[[1]] == coef_name)

  # Rows are samples; columns are grid points
  coef_function_samples = sample_array[coef_index,,] %>% t()

  coef_function_ci = coef_function_samples %>% apply(MARGIN = 2, FUN = \(col){
    quantile(col, c(lower, upper))
  })

  coef_function_mean = coef_function_samples %>% apply(MARGIN = 2, FUN = mean)

  plot_tib = tibble(
    p = (1:ncol(coef_function_samples)) / (ncol(coef_function_samples) + 1),
    mean = coef_function_mean,
    lower = coef_function_ci[1,] %>% as.numeric(),
    upper = coef_function_ci[2,] %>% as.numeric(),
    signif = map2_dbl(lower, upper, ~ .x > 0 || .y < 0) %>%
                        as.numeric(),
    signif_y = min(lower) - 0.1 * abs(min(lower))
  )

  all_non_signif = all(plot_tib$signif == 0)

  ggplot(plot_tib) +
    geom_ribbon(aes(x = p, ymin = lower, ymax = upper),
                alpha = 0.6, fill = "grey70") +
    geom_path(aes(x = p, y = mean), color = "black") +
    geom_point(aes(x = p, y = signif_y, alpha = signif), color = "red") +
    {
      if(all_non_signif)
        scale_alpha_continuous(guide = "none", range = c(0, 0.000001))
      else
        scale_alpha_continuous(guide = "none", range = c(0, 2))
    } +
    ggtitle(paste0(coef_name, " Functional Coefficient")) +
    theme_bw()
}
