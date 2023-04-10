#' @export
ScreePlot = function(data, selection_count_tib, min_count, max_count, metric_1, metric_2){
  coefs_and_recon = map(min_count:max_count, \(c){
    tryCatch({
      GetQuantletCoefficients(data, selection_count_tib %>%
                                filter(selection_counts >= c))
    }, error = function(e){
      return(NULL)
    })
  })

  metric_1_summary = map(coefs_and_recon, \(Q){
    map2_dbl(Q$y_star, data, ~ metric_1(.x, .y))
  })

  metric_2_summary = map_dbl(metric_1_summary, ~ metric_2(.x))

  results_tib = tibble(
    `Selection threshold (>=)` = min_count:max_count,
    `Reconstruction Quality` = metric_2_summary
  )

  ggplot(results_tib) +
    geom_point(aes(x = `Selection threshold (>=)`, y = `Reconstruction Quality`)) +
    geom_path(aes(x = `Selection threshold (>=)`, y = `Reconstruction Quality`)) +
    ggtitle("Quantile Reconstruction Quality by Selection threshold") +
    theme_bw()
}
