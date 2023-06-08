#' @import ggplot2
#' @importFrom purrr map map2_dbl map_dbl
#' @importFrom tibble tibble
#' @importFrom tidyr drop_na
#' @export
ScreePlot = function(data, selection_count_tib, min_count, max_count,
                     metric_1, metric_2, grid_size = NULL, ...){

  additional_params = list(...)

  if(is.null(grid_size)){
    grid_size = data %>% map_dbl(~length(.x)) %>% max()
  }

  # This is a list, where each entry is a list of the projected data using
  # the specified threshold for quantlet selection
  projected_data = map(min_count:max_count, \(c){
    tryCatch({
      quantlet_matrix = SelectQuantlets(selection_count_tib, c,
                                      grid_size = grid_size)

      Q_star_list = GetQuantletCoefficients(data, quantlet_matrix)

      map(Q_star_list, \(Q_star){
        quantlet_matrix %*% Q_star
      })
    }, error = function(e){
      return(NULL)
    })
  })

  min_viable = map_dbl(projected_data, ~ length(.x)) %>%
                (\(projection_lengths){
                  min_count + min(which(projection_lengths > 0)) - 1
                })

  max_viable = map_dbl(projected_data, ~ length(.x)) %>%
    (\(projection_lengths){
      min_count + max(which(projection_lengths > 0)) - 1
    })



  message(paste0("Successfully generated quantlet coefficients ",
                "for selection threshold range ",
                min_viable, "-", max_viable))

  data_common_grid = SameLengthData(data, grid_size)

  # First, compute metric_1 on the "reconstructed"/"projected" data with actual
  # data
  metric_1_summary = map(projected_data, \(y_star_list){
    if(is.null(y_star_list)) return(NA)
    map2_dbl(y_star_list, data_common_grid, \(y_star, y){
      metric_1(y_star, y)
    })
  })

  # Next, compute metric_2 to summarize results from metric_1
  metric_2_summary = map_dbl(metric_1_summary, ~ metric_2(.x))

  results_tib = tibble(
    `Selection threshold (>=)` = min_count:max_count,
    `Reconstruction Quality` = metric_2_summary
  ) %>%
    drop_na()

  metric_1_name = substitute(metric_1)
  if(!is.null(additional_params$metric_1_name)){
    metric_1_name = additional_params$metric_1_name
  }

  metric_2_name = substitute(metric_2)
  if(!is.null(additional_params$metric_2_name)){
    metric_2_name = additional_params$metric_2_name
  }

  ggplot(results_tib) +
    geom_point(aes(x = `Selection threshold (>=)`, y = `Reconstruction Quality`)) +
    geom_path(aes(x = `Selection threshold (>=)`, y = `Reconstruction Quality`)) +
    ggtitle("Quantile Reconstruction Quality by Selection threshold",
            subtitle = paste0("Metric 1: ", metric_1_name, ", ",
                              "Metric 2: ", metric_2_name)) +
    theme_bw()
}
