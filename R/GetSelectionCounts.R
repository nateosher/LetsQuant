#' @importFrom magrittr `%>%`
#' @importFrom tibble tibble
#' @importFrom tidyr expand_grid
#' @importFrom dplyr bind_rows mutate
#' @export
GetSelectionCounts = function(data_list, a = NULL, b = NULL, progress = TRUE) {
  if(is.null(a))
    a = c(seq(0.1, 1, by = 0.1), seq(2, 100, by = 1))
  if(is.null(b))
    b = c(seq(0.1, 1, by = 0.1), seq(2, 100, by = 1))

  n = length(data_list)
  lasso.list = vector("list", n)
  grid_tibble = tibble(a = c(0, 0),
                       b = c(0, 1),
                       distribution = c("intercept", "normal")) %>%
   bind_rows(expand_grid(a, b) %>%
              mutate(distribution = "beta")
            ) %>%
    mutate(
      selection_counts = 0
    )

  progressr::with_progress({
    progressr::handlers(
      list(
        progressr::handler_progress(
          format = ":spin :current/:total [:bar] :percent in :elapsed ETA: :eta"
        )
      )
    )

    prog = progressr::progressor(n)

    for(i in 1:n) {
      if(progress)
        prog()

      y = data_list[[i]]
      n_i = length(y)
      p_grid = (1:n_i)/(n_i + 1)

      beta_cdf_grid_matrix = GENERATE_BETA_CDF_GRID(a, b, p_grid)

      std_norm_cdf_grid = qnorm(p_grid, 0, 1)
      normalized_std_norm_cdf_grid = (std_norm_cdf_grid - mean(std_norm_cdf_grid)) /
                                      sd(std_norm_cdf_grid)

      full_cdf_grid_matrix = cbind(normalized_std_norm_cdf_grid,
                                    t(beta_cdf_grid_matrix))

      lasso_fit = glmnet::glmnet(full_cdf_grid_matrix, y, intercept = TRUE)
      cvfit.lasso = glmnet::cv.glmnet(full_cdf_grid_matrix, y,
                                       intercept = TRUE, nfolds = 3)

      selected = coef(lasso_fit, s = cvfit.lasso$lambda.1se) %>%
                  (\(v) !(v == 0)) %>%
                  as.vector() %>%
                  which()
      lasso.list[[i]] = selected
    }
  })

  selection_table = lasso.list %>% unlist() %>% table()
  cdf_indices = (selection_table %>% names() %>% as.numeric())
  selection_counts = selection_table %>% unname() %>% as.numeric()

  grid_tibble$selection_counts[cdf_indices] = selection_counts

  return(grid_tibble)
}
