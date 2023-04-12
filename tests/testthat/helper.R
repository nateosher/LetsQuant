set.seed(11223344)
ex_data = purrr::map(1:30, \(i){
  n_obs = sample(100:150, 1)
  rgamma(n_obs, runif(1, 2, 10), runif(1, 2, 10)) %>%
    sort()
})

selection_count_tib = GetSelectionCounts(ex_data,
                                         a = c(seq(0.1, 1, by = 0.2), seq(2, 100, by = 1)),
                                         b = c(seq(0.1, 1, by = 0.2), seq(2, 100, by = 1)),
                                         progress = FALSE)
