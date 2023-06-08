set.seed(11223344)
ex_data = purrr::map(1:30, \(i){
  n_obs = sample(100:150, 1)
  raw_data = rgamma(n_obs, runif(1, 2, 10), runif(1, 2, 10)) %>%
    sort()
  id = sample(LETTERS, 6, replace = TRUE) %>% paste(collapse = "")

  return(list(
    data = raw_data,
    id = id
  ))
})

test_quantlet_basis = new_QuantletBasis(ex_data, progress = FALSE)
