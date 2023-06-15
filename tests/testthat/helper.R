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

test_metadata = tibble(
  id = map_chr(ex_data, ~ .x$id),
  group = sample(c(1, 2), length(ex_data), replace = TRUE) %>%
    factor(),
  other_var = rnorm(30)
)

test_quantlet_basis = new_QuantletBasis(ex_data,
                                        metadata = test_metadata,
                                        progress = TRUE)

test_quantlet_basis = update_quantlet_basis(test_quantlet_basis, 7)

test_qr = QuantletRegression(test_quantlet_basis,
                             c("group", "other_var"),
                             2000, 4000)
