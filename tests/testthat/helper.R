set.seed(11223344)

n = 30

test_metadata = tibble(
  id = map_chr(1:n, ~ sample(LETTERS, 6, replace = TRUE) %>% paste(collapse = "")),
  group = sample(c(1, 2), n, replace = TRUE) %>%
    factor(),
  other_var = rnorm(n)
)

ex_data = purrr::map(1:n, \(i){
  n_obs = sample(100:150, 1)
  raw_data = {
    raw_data = rgamma(n_obs, runif(1, 2, 10), runif(1, 2, 10))
    raw_data +
      1 * (test_metadata$group[i] == 2) +
      1 * test_metadata$other_var[i]
  } %>%
    sort()

  return(raw_data)
})


test_quantlet_basis = new_QuantletBasis(ex_data,
                                        progress = TRUE)

# test_qr = QuantletRegression(test_quantlet_basis,
#                              c("group", "other_var"),
#                              2000, 4000)
