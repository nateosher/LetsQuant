BayesianLM = function(y, X, n_burn, n_sample, quantlet_num = NULL){
  n = nrow(X)
  p = ncol(X)

  beta_mean = solve(t(X) %*% X) %*% t(X) %*% y

  beta_cov_kernel = solve(t(X) %*% X)

  beta_samples = matrix(nrow = ncol(X), ncol = n_sample)

  sigma_2_inv_samples = numeric(n_sample)

  cur_beta = beta_mean
  cur_sigma_2_inv = sum((y - X %*% cur_beta)^2) / (n - p - 1)
  cur_cov = (1/cur_sigma_2_inv) * beta_cov_kernel

  for(i in 1:(n_burn + n_sample)){
    cur_beta = MASS::mvrnorm(n = 1, mu = beta_mean, Sigma = cur_cov)
    cur_sigma_2_inv = rgamma(n = 1,
                             shape = (n - p) / 2,
                             rate = 0.5 * sum((y - X %*% cur_beta)^2))

    cur_cov = (1/cur_sigma_2_inv) * beta_cov_kernel

    if(i > n_burn){
      beta_samples[,i - n_burn] = cur_beta
      sigma_2_inv_samples[i - n_burn] = cur_sigma_2_inv
    }
  }



  return(list(
    beta_samples = beta_samples,
    sigma_2_samples = 1/sigma_2_inv_samples
  ))
}
