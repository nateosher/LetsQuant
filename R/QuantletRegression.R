#' @export
QuantletRegression = function(qcoef_list, X, n_burn, n_sample, progress = TRUE){
  Q_star = map(qcoef_list, ~ matrix(.x, nrow = 1)) %>%
    do.call(rbind, .)

    reg_list = map(1:ncol(Q_star), \(i){
      if(progress) ProgressBar(i-1, ncol(Q_star))
      BayesianLM(Q_star[,i], X, n_burn, n_sample)
    })

  beta_samples = array(dim = c(ncol(X), ncol(Q_star),n_sample),
                       dimnames = list(colnames(X),
                                       paste0("Quantlet ", 1:ncol(Q_star)),
                                       1:n_sample
                                    )
                       )

  for(i in 1:length(reg_list)){
    beta_samples[,i,] = reg_list[[i]]$beta_samples
  }

  sigma_2_samples = map(reg_list, ~ .x$sigma_2_samples) %>%
    do.call(rbind, .)

  return(list(
    beta_sample_array = beta_samples,
    sigma_2_samples = sigma_2_samples
  ))
}
