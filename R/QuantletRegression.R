#' @export
QuantletRegression = function(qcoef_list, X, n_burn, n_sample){
  Q_star = map(qcoef_list, ~ matrix(.x, nrow = 1)) %>%
    do.call(rbind, .)

  progressr::with_progress({
    progressr::handlers(
      list(
        progressr::handler_progress(
          format = ":spin :current/:total (:message) [:bar] :percent in :elapsed ETA: :eta"
        )
      )
    )

    prog = progressr::progressor(steps = ncol(Q_star))

    reg_list = map(1:ncol(Q_star), \(i){
      prog(message = paste0("Quantlet ", i, "..."))
      BayesianLM(Q_star[,i], X, n_burn, n_sample)
    })
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
