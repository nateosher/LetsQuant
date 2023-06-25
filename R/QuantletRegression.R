#' @importFrom magrittr `%>%`
#' @export
QuantletRegression = function(qb, vars_to_use, n_burn, n_sample,
                              progress = TRUE){
  check_quantlet_regression_inputs(qb, vars_to_use)
  formula_string = paste0(vars_to_use, collapse = " + ")
  formula_string = paste0("dummy_outcome ~ ", formula_string, " - 1")
  model_formula = as.formula(formula_string)
  X = qb$metadata %>%
    select(vars_to_use) %>%
    mutate(dummy_outcome = 1) %>%
    model.matrix(model_formula, .)

  quiet_summarize = purrr::quietly(dplyr::summarize)

  missing_data = qb$metadata %>%
    select(any_of(vars_to_use)) %>%
    quiet_summarize(across(.cols = everything(), ~ is.na(.x))) %>%
    (\(x) x$result) %>%
    purrr::reduce(`|`)

  model_output = RunQuantletRegression(qb$current_qcoefs[!missing_data],
                                       X,
                                       n_burn,
                                       n_sample,
                                       progress)

  structure(
    list(
      model_output = model_output,
      variables = dimnames(X)[[2]],
      n_burn = n_burn,
      n_sample = n_sample,
      quantlet_basis = qb$current_basis
    ),
    class = "QuantletRegression"
  )
}

check_quantlet_regression_inputs = function(qb, vars_to_use){
  if(is.null(qb$current_basis)){
    stop("no basis generated - see `update_quantlet_basis` function")
  }

  if(is.null(qb$metadata)){
    stop(paste0("metadata must be added in order to run quantlet regression; ",
                "see `update_metadata` function for details."))
  }

  if(! all( vars_to_use %in% colnames(qb$metadata) ) ){
    stop(paste0("certain variables present in `vars_to_use` not present in ",
                "provided metadata"))
  }
}



RunQuantletRegression = function(qcoef_list, X, n_burn, n_sample, progress = TRUE){
  Q_star = map(qcoef_list, ~ matrix(.x, nrow = 1)) %>%
    do.call(rbind, .)

    reg_list = map(1:ncol(Q_star), \(i){
      if(progress) ProgressBar(i-1, ncol(Q_star))
      BayesianLM(Q_star[,i], X, n_burn, n_sample)
    })
    ProgressBar(ncol(Q_star) + 1, ncol(Q_star))

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

#' @export
print.QuantletRegression = function(qr, ...){
  cat("`QuantletRegressions` object\n")
  cat(qr$n_burn, "burn-in samples,", qr$n_sample, "post burn-in samples\n")
  cat("Variables used:", paste0(qr$variables, collapse = ", "), "\n")
}

#' @importFrom RColorBrewer brewer.pal
#' @export
plot.QuantletRegression = function(qr, ...){
  additional_params = list(...)

  check_quantlet_regression_plot_inputs(qr, additional_params)

  plot_type = additional_params$type

  if(is.null(plot_type) || plot_type == "coef_fcn"){
    fcn_samples = GetCoefFcnSamples(qr$quantlet_basis, qr$model_output)
    lower = ifelse(is.null(additional_params$lower),
                   0.025,
                   additional_params$lower)

    upper = ifelse(is.null(additional_params$upper),
                   0.975,
                   additional_params$upper)
    PlotFcnSamples(fcn_samples,
                   additional_params$coef_name,
                   lower, upper)
  }else if(plot_type == "density"){
    density_colors = RColorBrewer::brewer.pal(nrow(additional_params$coef_mat),
                                              "PuBuGn") %>%
                      suppressWarnings()

    if(!is.null(additional_params$colors)){
      density_colors = additional_params$colors
    }

    fcn_samples = GetCoefFcnSamples(qr$quantlet_basis, qr$model_output)

    PlotDensities(fcn_samples,
                  coef_mat = additional_params$coef_mat,
                  colors = density_colors,
                  span = additional_params$span)
  }
}

check_quantlet_regression_plot_inputs = function(qr, additional_params){
  if(is.null(additional_params$type) ||
     additional_params$type == "coef_fcn"){
    if(is.null(additional_params$coef_name)){
      stop(paste0("please provide name of coefficient to plot via ",
                  "the `coef_name` argument"))
    }
  }else if(additional_params$type == "density"){
    if(is.null(additional_params$coef_mat))
      stop("coefficient matrix must be provided")

    if(nrow(additional_params$coef_mat) > 9 &&
       is.null(additional_params$colors)){
      stop(paste0("cannot generate default color palette with > 9 colors; ",
                  "please provide a vector of colors via the `colors` ",
                  "argument (1 per row of `coef_mat`)"))
    }

    if(!is.null(additional_params$colors) &&
       nrow(additional_params$coef_mat) != length(additional_params$colors)){
      stop(paste0("`colors` vector must be the same length as ",
                  "nrow(coef_mat)"))
    }

  }
}
































