#' @importFrom purrr map_chr map_lgl
#' @importFrom dplyr select
#' @importFrom magrittr `%>%` set_colnames
#' @importFrom tibble as_tibble
#' @export
new_QuantletBasis = function(raw_data_list,
                             a = c(seq(0.1, 1, by = 0.2), seq(2, 100, by = 1)),
                             b = c(seq(0.1, 1, by = 0.2), seq(2, 100, by = 1)),
                             progress = TRUE){

  check_qb_constructor_input(raw_data_list, a, b)

  id_list = NULL
  raw_data = raw_data_list
  if(class(raw_data[[1]]) == "list"){
    id_list = map_chr(raw_data, ~ .x$id)
    raw_data = map(raw_data_list, ~ .x$data)
  }

  message("Constructing quantlet basis object...")
  selection_counts = GetSelectionCounts(raw_data, a, b, progress = progress)

  structure(
    list(
      raw_data = raw_data,
      selection_counts = selection_counts %>%
        filter(selection_counts > 0),
      ids = id_list,
      current_basis = NULL,
      current_cutoff = NULL
    ),
    class = "QuantletBasis"
  )
}

check_qb_constructor_input = function(raw_data, a, b){
  # Raw data checks
  if(length(raw_data) < 1){
    stop("raw data must have at least one entry")
  }

  if(class(raw_data) != "list")
    stop("raw data must be passed as list")

  not_all_numeric = raw_data %>%
    map_chr(~ class(.x)) %>%
    (\(classes) classes != "numeric") %>%
    any()

  not_all_have_ids = raw_data %>%
    map_lgl(~ (class(.x) != "list") || is.null(.x$id) ) %>%
    any()

  not_all_have_data = raw_data %>%
    map_lgl(~ class(.x) != "list" ||
              is.null(.x$data) ) %>%
    any()

  improper_entry_structure = not_all_have_ids || not_all_have_data

  if(not_all_numeric && improper_entry_structure){
    stop(paste0("all `raw_data` list entries must be numeric vectors OR ",
                "lists with `data` (numeric vector) attribute and `id` ",
                "(string) attribute"))
  }

  all_sorted = raw_data %>%
    map(\(l){
      if(not_all_numeric) return(l$data)
      else return(l)
    }) %>%
    map_lgl(~ is.unsorted(.x)) %>%
    (\(unsorted_bools) !unsorted_bools) %>%
    any()

  if(!all_sorted)
    stop("all numeric vector entries must be sorted low -> high")

  # a and b
  if(class(a) != "numeric" || class(b) != "numeric")
    stop("`a` and `b` parameters must both be numeric vectors")

  if(any(a <= 0 | b <= 0))
    stop("all entries of `a` and `b` must be strictly greater than 0")
}

#' @export
print.QuantletBasis = function(qb, ...){
  cat("`QuantletBasis` with", length(qb$raw_data), "observations\n")
  if(is.null(qb$current_basis)){
    cat("No basis selected\n")
  }else{
    cat("Basis selected using cutoff =", qb$current_cutoff, "\n")
    cat(ncol(qb$current_basis), "quantlets selected\n")
    cat("Grid size:", nrow(qb$current_basis), "\n")
  }
}

#' @export
summary.QuantletBasis = function(qb, ...){
  cat("`QuantletBasis` with", length(qb$raw_data), "observations\n")
  if(is.null(qb$current_basis)){
    cat("No basis selected\n")
  }else{
    cat("Basis selected using cutoff =", qb$current_cutoff, "\n")
    cat(ncol(qb$current_basis), "quantlets selected\n")
    cat("Grid size:", nrow(qb$current_basis), "\n")
  }
}

#' @export
plot.QuantletBasis = function(qb, ...){
  additional_params = list(...)
  original_call = sys.call()
  if(length(additional_params) == 0)
    plot_qb_standard(qb)
  else if(additional_params$type == "scree"){
    plot_qb_scree(qb, additional_params$min_count,
                  additional_params$max_count,
                  additional_params$metric_1,
                  additional_params$metric_2,
                  additional_params$grid_size,
                  metric_1_name = original_call$metric_1,
                  metric_2_name = original_call$metric_2)
  }
}

plot_qb_standard = function(qb){

  check_standard_plot_params(qb)

  PlotQuantletMatrix(qb$current_basis)

}

check_standard_plot_params = function(qb){
  if(is.null(qb$current_basis))
    stop("no basis generated - see `update_quantlet_basis` function")
}

plot_qb_scree = function(qb, min_count, max_count,
                         metric_1, metric_2, grid_size, ...){

  additional_params = list(...)

  check_scree_plot_params(qb, min_count, max_count, metric_1,
                          metric_2, grid_size)

  metric_1_name = additional_params$metric_1_name
  metric_2_name = additional_params$metric_2_name

  ScreePlot(qb$raw_data, qb$selection_counts, min_count, max_count,
            metric_1, metric_2, grid_size,
            metric_1_name = metric_1_name,
            metric_2_name = metric_2_name)
}

check_scree_plot_params = function(qb, min_count, max_count, metric_1,
                                   metric_2, grid_size){
  any_missing = is.null(min_count) ||
                is.null(max_count) ||
                is.null(metric_1) ||
                is.null(metric_2)
  if(any_missing){
    stop(paste0("in order to produce scree plot, the following arguments ",
                "must be passed: min_count, max_count, metric_1, and metric_2"))
  }

  if(min_count >= max_count){
    stop(paste0("`min_count` parameter must be strictly",
         " less than `max_count` parameter"))
  }

  if(class(metric_1) != "function" || class(metric_2) != "function"){
    stop(paste0("`metric_1` and `metric_2` must be functions"))
  }
}

#' @export
update_quantlet_basis = function(qb, cutoff, grid_size = NULL,
                                 orthogonalize = TRUE, force_intercept = TRUE){
  max_n_obs = map_dbl(qb$raw_data, ~ length(.x)) %>%
    max()
  basis = SelectQuantlets(qb$selection_counts, cutoff,
                          grid_size = max_n_obs)
  qb$current_basis = basis
  qb$current_cutoff = cutoff
  return(qb)
}

#' @export
get_quantlet_basis = function(qb){
  if(is.null(qb$current_basis))
    stop("no basis generated - see `update_quantlet_basis` function")

  return(qb$current_basis)
}

#' @export
get_quantlet_coefficients = function(qb, normalize = FALSE){
  if(is.null(qb$current_basis))
    stop("no basis generated - see `update_quantlet_basis` function")

  qcoef_list = GetQuantletCoefficients(qb$raw_data, qb$current_basis)

  map(qcoef_list, ~ matrix(.x, nrow = 1)) %>%
    do.call(rbind, .) %>%
    set_colnames(paste0("q", 1:length(qcoef_list[[1]]))) %>%
    as_tibble() %>%
    {
      if(normalize){
        mutate(., across(.cols = everything(), ~ (.x - mean(.x)) / sd(.x)))
      }else{
        .
      }
    } %>%
    mutate(
      id = qb$id
    ) %>%
    select(id, everything())
}






























