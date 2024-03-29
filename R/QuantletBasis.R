#' @importFrom purrr map_chr map_lgl
#' @importFrom dplyr select
#' @importFrom magrittr `%>%` set_colnames
#' @importFrom tibble as_tibble
#' @export
new_QuantletBasis = function(raw_data,
                             id_list = NULL,
                             metadata = NULL,
                             a = c(seq(0.1, 1, by = 0.2), seq(2, 100, by = 1)),
                             b = c(seq(0.1, 1, by = 0.2), seq(2, 100, by = 1)),
                             progress = TRUE){

  check_qb_constructor_input(raw_data, id_list, metadata, a, b)

  message("Constructing quantlet basis object...")
  selection_counts = GetSelectionCounts(raw_data, a, b, progress = progress)

  structure(
    list(
      raw_data = raw_data,
      selection_counts = selection_counts %>%
        filter(selection_counts > 0),
      ids = id_list,
      metadata = metadata,
      current_basis = NULL,
      current_qcoefs = NULL,
      current_cutoff = NULL
    ),
    class = "QuantletBasis"
  )
}

check_qb_constructor_input = function(raw_data, id_list, metadata, a, b){
  # id_list checks
  if(!is.null(id_list) && length(id_list) != length(raw_data)){
    stop("`id_list` must be same length of `raw_data` if both are passed")
  }
  # Raw data checks
  if(length(raw_data) < 1){
    stop("raw data must have at least one entry")
  }

  if(class(raw_data) != "list")
    stop("raw data must be passed as list")

  not_all_numeric = raw_data %>%
    map_chr(~ class(.x)) %>%
    (\(classes){
      any(!(classes %in% c("numeric", "integer")))
    }) %>%
    any()

  if(not_all_numeric)
    stop("all `raw_data` list entries must be numeric/integer vectors")

  all_sorted = raw_data %>%
    map(\(l){
      if(not_all_numeric) return(l$data)
      else return(l)
    }) %>%
    map_lgl(~ is.unsorted(.x)) %>%
    any() %>%
    (\(x) !x)

  if(!all_sorted)
    stop("all numeric vector entries must be sorted low -> high")

  # Metadata
  metadata_exists = !is.null(metadata)

  if(metadata_exists){
    check_metadata(id_list, metadata)
  }

  # a and b
  if(class(a) != "numeric" || class(b) != "numeric")
    stop("`a` and `b` parameters must both be numeric vectors")

  if(any(a <= 0 | b <= 0))
    stop("all entries of `a` and `b` must be strictly greater than 0")
}

check_metadata = function(id_list, metadata){
  # if metadata is provided, ids should be too
  if(is.null(id_list)){
    stop(paste0("in order to use metadata, ids must be passed ",
                "via the `id_list` argument"))
  }
  # metadata should be either tibble or data frame
  if(!("data.frame" %in% class(metadata)))
    stop("metadata should be passed as a data.frame or tibble")

  # check for ids in metadata
  if(! ("id" %in% colnames(metadata))){
    stop(paste0("metadata must include `id` column"))
  }

  # If there are no common ids, throw error
  if(! any(id_list %in% metadata$id)){
    stop("metadata and raw data have no common ids")
  }

  # If there are some ids not in the data, throw warning
  if( any(!( id_list %in% metadata$id )) ){
    warning(paste0("not all observations have corresponding entries ",
                   "in metadata; these observations will not be ",
                   "used in analyses."))
  }

  any_missing_metadata = (metadata %>% is.na() %>% sum()) > 0
  if(any_missing_metadata){
    warning(paste0("missing data are currently unsupported- observations ",
                   "with missingness may be removed from analyses."))
  }
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
  if(is.null(qb$metadata)){
    cat("No attached metadata\n")
  }else{
    cat("Attached metadata with", ncol(qb$metadata), "columns\n")
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
  message("Computing new quantlet coefficients...")
  qb$current_qcoefs = GetQuantletCoefficients(qb$raw_data, qb$current_basis,
                                              progress = TRUE)
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

#' @export
update_metadata = function(qb, new_metadata){
  check_metadata_update(qb, new_metadata)
  qb$metadata = new_metadata
  return(qb)
}

check_metadata_update = function(qb, metadata){
  # metadata should be either tibble or data frame
  if(!("data.frame" %in% class(metadata)))
    stop("metadata should be passed as a data.frame or tibble")

  # check for ids in raw data
  if(is.null(qb$ids)){
    stop(paste0("data must have ids to use metadata; see `update_ids` ",
                "function"))
  }

  # check for ids in metadata
  if(! ("id" %in% colnames(metadata))){
    stop(paste0("metadata must include `id` column"))
  }

  # check for congruence between ids
  raw_data_ids = qb$ids

  # If there are no common ids, throw error
  if(! any(raw_data_ids %in% metadata$id)){
    stop("metadata and raw data have no common ids")
  }

  # If there are some ids not in the data, throw warning
  if( any(!( raw_data_ids %in% metadata$id )) ){
    warning(paste0("not all observations have corresponding entries ",
                   "in metadata; these observations will not be ",
                   "used in analyses."))
  }

  any_missing_metadata = (metadata %>% is.na() %>% sum()) > 0
  if(any_missing_metadata){
    warning(paste0("missing data are currently unsupported- observations ",
                   "with missingness may be removed from analyses."))
  }
}

#' @export
update_ids = function(qb, new_ids){
  check_new_ids(qb, new_ids)
  qb$ids = new_ids
  return(qb)
}

check_new_ids = function(qb, new_ids){
  if(class(new_ids) != "character"){
    stop(paste0("`new_ids` must be of class `character`"))
  }
  if(length(new_ids) != length(qb$raw_data)){
    stop(paste0("vector of new ids must be the same length as ",
                "data"))
  }

  if(!is.null(qb$metadata)){
    # If there are no common ids, throw error
    if(! any(new_ids %in% qb$metadata$id)){
      stop("metadata and raw data have no common ids")
    }

    # If there are some ids not in the data, throw warning
    if( any(!( new_ids %in% qb$metadata$id )) ){
      warning(paste0("not all observations have corresponding entries ",
                     "in metadata; these observations will not be ",
                     "used in analyses."))
    }
  }
}


























