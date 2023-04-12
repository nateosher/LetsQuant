#' @export
SameLengthData = function(data, common_length){
  # The common quantile grid has to be at least as long as the maximum number
  # of observations per patient
  if(common_length < (map_dbl(data, ~ length(.x)) %>% max())){
    stop(paste("`common_length` must be at least as large as maximum #",
               "observations across data set"))
  }


  p_grid = (1:common_length) / (common_length + 1)
  data_list_common_grid = map(data, \(d){
    quantile(d, p_grid)
  })
  return(data_list_common_grid)
}
