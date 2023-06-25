#' @export
ccc = function(x, y){
  sigma_x = sd(x)
  sigma_y = sd(y)
  if(sigma_x == 0 || sigma_y == 0)
    return(0)

  rho = cor(x, y)

  mu_x = mean(x)
  mu_y = mean(y)

  return(
    (2 * rho * sigma_x * sigma_y) / (sigma_x^2 + sigma_y^2 + (mu_x - mu_y)^2)
  )
}
