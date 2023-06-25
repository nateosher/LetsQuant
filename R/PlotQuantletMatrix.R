#' @import ggplot2
#' @importFrom tibble tibble
#' @importFrom purrr map
PlotQuantletMatrix = function(M) {
  n_quantlets = ncol(M)
  quantlet_tibble = tibble(
    vals = as.numeric(M),
    p = rep(1:nrow(M) / (nrow(M) + 1), n_quantlets),
    labels = rep(paste("Q", 1:n_quantlets), each = nrow(M))
  )

  if(abs(floor(sqrt(n_quantlets)) - sqrt(n_quantlets)) < 1e-15){
    n_rows = sqrt(n_quantlets)
  }else{
    n_rows = ceiling(sqrt(n_quantlets))
  }

  plot_list = map(paste("Q", 1:ncol(M)), \(Q){
    ggplot(quantlet_tibble %>% filter(labels == Q)) +
      geom_path(aes(x = p, y = vals)) +
      ylab("") + xlab("") +
      ggtitle(Q) +
      theme_bw()
  })

  do.call(gridExtra::grid.arrange, c(plot_list, nrow = n_rows))
}
