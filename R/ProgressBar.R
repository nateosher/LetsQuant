#' Prints text progress bar with spinny thing
#' @param cur current index
#' @param total total number of things to be done
#' @return NULL
ProgressBar = function(cur, total,...){
  n_equals = min(floor((cur/total) * (options("width")$width - 10)),
                 options("width")$width - 10)
  n_space = options("width")$width - n_equals - 10
  spinny_thing = rep(c("-", "\\", "|", "/"), each = 1)[(cur %% 4) + 1]
  if(cur > total){
    status = "  Done      \n"
    spinny_thing = "+"
  }else{
    status = paste0("  ", round(cur / total, 2) * 100, "%")
  }

  message(c("\r[",
            rep("=", n_equals),
            rep("-", n_space),
            "]  ", spinny_thing, status), sep = ""
          , appendLF = F)
}
