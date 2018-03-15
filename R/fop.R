##### Quoted from http://adv-r.had.co.nz/Function-operators.html
#' @export
function(f, ...) {
  l <- list(...)
  function(...) {
    do.call(f, c(l, list(...)))
  }
}

#' @export
combine_args <- function (f) {
  force(f)
  function(args) {
    do.call(f, args)
  }
}

#' @export
compose <- function(f, g,right = FALSE) {
  if(right){
    function(...) f(g(...))
  }else{
    function(...) g(f(...))
  }
}


#' @source Adapted from pryr
#' @export
compose <- function (...,right = FALSE)
{
  fs <- lapply(list(...), match.fun)
  n <- length(fs)
  if (right){
    ### Reduce from left to right
    fs = rev(fs)
  }
  first <- fs[[1]]
  rest <- fs[-1]
  function(...) {
    out <- first(...)
    for (f in rest) {
      out <- f(out)
    }
    out
  }
}

# Avoid conflict with igraph
#' @export
Fcompose <- compose

#' @export
rbind_list <- combine_args(rbind)


