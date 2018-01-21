#' Broadcasting an operation on multi-dimensinoal arrays
#'
#' Broadcasting two arrays to the same shape and then apply FUN.
#'
#' @param arrA,arrB Arrays to perfom the operator FUN(arrA,arrB)
#' @param FUN function to be performed (must be vectorised)
#'
#' @return An array of the shape with each dimension being the maximum of dimA,dimB.
#' @examples
#'
#'   M = array(1:12,dim=c(2,2,3))
#'   N = rbind(c(1,2))
#'   O = bsxfun(M,N,'*')
#' @note
#' #Sun Jan 21 01:27:48 2018 ------------------------------
#'
#' @author Feng Geng (shouldsee.gem@gmail.com)
#' @export
#'
bsxfun <-
  function(arrA,arrB,FUN = '+'){
   arrA <- as.array(arrA)
   arrB <- as.array(arrB)
   dimA <- dim(arrA)
   dimB <- dim(arrB)
   if (length(dimA) >= length(dimB)){
     arrL = list(arrA,arrB)
   }else{
     arrL = list(arrB,arrA)
   }
   dim1 <- dim(arrL[[1]])
   dim2 <- dim(arrL[[2]])
   length(dim1)<-length(dim2)
   nonS = (dim1!=1) & (dim2!=1)

   if( !all(dim1[nonS]== dim2[nonS])){
     errmsg = sprintf("All non-singleton axis must be equal: Array 1 [%s] , Array 2 [%s]",
                      paste(dimA,collapse = ','),
                      paste(dimB,collapse = ','))
     stop(errmsg)
   }
   MARG = which(nonS)
   sweep(arrL[[1]],arrL[[2]],MARGIN = MARG,FUN = FUN)
}
#
if (interactive()){

  M = array(1:12,dim=c(2,2,3))
  N = rbind(c(1,2))
  O = bsxfun(M,N,'*')

  M = array(1:12,dim=c(2,2,3))
  N = rbind(c(1,2))
  O = bsxfun(N,M,'*')

  ## Not run
  # try({
  #   M = array(1:12,dim=c(2,2,3))
  #   N = rbind(c(1,2,3))
  #   O = bsxfun(M,N,'*')
  # })
  ##
}


#
#
# {
#   library(microbenchmark)
#   a = c(1,0,3,-4)
#   n_col = 4
#
#   # make example more realistic by expanding number of rows of b
#   nsample = 1e3
#   b = pracma::repmat(matrix(c(0,2,1,9,5,-3,7,4),nrow = 2,ncol = n_col), nsample / 2, 1)
#
#   print(microbenchmark(
#     erg_1 = sweep(b, 2, a, '*'),
#     erg_2 = bsxfun(b,  rbind(a), '*'),
#     erg_3 = pracma::bsxfun('*', pracma::repmat(a, nsample, 1), b)
#   ))
#   # Unit: microseconds
#   # expr      min        lq      mean    median        uq       max neval cld
#   # erg_1   88.803   96.9395  113.8098  106.7960  125.7245   204.942   100  a
#   # erg_2  108.456  123.8730  148.4552  142.8465  161.7485   250.351   100  a
#   # erg_3 4431.176 4525.1725 4715.9614 4587.9385 4676.1885 12920.696   100  b
# }
